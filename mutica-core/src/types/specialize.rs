use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};
use smallvec::smallvec;

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, InvokeContext, ReductionContext,
        Representable, Rootable, Type, TypeCheckContext, TypeError, TypeRef, closure::ClosureEnv,
        fixpoint::FixPointInner, type_bound::TypeBound,
    },
    util::{collector::Collector, cycle_detector::FastCycleDetector},
};

/// # 类型特化运算符 (Type Specialization Operator)
///
/// ## 理论定义
///
/// `Specialize` 实现了类型系统中的**特化类型表示**，表示为 `Min<T₁, T₂, ..., Tₙ>`。
///
/// **重要**：这**不是**数学意义的最大下界(glb)运算，也**不是**集合论的交集运算！
///
/// ### 核心语义
///
/// `Min<T₁, T₂, ..., Tₙ>` 表示一个**不可约类型集合** `{T₁, T₂, ..., Tₙ}`，满足：
///
/// - **互不包含性**：∀i,j. i≠j ⟹ Tᵢ ⊄ Tⱼ ∧ Tⱼ ⊄ Tᵢ
/// - **极小性**：无法通过子类型关系进一步约简
///
/// ### 子类型语义（定义性质）
///
/// - **协变性质**：`S <: Min<T₁, ..., Tₙ>` **定义为** `∀i. S <: Tᵢ`
/// - **逆变性质**：`Min<T₁, ..., Tₙ> <: U` **定义为** `∃i. Tᵢ <: U`
///
/// 这些是**定义**，不是从其他性质推导出来的！
///
/// ### 重要说明：不是逻辑交集！
///
/// **错误理解**：
/// - ❌ `Min<String, Integer> = ⊥` (认为是空交集)
/// - ❌ `Min` 是格论中的下确界运算
/// - ❌ 可以从集合论交集类比理解
///
/// **正确理解**：
/// - ✅ `Min<String, Integer> = {String, Integer}` (不可约类型集合)
/// - ✅ `String` 和 `Integer` 之间没有子类型关系，所以集合无法简化
/// - ✅ 这是类型系统中特有的表示方法
///
/// ### 非分配性质 (Non-Distributive Property)
///
/// **重要**：类型特化运算**不满足分配律**，因为它不是代数运算！
///
/// ```text
/// 一般情况下：Max<A, Min<B, C>> ≠ Min<Max<A,B>, Max<A,C>>
/// ```
///
/// 这不是因为"子类型格不是布尔代数"，而是因为 `Min` 和 `Max` 根本就不是代数运算符。
///
/// ### 实现细节
///
/// 1. **扁平化**：自动展开嵌套的 `Specialize` 类型
/// 2. **吸收律**：移除被其他类型subsume的冗余类型
/// 3. **简化**：单个类型时直接返回该类型，空集时返回 `⊤`
#[derive(Clone)]
pub struct Specialize {
    types: Arc<[Type]>,
    is_nf: bool,
}

impl GCTraceable<FixPointInner> for Specialize {
    fn collect(
        &self,
        queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<FixPointInner>>,
    ) {
        for sub in self.types.iter() {
            sub.collect(queue);
        }
    }
}

impl Rootable<FixPointInner>for Specialize {
    fn upgrade(&self, collected: &mut Vec<GCArc<FixPointInner>>) {
        for sub in self.types.iter() {
            sub.upgrade(collected);
        }
    }
}

impl CoinductiveType<Type> for Specialize {
    type RefDispatcher<'a>
        = TypeRef<'a>
    where
        Self: 'a;
    fn dispatch(self) -> Type {
        Type::Specialize(self)
    }

    fn dispatch_ref<'a>(&'a self) -> Self::RefDispatcher<'a> {
        TypeRef::Specialize(self)
    }

    fn is(&self, other: &Type, ctx: &mut TypeCheckContext) -> Result<Option<()>, TypeError<Type>> {
        ctx.pattern_env.collect(|pattern_env| {
            let enabled = pattern_env.is_enabled();
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env);
            match other {
                Type::Bound(TypeBound::Top) => Ok(Some(())), // 快速路径
                Type::Specialize(v) => v.has(self, &mut inner_ctx),
                Type::FixPoint(v) => v.has(self, &mut inner_ctx),
                Type::Pattern(v) => v.has(self, &mut inner_ctx),
                Type::Variable(v) => v.has(self, &mut inner_ctx),

                _ if enabled => {
                    // 当 pattern_mode 不为 false 时,表示需要匹配子模式
                    // 这时不能短路返回,因为可能需要多个子类型共同匹配一个模式
                    let mut matched = false;
                    for sub in self.types.iter() {
                        // 实际上fallback可能会导致pattern_env被意外修改,我们需要一个可回滚的机制
                        matched |= sub.is(other, &mut inner_ctx)?.is_some()
                    }
                    Ok(if matched { Some(()) } else { None })
                }

                _ => {
                    // 当 pattern_mode 为 false 时,表示不需要匹配子模式,短路返回不会影响正确性
                    for sub in self.types.iter() {
                        if sub.is(other, &mut inner_ctx)?.is_some() {
                            return Ok(Some(()));
                        }
                    }
                    Ok(None)
                }
            }
        })
    }

    /// 类型应用：分布到集合中每个类型
    ///
    /// `Min<T₁, ..., Tₙ>[V] = Min<T₁[V], ..., Tₙ[V]>`
    ///
    /// 类型应用操作分布到不可约集合中的每个类型上。
    fn reduce(self, ctx: &mut ReductionContext) -> Result<Type, TypeError<Type>> {
        let mut result = smallvec::SmallVec::<[Type; 8]>::new();
        for sub in self.types.into_iter() {
            result.push(sub.clone().reduce(ctx)?);
        }
        Self::new(&result, ctx.closure_env)
    }

    fn invoke(&self, ctx: &mut InvokeContext) -> Result<Type, TypeError<Type>> {
        let mut result = smallvec::SmallVec::<[Type; 8]>::new();
        for sub in self.types.iter() {
            result.push(sub.invoke(ctx)?);
        }
        Self::new(&result, ctx.closure_env)
    }

    fn is_normal_form(&self) -> bool {
        self.is_nf
    }
}

impl CoinductiveTypeWithAny<Type> for Specialize {
    fn has<V: CoinductiveType<Type> + Clone>(
        &self,
        other: &V,
        ctx: &mut TypeCheckContext,
    ) -> Result<Option<()>, super::TypeError<Type>> {
        ctx.pattern_env.collect(|_| {
            let mut new_pattern_env = Collector::new_disabled();
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, &mut new_pattern_env);
            for sub in self.types.iter() {
                // 我们传入 disabled 是因为specialize是乱序的,它不适用于模式匹配,因为模式匹配的解构是有序的
                if other.is(sub, &mut inner_ctx)?.is_none() {
                    return Ok(None);
                }
            }
            Ok(Some(()))
        })
    }
}

impl Representable for Specialize {
    fn represent(&self, path: &mut FastCycleDetector<*const ()>) -> String {
        let mut result = String::new();
        result.push_str("Min<");
        for (i, sub) in self.types.iter().enumerate() {
            if i > 0 {
                result.push_str(", ");
            }
            result.push_str(&sub.represent(path));
        }
        result.push_str(">");
        result
    }
}

impl Specialize {
    /// 构造不可约类型集合，应用吸收律和简化规则
    ///
    /// ## 构造算法
    ///
    /// 1. **扁平化**：递归展开嵌套的 `Min<...>` 类型
    /// 2. **吸收律**：移除冗余类型 - 如果 `Tᵢ <: Tⱼ`，则移除 `Tⱼ`  
    /// 3. **简化**：
    ///    - 空集合 → `⊤` (Top)
    ///    - 单元素 → 该元素本身
    ///    - 多元素 → `Min<...>`
    ///
    /// ### 理论依据
    ///
    /// 吸收律确保结果集合的**不可约性**：
    /// ```text
    /// 若 A <: B，则 Min<A, B> ≡ Min<A>
    /// ```
    ///
    /// 因为如果 A <: B，那么任何 A 的子类型自动也是 B 的子类型，
    /// 所以 B 在集合中是多余的。
    ///
    /// 最终结果保证：集合中任意两个类型都不存在子类型关系。
    pub fn new<I, T>(types: I, closure_env: &ClosureEnv) -> Result<Type, TypeError<Type>>
    where
        I: IntoIterator<Item = T>,
        T: AsDispatcher,
    {
        fn collect(
            collected: &mut Vec<Type>,
            path: &mut FastCycleDetector<*const ()>,
            t: &Type,
        ) -> Result<(), TypeError<Type>> {
            t.map(path, |path, t| -> Result<(), TypeError<Type>> {
                match t {
                    Type::Specialize(specialize) => {
                        for sub in specialize.types.iter() {
                            collect(collected, path, sub)?;
                        }
                    }
                    _ => {
                        collected.push(t.clone());
                    }
                }
                Ok(())
            })?
        }
        let mut collected = Vec::new();

        let mut all_nf = true;

        types
            .into_iter()
            .map(|t| {
                all_nf &= t.as_ref_dispatcher().is_normal_form();
                collect(
                    &mut collected,
                    &mut FastCycleDetector::new(),
                    t.as_ref_dispatcher(),
                )
            })
            .collect::<Result<Vec<_>, _>>()?;
        let mut absorbed: smallvec::SmallVec<[bool; 8]> = smallvec![false; collected.len()];

        for i in 0..collected.len() {
            if absorbed[i] {
                continue;
            }
            for j in 0..collected.len() {
                if i != j && !absorbed[j] {
                    // 如果有其他类型是该类型的子类型,则该类型被吸收
                    let mut assumptions = smallvec![];
                    let mut pattern_env = Collector::new_disabled();
                    let mut check_ctx = TypeCheckContext::new(
                        &mut assumptions,
                        (closure_env, closure_env),
                        &mut pattern_env,
                    );
                    if collected[j].is(&collected[i], &mut check_ctx)?.is_some() {
                        absorbed[i] = true;
                        break;
                    }
                }
            }
        }

        let mut result = Vec::new();
        for (i, t) in collected.into_iter().enumerate() {
            if !absorbed[i] {
                result.push(t);
            }
        }

        let new_type = match result.len() {
            0 => TypeBound::Top.dispatch(),
            1 => result.into_iter().next().unwrap(),
            _ => Specialize {
                is_nf: all_nf,
                types: Arc::from(result),
            }
            .dispatch(),
        };
        Ok(new_type)
    }

    /// 直接构造，不进行任何简化
    pub fn new_raw<I, T>(types: I) -> Type
    where
        I: IntoIterator<Item = T>,
        T: AsDispatcher,
    {
        let collected: Vec<_> = types.into_iter().map(|t| t.into_dispatcher()).collect();
        Self {
            is_nf: false,
            types: Arc::from(collected),
        }
        .dispatch()
    }

    pub fn types(&self) -> &[Type] {
        &self.types
    }
}
