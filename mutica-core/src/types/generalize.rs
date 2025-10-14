use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};
use smallvec::smallvec;

use crate::{
    types::{
        AsType, CoinductiveType, CoinductiveTypeWithAny, InvokeContext, ReductionContext,
        Representable, Rootable, Type, TypeCheckContext, TypeError, closure::ClosureEnv,
        fixpoint::FixPointInner, type_bound::TypeBound,
    },
    util::{collector::Collector, cycle_detector::FastCycleDetector},
};

/// # 类型泛化运算符 (Type Generalization Operator)
///
/// ## 理论定义
///
/// `Generalize` 实现了类型系统中的**泛化类型表示**，表示为 `Max<T₁, T₂, ..., Tₙ>`。
///
/// **重要**：这**不是**数学意义的最小上界(lub)运算，也**不是**集合论的并集运算！
///
/// ### 核心语义
///
/// `Max<T₁, T₂, ..., Tₙ>` 表示一个**不可约类型集合** `{T₁, T₂, ..., Tₙ}`，满足：
///
/// - **互不包含性**：∀i,j. i≠j ⟹ Tᵢ ⊄ Tⱼ ∧ Tⱼ ⊄ Tᵢ
/// - **极大性**：无法通过子类型关系进一步约简（保留最泛化的类型）
///
/// ### 子类型语义（定义性质）
///
/// - **协变性质**：`S <: Max<T₁, ..., Tₙ>` **定义为** `∃i. S <: Tᵢ`
/// - **逆变性质**：`Max<T₁, ..., Tₙ> <: U` **定义为** `∀i. Tᵢ <: U`
///
/// 这些是**定义**，不是从其他性质推导出来的！
///
/// ### 重要说明：不是逻辑并集！
///
/// **错误理解**：
/// - ❌ `Max<String, Integer> = ⊤` (认为是全并集)
/// - ❌ `Max` 是格论中的上确界运算
/// - ❌ 可以从集合论并集类比理解
///
/// **正确理解**：
/// - ✅ `Max<String, Integer> = {String, Integer}` (不可约类型集合)
/// - ✅ `String` 和 `Integer` 之间没有子类型关系，所以集合无法简化
/// - ✅ 这是类型系统中特有的表示方法
///
/// ### 非分配性质 (Non-Distributive Property)
///
/// **重要**：类型泛化运算**不满足分配律**，因为它不是代数运算！
///
/// ```text
/// 一般情况下：Min<A, Max<B, C>> ≠ Max<Min<A,B>, Min<A,C>>
/// ```
///
/// 这不是因为"子类型格不是布尔代数"，而是因为 `Min` 和 `Max` 根本就不是代数运算符。
///
/// ### 实现细节
///
/// 1. **扁平化**：自动展开嵌套的 `Generalize` 类型
/// 2. **吸收律**：移除被其他类型包含的冗余类型（与 Min 相反）
/// 3. **简化**：单个类型时直接返回该类型，空集时返回 `⊥`
#[derive(Clone)]
pub struct Generalize {
    types: Arc<[Type]>,
    is_nf: bool,
}

impl GCTraceable<FixPointInner> for Generalize {
    fn collect(
        &self,
        queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<FixPointInner>>,
    ) {
        for sub in self.types.iter() {
            sub.collect(queue);
        }
    }
}

impl Rootable for Generalize {
    fn upgrade(&self, collected: &mut Vec<GCArc<FixPointInner>>) {
        for sub in self.types.iter() {
            sub.upgrade(collected);
        }
    }
}

impl CoinductiveType<Type> for Generalize {
    fn dispatch(self) -> Type {
        Type::Generalize(self)
    }

    fn is(&self, other: &Type, ctx: &mut TypeCheckContext) -> Result<Option<()>, super::TypeError> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.assumptions,
                ctx.closure_env,
                pattern_env,
                ctx.pattern_mode,
            );
            match other {
                Type::Bound(TypeBound::Top) => Ok(Some(())), // 快速路径
                Type::Specialize(v) => v.has(self, &mut inner_ctx),
                Type::FixPoint(v) => v.has(self, &mut inner_ctx),
                Type::Pattern(v) => v.has(self, &mut inner_ctx),
                Type::Variable(v) => v.has(self, &mut inner_ctx),
                _ => {
                    for sub in self.types.iter() {
                        if !sub.is(other, &mut inner_ctx)?.is_some() {
                            return Ok(None);
                        }
                    }
                    Ok(Some(()))
                }
            }
        })
    }

    /// 类型应用：分布到集合中每个类型
    ///
    /// `Max<T₁, ..., Tₙ>[V] = Max<T₁[V], ..., Tₙ[V]>`
    ///
    /// 类型应用操作分布到不可约集合中的每个类型上。
    fn reduce(self, ctx: &mut ReductionContext) -> Result<Type, TypeError> {
        let mut result = smallvec::SmallVec::<[Type; 8]>::new();
        for sub in self.types.into_iter() {
            result.push(sub.clone().reduce(ctx)?);
        }
        Self::new(&result, ctx.closure_env)
    }

    fn invoke(&self, ctx: &mut InvokeContext) -> Result<Type, TypeError> {
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

impl CoinductiveTypeWithAny<Type> for Generalize {
    fn has<V: CoinductiveType<Type> + Clone>(
        &self,
        other: &V,
        ctx: &mut TypeCheckContext,
    ) -> Result<Option<()>, super::TypeError> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, false);
            for sub in self.types.iter() {
                // 我们传入 false 是因为generalize是乱序的,它不适用于模式匹配,因为模式匹配的解构是有序的
                if other.is(sub, &mut inner_ctx)?.is_some() {
                    return Ok(Some(())); // 由于不需要匹配子模式,短路返回不会影响正确性
                }
            }
            Ok(None)
        })
    }
}

impl Representable for Generalize {
    fn represent(&self, path: &mut FastCycleDetector<*const ()>) -> String {
        let mut result = String::new();
        result.push_str("Max<");
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

impl Generalize {
    /// 构造不可约类型集合，应用吸收律和简化规则（与Specialize对偶）
    ///
    /// ## 构造算法
    ///
    /// 1. **扁平化**：递归展开嵌套的 `Max<...>` 类型
    /// 2. **吸收律**：移除冗余类型 - 如果 `Tᵢ <: Tⱼ`，则移除 `Tᵢ`（与Min相反）
    /// 3. **简化**：
    ///    - 空集合 → `⊥` (Bottom)  
    ///    - 单元素 → 该元素本身
    ///    - 多元素 → `Max<...>`
    ///
    /// ### 理论依据
    ///
    /// 吸收律确保结果集合的**不可约性**（泛化方向）：
    /// ```text
    /// 若 A <: B，则 Max<A, B> ≡ Max<B>
    /// ```
    ///
    /// 因为如果 A <: B，那么任何既是 A 又是 B 的超类型的类型，
    /// 只需要是 B 的超类型即可，所以 A 在集合中是多余的。
    ///
    /// 最终结果保证：集合中任意两个类型都不存在子类型关系，
    /// 且保留的都是最泛化的类型。
    pub fn new<I, T>(types: I, closure_env: &ClosureEnv) -> Result<Type, TypeError>
    where
        I: IntoIterator<Item = T>,
        T: AsType,
    {
        fn collect(
            collected: &mut Vec<Type>,
            path: &mut FastCycleDetector<*const ()>,
            t: &Type,
        ) -> Result<(), TypeError> {
            t.map(path, |path, t| -> Result<(), TypeError> {
                match t {
                    Type::Generalize(generalize) => {
                        for sub in generalize.types.iter() {
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

        // 检测是否所有类型都是NF形式
        // 如果他们全部是NF形式,那么结果也是NF形式
        let mut all_nf = true;

        types
            .into_iter()
            .map(|t| {
                all_nf &= t.as_type_ref().is_normal_form();
                collect(
                    &mut collected,
                    &mut FastCycleDetector::new(),
                    t.as_type_ref(),
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
                    // 如果该类型是其他类型的子类型,则该类型被吸收
                    let mut assumptions_temp = smallvec![];
                    let mut pattern_env_temp = Collector::new();
                    let mut check_ctx = TypeCheckContext::new(
                        &mut assumptions_temp,
                        (closure_env, closure_env),
                        &mut pattern_env_temp,
                        false,
                    );
                    if collected[i].is(&collected[j], &mut check_ctx)?.is_some() {
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
            0 => TypeBound::Bottom.dispatch(),
            1 => result.into_iter().next().unwrap(),
            _ => Generalize {
                types: Arc::from(result),
                is_nf: true,
            }
            .dispatch(),
        };
        Ok(new_type)
    }

    /// 构造只有被reduce后才会正确化简的Generalize类型，不进行吸收律和简化
    pub fn new_raw<I, T>(types: I) -> Type
    where
        I: IntoIterator<Item = T>,
        T: AsType,
    {
        let collected: Vec<_> = types.into_iter().map(|t| t.into_type()).collect();
        Self {
            types: Arc::from(collected),
            is_nf: false,
        }
        .dispatch()
    }

    pub fn types(&self) -> &[Type] {
        &self.types
    }
}
