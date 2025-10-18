use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};
use smallvec::smallvec;

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, Type, TypeCheckContext, TypeError, TypeRef,
        closure::ClosureEnv, type_bound::TypeBound,
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
pub struct Specialize<T: GcAllocObject<T, Inner = Type<T>>> {
    types: Arc<[Type<T>]>,
    is_nf: bool,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Specialize<T> {
    fn clone(&self) -> Self {
        Self {
            types: self.types.clone(),
            is_nf: self.is_nf,
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Specialize<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        for sub in self.types.iter() {
            sub.collect(queue);
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for Specialize<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;

    fn into_dispatcher(self) -> Type<T> {
        Type::Specialize(self)
    }

    fn as_ref_dispatcher<'a>(&'a self) -> Self::RefDispatcher<'a> {
        TypeRef::Specialize(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GcAllocObject<T> for Specialize<T> {
    type Inner = Type<T>;
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Specialize<T> {
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        for sub in self.types.iter() {
            sub.upgrade(collected);
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for Specialize<T> {
    fn fulfill(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<Option<()>, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let enabled = pattern_env.is_enabled();
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env);
            match other {
                TypeRef::Generalize(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Neg(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Rot(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                
                _ if enabled => {
                    // 当 pattern_mode 不为 false 时,表示需要匹配子模式
                    // 这时不能短路返回,因为可能需要多个子类型共同匹配一个模式
                    let mut matched = false;
                    for sub in self.types.iter() {
                        // 实际上fallback可能会导致pattern_env被意外修改,我们需要一个可回滚的机制
                        matched |= sub.fulfill(other.clone(), &mut inner_ctx)?.is_some()
                    }
                    Ok(if matched { Some(()) } else { None })
                }

                _ => {
                    // 当 pattern_mode 为 false 时,表示不需要匹配子模式,短路返回不会影响正确性
                    for sub in self.types.iter() {
                        if sub.fulfill(other.clone(), &mut inner_ctx)?.is_some() {
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
    fn reduce(
        self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        let mut result = smallvec::SmallVec::<[Type<T>; 8]>::new();
        for sub in self.types.into_iter() {
            result.push(sub.clone().reduce(ctx)?);
        }
        Self::new(&result, ctx.closure_env)
    }

    fn invoke(
        &self,
        ctx: &mut InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        let mut result = smallvec::SmallVec::<[Type<T>; 8]>::new();
        for sub in self.types.iter() {
            result.push(sub.invoke(ctx)?);
        }
        Self::new(&result, ctx.closure_env)
    }

    fn is_normal_form(&self) -> bool {
        self.is_nf
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveTypeWithAny<Type<T>, T> for Specialize<T> {
    fn accept(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<Option<()>, super::TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|_| {
            let mut new_pattern_env = Collector::new_disabled();
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, &mut new_pattern_env);
            for sub in self.types.iter() {
                // 我们传入 disabled 是因为specialize是乱序的,它不适用于模式匹配,因为模式匹配的解构是有序的
                if other.is(sub.as_ref_dispatcher(), &mut inner_ctx)?.is_none() {
                    return Ok(None);
                }
            }
            Ok(Some(()))
        })
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Specialize<T> {
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

impl<T: GcAllocObject<T, Inner = Type<T>>> Specialize<T> {
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
    pub fn new<I, X>(
        types: I,
        closure_env: &ClosureEnv<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>>
    where
        I: IntoIterator<Item = X>,
        X: AsDispatcher<Type<T>, T>,
    {
        fn collect<T: GcAllocObject<T, Inner = Type<T>>>(
            collected: &mut Vec<Type<T>>,
            path: &mut FastCycleDetector<*const ()>,
            t: Type<T>,
        ) -> Result<(), TypeError<Type<T>, T>> {
            let result =
                t.map_inner(path, |path, t| -> Result<bool, TypeError<Type<T>, T>> {
                    Ok(match t {
                        TypeRef::Specialize(specialize) => {
                            for sub in specialize.types.iter() {
                                collect(collected, path, sub.clone())?;
                            }
                            false
                        }
                        _ => true,
                    })
                })??;
            if result {
                collected.push(t);
            }
            Ok(())
        }
        let mut collected = Vec::new();

        let mut all_nf = true;

        types
            .into_iter()
            .map(|t| {
                let x = t.into_dispatcher();
                all_nf &= x.is_normal_form();
                collect(&mut collected, &mut FastCycleDetector::new(), x)
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
                    if collected[j]
                        .fulfill(collected[i].as_ref_dispatcher(), &mut check_ctx)?
                        .is_some()
                    {
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
    pub fn new_raw<I, X>(types: I) -> Type<T>
    where
        I: IntoIterator<Item = X>,
        X: AsDispatcher<Type<T>, T>,
    {
        let collected: Vec<_> = types.into_iter().map(|t| t.into_dispatcher()).collect();
        Self {
            is_nf: false,
            types: Arc::from(collected),
        }
        .dispatch()
    }

    pub fn types(&self) -> &[Type<T>] {
        &self.types
    }
}
