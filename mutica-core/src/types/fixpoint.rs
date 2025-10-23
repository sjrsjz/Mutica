use std::sync::{Arc, RwLock};

use arc_gc::{
    arc::{GCArc, GCArcWeak},
    gc::GC,
    traceable::GCTraceable,
};

use crate::{
    as_type,
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, TaggedPtr, Type, TypeCheckContext, TypeError,
        TypeRef,
    },
    util::{
        cycle_detector::FastCycleDetector, rootstack::RootStack,
        three_valued_logic::ThreeValuedLogic,
    },
};

/// # 不动点算子内部实现 (Fixed-Point Operator Inner Implementation)
///
/// `FixPointInner` 是不动点类型的内部存储结构，使用 `OnceLock`
/// 实现**延迟初始化**，支持递归类型的前向引用。
///
/// ## 设计原理
///
/// 在类型理论中，不动点算子 **μX.T(X)** 表示满足方程 **X = T(X)** 的类型。
/// 例如：
/// - 自然数：`Nat = μX. () | X`  
/// - 列表：`List<A> = μX. () | (A, X)`
/// - 树：`Tree<A> = μX. A | (X, X)`
///
/// ## 延迟初始化机制
///
/// 由于递归类型的定义需要引用自身，我们必须：
/// 1. **先创建占位符**：分配类型引用但不指定内容
/// 2. **后填充定义**：通过 `set` 方法设置具体的递归结构

pub struct FixPoint<T: GcAllocObject<T, Inner = Type<T>>> {
    reference: GCArcWeak<T>,
    is_nf: Arc<RwLock<ThreeValuedLogic>>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for FixPoint<T> {
    fn clone(&self) -> Self {
        Self {
            reference: self.reference.clone(),
            is_nf: self.is_nf.clone(),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GcAllocObject<T> for FixPoint<T> {
    type Inner = Type<T>;
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for FixPoint<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<GCArcWeak<T>>) {
        queue.push_back(self.reference.clone());
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for FixPoint<T> {
    fn upgrade<'roots>(&self, collected: &'roots mut Vec<GCArc<T>>) {
        if let Some(inner) = self.reference.upgrade() {
            collected.push(inner);
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> FixPoint<T> {
    pub fn map<F, R>(
        &self,
        path: &mut FastCycleDetector<TaggedPtr<()>>,
        f: F,
    ) -> Result<R, TypeError<<T as GcAllocObject<T>>::Inner, T>>
    where
        F: FnOnce(
            &mut FastCycleDetector<TaggedPtr<()>>,
            <T::Inner as AsDispatcher<T::Inner, T>>::RefDispatcher<'_>,
        ) -> R,
    {
        self.reference
            .upgrade()
            .ok_or(TypeError::UnresolvableType)
            .and_then(|inner: GCArc<T>| inner.as_ref().map_inner(path, f))
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> FixPoint<T> {
    /// 创建递归类型占位符
    ///
    /// ## 返回值
    ///
    /// 返回一个 [`Type`]，包含：
    /// - 未初始化的 `FixPoint`
    /// - 对应的强引用以保证 GC 安全

    pub fn new_placeholder<'roots>(
        gc: &mut GC<T>,
        roots: &'roots mut RootStack<Type<T>, T>,
    ) -> Type<T> {
        let pointer = gc.create(T::new_placeholder());
        let fix_point = FixPoint {
            reference: pointer.as_weak(),
            is_nf: Arc::new(RwLock::new(ThreeValuedLogic::Unknown)),
        };
        roots.push(pointer);
        Type::FixPoint(fix_point)
    }

    /// 设置递归类型的具体定义
    ///
    /// ## 参数
    /// - `t`: 递归类型的展开形式，可以引用自身
    ///
    /// ## 错误
    /// - `RedeclaredType`: 类型已经被设置过
    /// - `UnresolvableType`: 不动点引用已失效
    pub fn set<V: AsDispatcher<Type<T>, T>>(&self, t: V) -> Result<(), TypeError<Type<T>, T>> {
        if let Some(inner) = self.reference.upgrade() {
            let t = t.into_dispatcher();
            let is_nf = t.is_normal_form();
            inner.as_ref().set_inner(t)?;
            // 先预设置归约状态
            match self.is_nf.write() {
                Ok(mut nf_lock) => {
                    *nf_lock = is_nf;
                }
                Err(_) => {
                    return Err(TypeError::UnresolvableType);
                }
            }
            // 重新计算所有相关类型的归约状态
            self.recalculate_normal_form(&mut FastCycleDetector::new());
            Ok(())
        } else {
            Err(TypeError::UnresolvableType) // reference is dead
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for FixPoint<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;
    fn into_dispatcher(self) -> Type<T> {
        Type::FixPoint(self)
    }

    fn as_ref_dispatcher<'a>(&'a self) -> Self::RefDispatcher<'a> {
        TypeRef::FixPoint(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for FixPoint<T> {
    /// 递归类型的子类型检查
    ///
    /// ## 算法原理
    ///
    /// 递归类型的子类型检查必须处理**无限展开**的问题。
    /// 我们使用**假设集合** (assumption set) 来记录"正在检查的关系"，
    /// 当遇到重复的检查时，假设关系成立（**协归纳假设**）。
    ///
    /// ## 协归纳子类型规则
    ///
    /// ```text
    /// Γ, μX.S <: μY.T ⊢ S[μX.S/X] <: T[μY.T/Y]
    /// ────────────────────────────────────────────
    ///              μX.S <: μY.T
    /// ```
    ///
    /// 即：在假设 μX.S <: μY.T 的前提下，检查展开后的类型关系。
    fn fulfill(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<Option<()>, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env);
            match self.reference.upgrade() {
                Some(inner) => {
                    let inner = inner
                        .as_ref()
                        .get_inner()
                        .ok_or(TypeError::UnresolvableType)?;
                    let self_ptr = inner.tagged_ptr();
                    let other_ptr = other.tagged_ptr();
                    let assumption_pair = (self_ptr, other_ptr);

                    // 在 inner_ctx 的 assumptions 中检查，而不是 ctx.assumptions
                    let already_assumed =
                        inner_ctx.assumptions.iter().any(|a| a == &assumption_pair);
                    if already_assumed {
                        return Ok(Some(())); // already assumed
                    }

                    inner_ctx.assumptions.push(assumption_pair);
                    let result = inner.fulfill(other, &mut inner_ctx);
                    inner_ctx.assumptions.pop();
                    result
                }
                None => Err(TypeError::UnresolvableType), // reference is dead
            }
        })
    }

    fn reduce(
        self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        match self.reference.upgrade() {
            Some(inner) => {
                let inner_type = inner
                    .as_ref()
                    .get_inner()
                    .ok_or(TypeError::UnresolvableType)?;
                for r in ctx.rec_assumptions.iter_mut().rev() {
                    if r.0 == inner_type.tagged_ptr() {
                        //已经假设递归的归约结果,直接返回
                        r.2 = true; // mark as used
                        return Ok(r.1.clone());
                    }
                }
                let temp_fixpoint = Self::new_placeholder(ctx.gc, ctx.roots);
                // 假设递归类型的归约结果为 temp_fixpoint
                ctx.rec_assumptions
                    .push((inner_type.tagged_ptr(), temp_fixpoint.clone(), false));
                let result = (*inner_type).clone().reduce(ctx);
                let (_, _, used) = ctx.rec_assumptions.pop().unwrap();
                if used {
                    // 递归类型在展开中被使用,返回新的递归类型
                    as_type!(&temp_fixpoint, Type::FixPoint).set(result?)?;
                    Ok(temp_fixpoint)
                } else {
                    // 递归类型未被使用,直接返回展开结果
                    result
                }
            }
            None => Err(TypeError::UnresolvableType), // reference is dead
        }
    }

    fn invoke(
        &self,
        ctx: &mut InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        match self.reference.upgrade() {
            Some(inner) => inner
                .as_ref()
                .get_inner()
                .ok_or(TypeError::UnresolvableType)
                .and_then(|t| t.invoke(ctx)),
            None => Err(TypeError::UnresolvableType), // reference is dead
        }
    }

    fn is_normal_form(&self) -> ThreeValuedLogic {
        match self.reference.upgrade() {
            Some(_) => self
                .is_nf
                .read()
                .map_or(ThreeValuedLogic::False, |v| v.clone()),
            None => ThreeValuedLogic::False, // reference is dead
        }
    }

    fn recalculate_normal_form(&self, cycle_detector: &mut FastCycleDetector<TaggedPtr<()>>) {
        let is_nf = match self.reference.upgrade() {
            Some(inner) => {
                match inner.as_ref().get_inner() {
                    Some(t) => {
                        match cycle_detector.with_guard(t.tagged_ptr(), |cycle_detector| {
                            match self.is_nf.write() {
                                Ok(mut nf_lock) => {
                                    let is_nf: ThreeValuedLogic = nf_lock.clone();
                                    if let ThreeValuedLogic::Unknown = is_nf {
                                        // 先假设为真，后续会迭代到收敛
                                        *nf_lock = ThreeValuedLogic::True
                                    }
                                }
                                Err(_) => {}
                            }
                            let mut prev_nf = self.is_normal_form();
                            loop {
                                t.recalculate_normal_form(cycle_detector); // 迭代
                                let is_nf = self.is_normal_form();
                                if is_nf == prev_nf {
                                    // 如果收敛了就停止
                                    break is_nf;
                                }
                                prev_nf = is_nf;
                            }
                        }) {
                            Some(v) => v,
                            None => self.is_normal_form(),
                        }
                    }
                    None => ThreeValuedLogic::Unknown, // 未初始化
                }
            }
            None => ThreeValuedLogic::False, // reference is dead
        };
        if let Ok(mut nf_lock) = self.is_nf.write() {
            *nf_lock = is_nf;
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveTypeWithAny<Type<T>, T> for FixPoint<T> {
    #[stacksafe::stacksafe]
    fn accept(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<Option<()>, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env);
            match self.reference.upgrade() {
                Some(inner) => other.fullfill(
                    inner
                        .as_ref()
                        .get_inner()
                        .ok_or(TypeError::UnresolvableType)?
                        .as_ref_dispatcher(),
                    &mut inner_ctx,
                ),
                None => Err(TypeError::UnresolvableType), // reference is dead
            }
        })
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for FixPoint<T> {
    /// 递归类型的字符串表示
    ///
    /// 使用数学记号 `μ.地址 内容` 表示不动点类型，其中：
    /// - `μ` 表示不动点算子
    /// - `地址` 是类型对象的内存地址（用于区分不同的递归类型）
    /// - `内容` 是类型的展开形式（如果没有循环）
    ///
    /// 对于循环引用，只显示地址以避免无限递归打印。
    fn represent(&self, path: &mut FastCycleDetector<TaggedPtr<()>>) -> String {
        match self.reference.upgrade() {
            Some(inner) => match inner.as_ref().get_inner() {
                Some(t) => match path.with_guard(t.tagged_ptr(), |path| t.represent(path)) {
                    Some(s) => format!("μ.{:?} {}", t as *const _ as *const (), s),
                    None => format!("{:?}", t as *const _ as *const ()),
                },
                None => "!UninitializedFixPoint".to_string(), // 未初始化
            },
            None => "!InvalidFixPoint".to_string(), // reference is dead
        }
    }
}
