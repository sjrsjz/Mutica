use std::sync::{Arc, Weak};

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
        collector::CollectorExt, cycle_detector::FastCycleDetector, rootstack::RootStack, source_info::SourceLocation, three_valued_logic::ThreeValuedLogic
    },
};

use crate::types::CoinductiveTypeRef;

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
    source_info: Option<Arc<SourceLocation>>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for FixPoint<T> {
    fn clone(&self) -> Self {
        Self { reference: self.reference.clone(), source_info: self.source_info.clone() }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for FixPoint<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<GCArcWeak<T>>) {
        queue.push_back(self.reference.clone());
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for FixPoint<T> {
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
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
    ) -> Result<Option<R>, TypeError<<T as GcAllocObject<T>>::Inner, T>>
    where
        F: FnOnce(
            &mut FastCycleDetector<TaggedPtr<()>>,
            <T::Inner as AsDispatcher<T::Inner, T>>::RefDispatcher<'_>,
        ) -> R,
    {
        self.reference
            .upgrade()
            .ok_or(TypeError::UnresolvableType("Reference is dead".into()))
            .map(|inner: GCArc<T>| inner.as_ref().map_value(path, f))
    }

    pub fn take<F, R>(
        self,
        path: &mut FastCycleDetector<TaggedPtr<()>>,
        f: F,
    ) -> Result<Option<R>, TypeError<<T as GcAllocObject<T>>::Inner, T>>
    where
        F: FnOnce(&mut FastCycleDetector<TaggedPtr<()>>, T::Inner) -> R,
    {
        self.reference
            .upgrade()
            .ok_or(TypeError::UnresolvableType("Reference is dead".into()))
            .map(|inner: GCArc<T>| inner.as_ref().take_value(path, f))
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
    pub fn new_placeholder(gc: &mut GC<T>, roots: &mut RootStack<Type<T>, T>) -> Type<T> {
        Self::new_placeholder_with_info(gc, roots, None)
    }

    pub fn new_placeholder_with_info(
        gc: &mut GC<T>,
        roots: &mut RootStack<Type<T>, T>,
        source_info: Option<Arc<SourceLocation>>,
    ) -> Type<T> {
        let pointer = gc.create(T::new_placeholder());
        let fix_point = FixPoint { reference: pointer.as_weak(), source_info };
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
            inner.as_ref().set_value(t).map(|_| ())
        } else {
            Err(TypeError::UnresolvableType("Reference is dead".into()))
        }
    }

    pub fn reference(&self) -> &GCArcWeak<T> {
        &self.reference
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
    fn check(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        ctx.pattern_collector.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.collected,
            );
            match other {
                // 这里不能放 Generalize 等，不然会导致 fixpoint <: Max<> 这种形式，但是由于是通过accept调用的，会导致 Max 比 fixpoint 先拆开
                // 为了透明化 fixpoint 的存在，我们必须先展开 fixpoint
                TypeRef::FixPoint(v) => {
                    let l: Weak<_> = self.reference.clone().into();
                    let r: Weak<_> = v.reference.clone().into();
                    if l.ptr_eq(&r) {
                        // 相同引用，协归纳假设成立
                        return Ok(ThreeValuedLogic::True);
                    }
                    v.accept(self.as_ref_dispatcher(), &mut inner_ctx)
                }
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                _ => match self.reference.upgrade() {
                    Some(inner) => {
                        let inner = match inner.as_ref().get_value() {
                            Some(t) => t,
                            None => return Ok(ThreeValuedLogic::Unknown), // 未初始化
                        };
                        let self_ptr = inner.tagged_ptr();
                        let other_ptr = other.tagged_ptr();
                        let assumption_pair = (self_ptr, other_ptr);
                        // 在 inner_ctx 的 assumptions 中检查，而不是 ctx.assumptions
                        let already_assumed =
                            inner_ctx.assumptions.iter().any(|a| a == &assumption_pair);
                        if already_assumed {
                            return Ok(ThreeValuedLogic::True); // already assumed
                        }

                        inner_ctx.assumptions.push(assumption_pair.clone());
                        let result = inner.check(other, &mut inner_ctx);
                        inner_ctx.assumptions.pop();
                        result
                    }
                    None => Err(TypeError::UnresolvableType("Reference is dead".into())),
                },
            }
        })
    }

    fn subof(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        ctx.pattern_collector.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.collected,
            );
            match other {
                TypeRef::FixPoint(v) => {
                    let l: Weak<_> = self.reference.clone().into();
                    let r: Weak<_> = v.reference.clone().into();
                    if l.ptr_eq(&r) {
                        // 相同引用，协归纳假设成立
                        return Ok(ThreeValuedLogic::True);
                    }
                    v.superof(self.as_ref_dispatcher(), &mut inner_ctx)
                }
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                _ => match self.reference.upgrade() {
                    Some(inner) => {
                        let inner = match inner.as_ref().get_value() {
                            Some(t) => t,
                            None => return Ok(ThreeValuedLogic::Unknown), // 未初始化（实际上这个可能需要更精细的处理）
                        };
                        let self_ptr = inner.tagged_ptr();
                        let other_ptr = other.tagged_ptr();
                        let assumption_pair = (self_ptr, other_ptr);
                        // 在 inner_ctx 的 assumptions 中检查，而不是 ctx.assumptions
                        let already_assumed =
                            inner_ctx.assumptions.iter().any(|a| a == &assumption_pair);
                        if already_assumed {
                            return Ok(ThreeValuedLogic::True); // already assumed
                        }

                        inner_ctx.assumptions.push(assumption_pair.clone());
                        let result = inner.subof(other, &mut inner_ctx);
                        inner_ctx.assumptions.pop();
                        result
                    }
                    None => Err(TypeError::UnresolvableType("Reference is dead".into())),
                },
            }
        })
    }

    fn reduce(
        self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        match self.reference.upgrade() {
            Some(inner) => {
                let inner_type = match inner.as_ref().get_value() {
                    Some(t) => t,
                    None => return Ok(self.dispatch()), // 未初始化
                };
                for r in ctx.rec_assumptions.iter_mut().rev() {
                    if r.0 == inner_type.tagged_ptr() {
                        //已经假设递归的归约结果,直接返回
                        r.2 = true; // mark as used
                        return Ok(r.1.clone());
                    }
                }
                let temp_fixpoint = Self::new_placeholder(ctx.gc, ctx.roots);
                // 假设递归类型的归约结果为 temp_fixpoint
                ctx.rec_assumptions.push((inner_type.tagged_ptr(), temp_fixpoint.clone(), false));
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
            None => Err(TypeError::UnresolvableType("Reference is dead".into())),
        }
    }

    fn invoke(self, ctx: InvokeContext<Type<T>, T>) -> Result<Type<T>, TypeError<Type<T>, T>> {
        match self.reference.upgrade() {
            Some(inner) => inner
                .as_ref()
                .get_value()
                .ok_or(TypeError::UnresolvableType("Reference is dead".into()))
                .and_then(|t| t.clone().invoke(ctx)),
            None => Err(TypeError::UnresolvableType("Reference is dead".into())),
        }
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        self.source_info.as_ref()
    }

    fn report_source_info(&self) -> crate::types::TypeReport {
        if let Some(loc) = &self.source_info {
            let span = loc.span().clone();
            let filepath = loc.source().filepath().to_string();
            ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                .with_message(format!("Fixpoint type at {}", filepath))
                .with_label(
                    ariadne::Label::new((filepath, span)).with_message("Fixpoint defined here"),
                )
                .finish()
        } else {
            ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                .with_message("Fixpoint type has no source location")
                .with_label(
                    ariadne::Label::new(("<unknown>".to_string(), 0..0))
                        .with_message("Location unknown"),
                )
                .finish()
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveTypeWithAny<Type<T>, T> for FixPoint<T> {
    #[stacksafe::stacksafe]
    fn accept(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        ctx.pattern_collector.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.collected,
            );
            match self.reference.upgrade() {
                Some(inner) => other.check(
                    match inner.as_ref().get_value() {
                        Some(t) => t.as_ref_dispatcher(),
                        None => return Ok(ThreeValuedLogic::Unknown), // 未初始化
                    },
                    &mut inner_ctx,
                ),
                None => Err(TypeError::UnresolvableType("Reference is dead".into())),
            }
        })
    }

    #[stacksafe::stacksafe]
    fn superof(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        ctx.pattern_collector.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.collected,
            );
            match self.reference.upgrade() {
                Some(inner) => other.subof(
                    match inner.as_ref().get_value() {
                        Some(t) => t.as_ref_dispatcher(),
                        None => return Ok(ThreeValuedLogic::Unknown), // 未初始化
                    },
                    &mut inner_ctx,
                ),
                None => Err(TypeError::UnresolvableType("Reference is dead".into())),
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
    fn represent(
        &self,
        path: &mut FastCycleDetector<TaggedPtr<()>>,
        depth: usize,
        max_depth: usize,
    ) -> String {
        if depth > max_depth {
            return "...".to_string();
        }
        match self.reference.upgrade() {
            Some(inner) => match inner.as_ref().get_value() {
                Some(t) => match path
                    .with_guard(t.tagged_ptr(), |path| t.represent(path, depth, max_depth))
                {
                    Some(s) => format!("μ.{:?} {}", t as *const _ as *const (), s),
                    None => format!("{:?}", t as *const _ as *const ()),
                },
                None => "!UninitializedFixPoint".to_string(), // 未初始化
            },
            None => "!InvalidFixPoint".to_string(), // reference is dead
        }
    }
}
