use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::{
    test_true,
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, CollectorExt, GcAllocObject,
        InvokeContext, ReductionContext, Representable, Rootable, TaggedPtr, Type,
        TypeCheckContext, TypeError, TypeOfContext, TypeRef, subof::SubOf,
    },
    util::{
        cycle_detector::FastCycleDetector, source_info::SourceLocation,
        three_valued_logic::ThreeValuedLogic,
    },
};

pub enum InvokeCountinuationStyle<U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    TailCall,
    WithContinuation(U),   // 指定普通续体
    WithPerformHandler(U), // 指定Perform续体Handler
    WithBoth(U, U),        // 指定Perform续体和普通续体
    Pandom(V),
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Clone for InvokeCountinuationStyle<U, V> {
    fn clone(&self) -> Self {
        match self {
            InvokeCountinuationStyle::TailCall => InvokeCountinuationStyle::TailCall,
            InvokeCountinuationStyle::WithContinuation(cont) => {
                InvokeCountinuationStyle::WithContinuation(cont.clone())
            }
            InvokeCountinuationStyle::WithPerformHandler(cont) => {
                InvokeCountinuationStyle::WithPerformHandler(cont.clone())
            }
            InvokeCountinuationStyle::WithBoth(cont1, cont2) => {
                InvokeCountinuationStyle::WithBoth(cont1.clone(), cont2.clone())
            }
            InvokeCountinuationStyle::Pandom(_) => {
                unreachable!("InvokeCountinuationStyle::Pandom should never be cloned")
            }
        }
    }
}

pub struct Invoke<U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    // 0: function
    // 1: argument
    // 2: continuation
    // 3: source_info
    #[allow(clippy::type_complexity)]
    inner: Arc<(U, U, InvokeCountinuationStyle<U, V>)>,
    rootless: bool,
    source_info: Option<Arc<SourceLocation>>,
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Clone for Invoke<U, V> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            source_info: self.source_info.clone(),
            rootless: self.rootless,
        }
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> GCTraceable<V> for Invoke<U, V> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<V>>) {
        if self.rootless {
            return;
        }
        let (func, arg, cont_style) = self.inner.as_ref();
        func.collect(queue);
        arg.collect(queue);
        match cont_style {
            InvokeCountinuationStyle::TailCall => {}
            InvokeCountinuationStyle::WithContinuation(cont)
            | InvokeCountinuationStyle::WithPerformHandler(cont) => {
                cont.collect(queue);
            }
            InvokeCountinuationStyle::WithBoth(cont1, cont2) => {
                cont1.collect(queue);
                cont2.collect(queue);
            }
            InvokeCountinuationStyle::Pandom(_) => {
                unreachable!("InvokeCountinuationStyle::Pandom should never be collected")
            }
        }
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Rootable<V> for Invoke<U, V> {
    fn upgrade(&self, collected: &mut Vec<GCArc<V>>) {
        if self.rootless {
            return;
        }
        let (func, arg, cont_style) = self.inner.as_ref();
        func.upgrade(collected);
        arg.upgrade(collected);
        match cont_style {
            InvokeCountinuationStyle::TailCall => {}
            InvokeCountinuationStyle::WithContinuation(cont)
            | InvokeCountinuationStyle::WithPerformHandler(cont) => {
                cont.upgrade(collected);
            }
            InvokeCountinuationStyle::WithBoth(cont1, cont2) => {
                cont1.upgrade(collected);
                cont2.upgrade(collected);
            }
            InvokeCountinuationStyle::Pandom(_) => {
                unreachable!("InvokeCountinuationStyle::Pandom should never be upgraded")
            }
        }
    }

    fn rootless(&self) -> bool {
        self.rootless
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for Invoke<Type<T>, T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;

    fn into_dispatcher(self) -> Type<T> {
        Type::Invoke(self)
    }

    fn as_ref_dispatcher<'a>(&'a self) -> Self::RefDispatcher<'a> {
        TypeRef::Invoke(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for Invoke<Type<T>, T> {
    fn check(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, super::TypeError<Type<T>, T>> {
        ctx.pattern_collector.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.coinductive_assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.bound_generic_variables,
            );
            match other {
                TypeRef::All(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Any(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Constraint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::SubOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Invoke(v) => {
                    let (self_func, self_arg, self_cont_style) = self.inner.as_ref();
                    let (v_func, v_arg, v_cont_style) = v.inner.as_ref();

                    Ok(test_true!(self_func.check(v_func.as_ref_dispatcher(), &mut inner_ctx)?)
                        & test_true!(self_arg.check(v_arg.as_ref_dispatcher(), &mut inner_ctx)?)
                        & match (self_cont_style, v_cont_style) {
                            (
                                InvokeCountinuationStyle::TailCall,
                                InvokeCountinuationStyle::TailCall,
                            ) => ThreeValuedLogic::True,
                            (
                                InvokeCountinuationStyle::WithContinuation(c1),
                                InvokeCountinuationStyle::WithContinuation(c2),
                            ) => c1.check(c2.as_ref_dispatcher(), &mut inner_ctx)?,
                            (
                                InvokeCountinuationStyle::WithPerformHandler(c1),
                                InvokeCountinuationStyle::WithPerformHandler(c2),
                            ) => c1.check(c2.as_ref_dispatcher(), &mut inner_ctx)?,
                            (
                                InvokeCountinuationStyle::WithBoth(c1a, c1b),
                                InvokeCountinuationStyle::WithBoth(c2a, c2b),
                            ) => {
                                test_true!(c1a.check(c2a.as_ref_dispatcher(), &mut inner_ctx)?)
                                    & test_true!(
                                        c1b.check(c2b.as_ref_dispatcher(), &mut inner_ctx)?
                                    )
                            }
                            _ => ThreeValuedLogic::False,
                        })
                }
                _ => Ok(ThreeValuedLogic::False),
            }
        })
    }

    fn subof(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut super::TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        ctx.pattern_collector.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.coinductive_assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.bound_generic_variables,
            );
            match other {
                TypeRef::Any(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::All(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Invoke(v) => {
                    let (self_func, self_arg, self_cont_style) = self.inner.as_ref();
                    let (v_func, v_arg, v_cont_style) = v.inner.as_ref();

                    Ok(test_true!(self_func.subof(v_func.as_ref_dispatcher(), &mut inner_ctx)?)
                        & test_true!(self_arg.subof(v_arg.as_ref_dispatcher(), &mut inner_ctx)?)
                        & match (self_cont_style, v_cont_style) {
                            (
                                InvokeCountinuationStyle::TailCall,
                                InvokeCountinuationStyle::TailCall,
                            ) => ThreeValuedLogic::True,
                            (
                                InvokeCountinuationStyle::WithContinuation(c1),
                                InvokeCountinuationStyle::WithContinuation(c2),
                            ) => c1.subof(c2.as_ref_dispatcher(), &mut inner_ctx)?,
                            (
                                InvokeCountinuationStyle::WithPerformHandler(c1),
                                InvokeCountinuationStyle::WithPerformHandler(c2),
                            ) => c1.subof(c2.as_ref_dispatcher(), &mut inner_ctx)?,
                            (
                                InvokeCountinuationStyle::WithBoth(c1a, c1b),
                                InvokeCountinuationStyle::WithBoth(c2a, c2b),
                            ) => {
                                test_true!(c1a.subof(c2a.as_ref_dispatcher(), &mut inner_ctx)?)
                                    & test_true!(
                                        c1b.subof(c2b.as_ref_dispatcher(), &mut inner_ctx)?
                                    )
                            }
                            _ => ThreeValuedLogic::False,
                        })
                }
                _ => Ok(ThreeValuedLogic::False),
            }
        })
    }

    fn reduce(
        &self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, super::TypeError<Type<T>, T>> {
        let (func, arg, cont_style) = self.inner.as_ref();
        let new_func = func.reduce(ctx)?;
        let new_arg = arg.reduce(ctx)?;
        let mut rootless = new_func.rootless() && new_arg.rootless();
        let new_cont_style = match cont_style {
            InvokeCountinuationStyle::TailCall => InvokeCountinuationStyle::TailCall,
            InvokeCountinuationStyle::WithContinuation(cont) => {
                let new_cont = cont.reduce(ctx)?;
                rootless &= new_cont.rootless();
                InvokeCountinuationStyle::WithContinuation(new_cont)
            }
            InvokeCountinuationStyle::WithPerformHandler(cont) => {
                let new_handler = cont.reduce(ctx)?;
                rootless &= new_handler.rootless();
                InvokeCountinuationStyle::WithPerformHandler(new_handler)
            }
            InvokeCountinuationStyle::WithBoth(cont1, cont2) => {
                let new_cont = cont1.reduce(ctx)?;
                let new_handler = cont2.reduce(ctx)?;
                rootless &= new_cont.rootless() && new_handler.rootless();
                InvokeCountinuationStyle::WithBoth(new_cont, new_handler)
            }
            InvokeCountinuationStyle::Pandom(_) => {
                unreachable!("InvokeCountinuationStyle::Pandom should never be reduced")
            }
        };
        Ok(Self {
            inner: Arc::new((new_func, new_arg, new_cont_style)),
            rootless,
            source_info: self.source_info.clone(),
        }
        .dispatch())
    }

    fn invoke(
        &self,
        _ctx: InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, super::TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.clone().dispatch().into()))
    }

    fn type_of(
        &self,
        _ctx: &mut TypeOfContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        // Invoke的类型是一个SubOf，表示它是一个函数调用类型，但具体是什么函数调用类型需要根据上下文来确定
        Ok(SubOf::new(self.clone(), self.source_info.clone()))
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        self.source_info.as_ref()
    }

    fn report_source_info(&self) -> crate::types::TypeReport {
        if let Some(loc) = self.source_info() {
            let span = loc.span().clone();
            let filepath = loc.source().filepath().to_string();
            ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                .with_message(format!("Function invocation type at {}", filepath))
                .with_label(
                    ariadne::Label::new((filepath, span)).with_message("Invocation defined here"),
                )
                .finish()
        } else {
            ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                .with_message("Function invocation type has no source location")
                .with_label(
                    ariadne::Label::new(("<unknown>".to_string(), 0..0))
                        .with_message("Location unknown"),
                )
                .finish()
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Invoke<Type<T>, T> {
    fn represent(
        &self,
        path: &mut FastCycleDetector<TaggedPtr<()>>,
        depth: usize,
        max_depth: usize,
    ) -> String {
        if depth > max_depth {
            return "...".to_string();
        }
        let (func, arg, cont_style) = self.inner.as_ref();
        let func_repr = func.represent(path, depth + 1, max_depth);
        let arg_repr = arg.represent(path, depth + 1, max_depth);
        let cont_repr = match cont_style {
            InvokeCountinuationStyle::TailCall => "tail".to_string(),
            InvokeCountinuationStyle::WithContinuation(cont) => {
                format!("cps({})", cont.represent(path, depth + 1, max_depth))
            }
            InvokeCountinuationStyle::WithPerformHandler(cont) => {
                format!("hps({})", cont.represent(path, depth + 1, max_depth))
            }
            InvokeCountinuationStyle::WithBoth(cont1, cont2) => {
                format!(
                    "chps({}, {})",
                    cont1.represent(path, depth + 1, max_depth),
                    cont2.represent(path, depth + 1, max_depth)
                )
            }
            InvokeCountinuationStyle::Pandom(_) => "pandom".to_string(),
        };
        format!("Invoke(func: {}, arg: {}, cont: {})", func_repr, arg_repr, cont_repr)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Invoke<Type<T>, T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new<
        U: AsDispatcher<Type<T>, T>,
        V: AsDispatcher<Type<T>, T>,
        A: AsDispatcher<Type<T>, T>,
    >(
        func: U,
        arg: V,
        continuation: Option<A>,
        perform_continuation: Option<A>,

        source_info: Option<Arc<SourceLocation>>,
    ) -> Type<T> {
        let func = func.into_dispatcher();
        let arg = arg.into_dispatcher();
        let continuation = continuation.map(|c| c.into_dispatcher());
        let raise_continuation = perform_continuation.map(|c| c.into_dispatcher());
        let rootless = func.rootless()
            && arg.rootless()
            && continuation.as_ref().is_none_or(|c| c.rootless())
            && raise_continuation.as_ref().is_none_or(|c| c.rootless());
        let continuation_style = match (continuation, raise_continuation) {
            (None, None) => InvokeCountinuationStyle::TailCall,
            (Some(cont), None) => InvokeCountinuationStyle::WithContinuation(cont),
            (None, Some(cont)) => InvokeCountinuationStyle::WithPerformHandler(cont),
            (Some(cont1), Some(cont2)) => InvokeCountinuationStyle::WithBoth(cont1, cont2),
        };

        Invoke { inner: Arc::new((func, arg, continuation_style)), rootless, source_info }
            .dispatch()
    }

    pub fn func(&self) -> &Type<T> {
        &self.inner.as_ref().0
    }

    pub fn arg(&self) -> &Type<T> {
        &self.inner.as_ref().1
    }

    pub fn continuation(&self) -> Option<&Type<T>> {
        match &self.inner.as_ref().2 {
            InvokeCountinuationStyle::TailCall
            | InvokeCountinuationStyle::WithPerformHandler(_)
            | InvokeCountinuationStyle::Pandom(_) => None,
            InvokeCountinuationStyle::WithBoth(cont, _)
            | InvokeCountinuationStyle::WithContinuation(cont) => Some(cont),
        }
    }

    pub fn perform_handler(&self) -> Option<&Type<T>> {
        match &self.inner.as_ref().2 {
            InvokeCountinuationStyle::TailCall
            | InvokeCountinuationStyle::WithContinuation(_)
            | InvokeCountinuationStyle::Pandom(_) => None,
            InvokeCountinuationStyle::WithBoth(_, cont)
            | InvokeCountinuationStyle::WithPerformHandler(cont) => Some(cont),
        }
    }

    pub fn continuation_style(&self) -> &InvokeCountinuationStyle<Type<T>, T> {
        &self.inner.as_ref().2
    }
}
