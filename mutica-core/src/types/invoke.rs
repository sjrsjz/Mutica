use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::{
    test_true,
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, TaggedPtr, Type, TypeCheckContext, TypeError,
        TypeRef,
    },
    util::{
        arc_opt::ArcOpt, collector::CollectorExt, cycle_detector::FastCycleDetector,
        source_info::SourceLocation, three_valued_logic::ThreeValuedLogic,
    },
};

pub enum InvokeCountinuationStyle<T: GcAllocObject<T, Inner = Type<T>>> {
    TailCall,
    WithContinuation(Type<T>),   // 指定普通续体
    WithPerformHandler(Type<T>), // 指定Perform续体Handler
    WithBoth(Type<T>, Type<T>),  // 指定Perform续体和普通续体
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for InvokeCountinuationStyle<T> {
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
        }
    }
}

pub struct Invoke<T: GcAllocObject<T, Inner = Type<T>>> {
    // 0: function
    // 1: argument
    // 2: continuation
    // 3: source_info
    // 4: is_nf
    #[allow(clippy::type_complexity)]
    inner: ArcOpt<(Type<T>, Type<T>, InvokeCountinuationStyle<T>, Option<Arc<SourceLocation>>)>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Invoke<T> {
    fn clone(&self) -> Self {
        Self { inner: self.inner.clone() }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Invoke<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        let (func, arg, cont_style, _) = self.inner.as_ref();
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
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Invoke<T> {
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        let (func, arg, cont_style, _) = self.inner.as_ref();
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
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for Invoke<T> {
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

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for Invoke<T> {
    fn check(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, super::TypeError<Type<T>, T>> {
        ctx.pattern_collector.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.collected,
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
                    let (self_func, self_arg, self_cont_style, _) = self.inner.as_ref();
                    let (v_func, v_arg, v_cont_style, _) = v.inner.as_ref();

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
                ctx.assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.collected,
            );
            match other {
                TypeRef::Any(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::All(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Invoke(v) => {
                    let (self_func, self_arg, self_cont_style, _) = self.inner.as_ref();
                    let (v_func, v_arg, v_cont_style, _) = v.inner.as_ref();

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
        mut self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, super::TypeError<Type<T>, T>> {
        match self.inner.modify(|(func, arg, cont_style, source_info)| {
            let new_func = func.reduce(ctx)?;
            let new_arg = arg.reduce(ctx)?;

            let new_cont_style = match cont_style {
                InvokeCountinuationStyle::TailCall => InvokeCountinuationStyle::TailCall,
                InvokeCountinuationStyle::WithContinuation(cont) => {
                    InvokeCountinuationStyle::WithContinuation(cont.reduce(ctx)?)
                }
                InvokeCountinuationStyle::WithPerformHandler(cont) => {
                    InvokeCountinuationStyle::WithPerformHandler(cont.reduce(ctx)?)
                }
                InvokeCountinuationStyle::WithBoth(cont1, cont2) => {
                    InvokeCountinuationStyle::WithBoth(cont1.reduce(ctx)?, cont2.reduce(ctx)?)
                }
            };

            Ok((new_func, new_arg, new_cont_style, source_info))
        })? {
            Some(()) => Ok(self.dispatch()),
            None => {
                let (func, arg, _, source_info) = self.inner.as_ref();
                Ok(Self::new(
                    func.clone().reduce(ctx)?,
                    arg.clone().reduce(ctx)?,
                    self.continuation().map(|c| c.clone().reduce(ctx)).transpose()?,
                    self.perform_handler().map(|c| c.clone().reduce(ctx)).transpose()?,
                    source_info.clone(),
                ))
            }
        }
    }

    fn invoke(
        self,
        _ctx: InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, super::TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.dispatch().into()))
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        self.inner.as_ref().3.as_ref()
    }

    fn report_source_info(&self) -> crate::types::TypeReport {
        if let Some(loc) = self.inner.as_ref().3.as_ref() {
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

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Invoke<T> {
    fn represent(
        &self,
        path: &mut FastCycleDetector<TaggedPtr<()>>,
        depth: usize,
        max_depth: usize,
    ) -> String {
        if depth > max_depth {
            return "...".to_string();
        }
        let (func, arg, cont_style, _) = self.inner.as_ref();
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
        };
        format!("Invoke(func: {}, arg: {}, cont: {})", func_repr, arg_repr, cont_repr)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Invoke<T> {
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

        let continuation_style = match (continuation, raise_continuation) {
            (None, None) => InvokeCountinuationStyle::TailCall,
            (Some(cont), None) => InvokeCountinuationStyle::WithContinuation(cont),
            (None, Some(cont)) => InvokeCountinuationStyle::WithPerformHandler(cont),
            (Some(cont1), Some(cont2)) => InvokeCountinuationStyle::WithBoth(cont1, cont2),
        };

        Self { inner: ArcOpt::new((func, arg, continuation_style, source_info)) }.dispatch()
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
            | InvokeCountinuationStyle::WithPerformHandler(_) => None,
            InvokeCountinuationStyle::WithBoth(cont, _)
            | InvokeCountinuationStyle::WithContinuation(cont) => Some(cont),
        }
    }

    pub fn perform_handler(&self) -> Option<&Type<T>> {
        match &self.inner.as_ref().2 {
            InvokeCountinuationStyle::TailCall | InvokeCountinuationStyle::WithContinuation(_) => {
                None
            }
            InvokeCountinuationStyle::WithBoth(_, cont)
            | InvokeCountinuationStyle::WithPerformHandler(cont) => Some(cont),
        }
    }

    pub fn continuation_style(&self) -> &InvokeCountinuationStyle<T> {
        &self.inner.as_ref().2
    }

    pub fn take(
        self,
    ) -> (Type<T>, Type<T>, InvokeCountinuationStyle<T>, Option<Arc<SourceLocation>>) {
        match self.inner.take() {
            Ok((func, arg, cont_style, source_info)) => (func, arg, cont_style, source_info),
            Err(v) => {
                let (func, arg, cont_style, source_info) = v.as_ref();
                (func.clone(), arg.clone(), cont_style.clone(), source_info.clone())
            }
        }
    }
}
