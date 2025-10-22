use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, Type, TypeCheckContext, TypeError, TypeRef,
        type_bound::TypeBound,
    },
    util::{cycle_detector::FastCycleDetector, three_valued_logic::ThreeValuedLogic},
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
    inner: Arc<(Type<T>, Type<T>, InvokeCountinuationStyle<T>)>,
    is_nf: ThreeValuedLogic,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Invoke<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            is_nf: self.is_nf,
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Invoke<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        self.inner.0.collect(queue);
        self.inner.1.collect(queue);
        match self.inner.2 {
            InvokeCountinuationStyle::TailCall => {}
            InvokeCountinuationStyle::WithContinuation(ref cont)
            | InvokeCountinuationStyle::WithPerformHandler(ref cont) => {
                cont.collect(queue);
            }
            InvokeCountinuationStyle::WithBoth(ref cont1, ref cont2) => {
                cont1.collect(queue);
                cont2.collect(queue);
            }
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GcAllocObject<T> for Invoke<T> {
    type Inner = Type<T>;
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Invoke<T> {
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        self.inner.0.upgrade(collected);
        self.inner.1.upgrade(collected);
        match self.inner.2 {
            InvokeCountinuationStyle::TailCall => {}
            InvokeCountinuationStyle::WithContinuation(ref cont)
            | InvokeCountinuationStyle::WithPerformHandler(ref cont) => {
                cont.upgrade(collected);
            }
            InvokeCountinuationStyle::WithBoth(ref cont1, ref cont2) => {
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
    fn fulfill(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<Option<()>, super::TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env);
            match other {
                TypeRef::Specialize(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Generalize(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Bound(TypeBound::Top) => Ok(Some(())),
                TypeRef::Invoke(v) => {
                    let func_eq = self
                        .inner
                        .0
                        .fulfill(v.inner.0.as_ref_dispatcher(), &mut inner_ctx)?;
                    if func_eq.is_none() {
                        return Ok(None);
                    }
                    let arg_eq = self
                        .inner
                        .1
                        .fulfill(v.inner.1.as_ref_dispatcher(), &mut inner_ctx)?;
                    if arg_eq.is_none() {
                        return Ok(None);
                    }
                    let cont_eq = match (&self.inner.2, &v.inner.2) {
                        (
                            InvokeCountinuationStyle::TailCall,
                            InvokeCountinuationStyle::TailCall,
                        ) => Some(()),
                        (
                            InvokeCountinuationStyle::WithContinuation(c1),
                            InvokeCountinuationStyle::WithContinuation(c2),
                        ) => c1.fulfill(c2.as_ref_dispatcher(), &mut inner_ctx)?,
                        (
                            InvokeCountinuationStyle::WithPerformHandler(c1),
                            InvokeCountinuationStyle::WithPerformHandler(c2),
                        ) => c1.fulfill(c2.as_ref_dispatcher(), &mut inner_ctx)?,
                        (
                            InvokeCountinuationStyle::WithBoth(c1a, c1b),
                            InvokeCountinuationStyle::WithBoth(c2a, c2b),
                        ) => {
                            let res_a = c1a.fulfill(c2a.as_ref_dispatcher(), &mut inner_ctx)?;
                            if res_a.is_none() {
                                return Ok(None);
                            }
                            let res_b = c1b.fulfill(c2b.as_ref_dispatcher(), &mut inner_ctx)?;
                            if res_b.is_none() {
                                return Ok(None);
                            }
                            Some(())
                        }
                        _ => None,
                    };
                    if cont_eq.is_none() {
                        return Ok(None);
                    }
                    Ok(Some(()))
                }
                _ => Ok(None),
            }
        })
    }

    fn reduce(
        self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, super::TypeError<Type<T>, T>> {
        Ok(Self::new(
            self.inner.0.clone().reduce(ctx)?,
            self.inner.1.clone().reduce(ctx)?,
            self.continuation()
                .map(|c| c.clone().reduce(ctx))
                .transpose()?,
            self.perform_handler()
                .map(|c| c.clone().reduce(ctx))
                .transpose()?,
        ))
    }

    fn invoke(
        &self,
        _ctx: &mut InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, super::TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.clone().dispatch().into()))
    }

    fn is_normal_form(&self) -> ThreeValuedLogic {
        self.is_nf
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Invoke<T> {
    fn represent(&self, path: &mut FastCycleDetector<*const ()>) -> String {
        let func_repr = self.inner.0.represent(path);
        let arg_repr = self.inner.1.represent(path);
        let cont_repr = match &self.inner.2 {
            InvokeCountinuationStyle::TailCall => "tail".to_string(),
            InvokeCountinuationStyle::WithContinuation(cont) => {
                format!("cps({})", cont.represent(path))
            }
            InvokeCountinuationStyle::WithPerformHandler(cont) => {
                format!("hps({})", cont.represent(path))
            }
            InvokeCountinuationStyle::WithBoth(cont1, cont2) => {
                format!("chps({}, {})", cont1.represent(path), cont2.represent(path))
            }
        };
        format!(
            "Invoke(func: {}, arg: {}, cont: {})",
            func_repr, arg_repr, cont_repr
        )
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Invoke<T> {
    pub fn new<
        U: AsDispatcher<Type<T>, T>,
        V: AsDispatcher<Type<T>, T>,
        A: AsDispatcher<Type<T>, T>,
    >(
        func: U,
        arg: V,
        continuation: Option<A>,
        perform_continuation: Option<A>,
    ) -> Type<T> {
        let func = func.into_dispatcher();
        let arg = arg.into_dispatcher();
        let continuation = continuation.map(|c| c.into_dispatcher());
        let raise_continuation = perform_continuation.map(|c| c.into_dispatcher());
        let all_nf = func.is_normal_form()
            & arg.is_normal_form()
            & continuation
                .as_ref()
                .map_or(ThreeValuedLogic::True, |c| c.is_normal_form())
            & raise_continuation
                .as_ref()
                .map_or(ThreeValuedLogic::True, |c| c.is_normal_form());

        let continuation_style = match (continuation, raise_continuation) {
            (None, None) => InvokeCountinuationStyle::TailCall,
            (Some(cont), None) => InvokeCountinuationStyle::WithContinuation(cont),
            (None, Some(cont)) => InvokeCountinuationStyle::WithPerformHandler(cont),
            (Some(cont1), Some(cont2)) => InvokeCountinuationStyle::WithBoth(cont1, cont2),
        };

        let inner = Arc::new((func, arg, continuation_style));
        Self {
            inner,
            is_nf: all_nf,
        }
        .dispatch()
    }

    pub fn func(&self) -> &Type<T> {
        &self.inner.0
    }

    pub fn arg(&self) -> &Type<T> {
        &self.inner.1
    }

    pub fn continuation(&self) -> Option<&Type<T>> {
        match &self.inner.2 {
            InvokeCountinuationStyle::TailCall
            | InvokeCountinuationStyle::WithPerformHandler(_) => None,
            InvokeCountinuationStyle::WithBoth(cont, _)
            | InvokeCountinuationStyle::WithContinuation(cont) => Some(cont),
        }
    }

    pub fn perform_handler(&self) -> Option<&Type<T>> {
        match &self.inner.2 {
            InvokeCountinuationStyle::TailCall | InvokeCountinuationStyle::WithContinuation(_) => {
                None
            }
            InvokeCountinuationStyle::WithBoth(_, cont)
            | InvokeCountinuationStyle::WithPerformHandler(cont) => Some(cont),
        }
    }

    pub fn continuation_style(&self) -> &InvokeCountinuationStyle<T> {
        &self.inner.2
    }
}
