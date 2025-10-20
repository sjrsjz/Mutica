use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, Type, TypeCheckContext, TypeError, TypeRef,
        type_bound::TypeBound,
    },
    util::cycle_detector::FastCycleDetector,
};

pub enum InvokeCountinuationStyle<T: GcAllocObject<T, Inner = Type<T>>> {
    TailCall,
    CPS(Type<T>),           // 指定普通续体
    HPS(Type<T>),           // 指定Perform续体Handler
    CHPS(Type<T>, Type<T>), // 指定Perform续体和普通续体
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for InvokeCountinuationStyle<T> {
    fn clone(&self) -> Self {
        match self {
            InvokeCountinuationStyle::TailCall => InvokeCountinuationStyle::TailCall,
            InvokeCountinuationStyle::CPS(cont) => InvokeCountinuationStyle::CPS(cont.clone()),
            InvokeCountinuationStyle::HPS(cont) => InvokeCountinuationStyle::HPS(cont.clone()),
            InvokeCountinuationStyle::CHPS(cont1, cont2) => {
                InvokeCountinuationStyle::CHPS(cont1.clone(), cont2.clone())
            }
        }
    }
}

pub struct Invoke<T: GcAllocObject<T, Inner = Type<T>>> {
    // 0: function
    // 1: argument
    // 2: continuation
    inner: Arc<(Type<T>, Type<T>, InvokeCountinuationStyle<T>)>,
    is_nf: bool,
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
            InvokeCountinuationStyle::CPS(ref cont) | InvokeCountinuationStyle::HPS(ref cont) => {
                cont.collect(queue);
            }
            InvokeCountinuationStyle::CHPS(ref cont1, ref cont2) => {
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
            InvokeCountinuationStyle::CPS(ref cont) | InvokeCountinuationStyle::HPS(ref cont) => {
                cont.upgrade(collected);
            }
            InvokeCountinuationStyle::CHPS(ref cont1, ref cont2) => {
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
                        (InvokeCountinuationStyle::CPS(c1), InvokeCountinuationStyle::CPS(c2)) => {
                            c1.fulfill(c2.as_ref_dispatcher(), &mut inner_ctx)?
                        }
                        (InvokeCountinuationStyle::HPS(c1), InvokeCountinuationStyle::HPS(c2)) => {
                            c1.fulfill(c2.as_ref_dispatcher(), &mut inner_ctx)?
                        }
                        (
                            InvokeCountinuationStyle::CHPS(c1a, c1b),
                            InvokeCountinuationStyle::CHPS(c2a, c2b),
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

    fn is_normal_form(&self) -> bool {
        self.is_nf
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Invoke<T> {
    fn represent(&self, path: &mut FastCycleDetector<*const ()>) -> String {
        let func_repr = self.inner.0.represent(path);
        let arg_repr = self.inner.1.represent(path);
        let cont_repr = match &self.inner.2 {
            InvokeCountinuationStyle::TailCall => "tail".to_string(),
            InvokeCountinuationStyle::CPS(cont) => format!("cps({})", cont.represent(path)),
            InvokeCountinuationStyle::HPS(cont) => format!("rps({})", cont.represent(path)),
            InvokeCountinuationStyle::CHPS(cont1, cont2) => {
                format!("crps({}, {})", cont1.represent(path), cont2.represent(path))
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
        raise_continuation: Option<A>,
    ) -> Type<T> {
        let func = func.into_dispatcher();
        let arg = arg.into_dispatcher();
        let continuation = continuation.map(|c| c.into_dispatcher());
        let raise_continuation = raise_continuation.map(|c| c.into_dispatcher());
        let all_nf = func.is_normal_form()
            && arg.is_normal_form()
            && continuation.as_ref().map_or(true, |c| c.is_normal_form())
            && raise_continuation
                .as_ref()
                .map_or(true, |c| c.is_normal_form());

        let continuation_style = match (continuation, raise_continuation) {
            (None, None) => InvokeCountinuationStyle::TailCall,
            (Some(cont), None) => InvokeCountinuationStyle::CPS(cont),
            (None, Some(cont)) => InvokeCountinuationStyle::HPS(cont),
            (Some(cont1), Some(cont2)) => InvokeCountinuationStyle::CHPS(cont1, cont2),
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
            InvokeCountinuationStyle::TailCall => None,
            InvokeCountinuationStyle::CPS(cont) | InvokeCountinuationStyle::HPS(cont) => Some(cont),
            InvokeCountinuationStyle::CHPS(cont1, _) => Some(cont1),
        }
    }

    pub fn perform_handler(&self) -> Option<&Type<T>> {
        match &self.inner.2 {
            InvokeCountinuationStyle::TailCall => None,
            InvokeCountinuationStyle::CPS(_) | InvokeCountinuationStyle::HPS(_) => None,
            InvokeCountinuationStyle::CHPS(_, cont2) => Some(cont2),
        }
    }

    pub fn continuation_style(&self) -> &InvokeCountinuationStyle<T> {
        &self.inner.2
    }
}

pub fn find_last_continuation<'a, T: GcAllocObject<T, Inner = Type<T>>>(
    cont_stack: &'a [InvokeCountinuationStyle<T>],
) -> Option<&'a Type<T>> {
    for cont in cont_stack.iter().rev() {
        match cont {
            InvokeCountinuationStyle::TailCall | InvokeCountinuationStyle::HPS(_) => {}
            InvokeCountinuationStyle::CPS(cont) => {
                return Some(cont);
            }
            InvokeCountinuationStyle::CHPS(cont1, _) => {
                return Some(cont1);
            }
        }
    }
    None
}

pub fn find_last_perform_handler<'a, T: GcAllocObject<T, Inner = Type<T>>>(
    cont_stack: &'a [InvokeCountinuationStyle<T>],
) -> Option<&'a Type<T>> {
    for cont in cont_stack.iter().rev() {
        match cont {
            InvokeCountinuationStyle::TailCall | InvokeCountinuationStyle::CPS(_) => {}
            InvokeCountinuationStyle::HPS(cont) => {
                return Some(cont);
            }
            InvokeCountinuationStyle::CHPS(_, cont2) => {
                return Some(cont2);
            }
        }
    }
    None
}

pub fn shrink_to_last_perform_handler<'a, T: GcAllocObject<T, Inner = Type<T>>>(
    cont_stack: &mut Vec<InvokeCountinuationStyle<T>>,
) -> Option<(Type<T>, Option<Type<T>>)> {
    while let Some(cont) = cont_stack.pop() {
        match cont {
            InvokeCountinuationStyle::TailCall | InvokeCountinuationStyle::CPS(_) => {}
            InvokeCountinuationStyle::HPS(cont) => {
                let last_cont = find_last_continuation(cont_stack);
                return Some((cont, last_cont.cloned()));
            }
            InvokeCountinuationStyle::CHPS(cont, cont2) => {
                return Some((cont2, Some(cont)));
            }
        }
    }
    None
}