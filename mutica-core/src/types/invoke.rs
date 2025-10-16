use std::sync::Arc;

use arc_gc::{arc::GCArc, gc::GC, traceable::GCTraceable};

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, Type, TypeCheckContext, TypeError, TypeRef,
        closure::{Closure, ClosureEnv, ParamEnv},
        pattern::Pattern,
        type_bound::TypeBound,
        variable::Variable,
    },
    util::{collector::Collector, cycle_detector::FastCycleDetector, rootstack::RootStack},
};

pub struct Invoke<T: GcAllocObject<T, Inner = Type<T>>> {
    // 0: function
    // 1: argument
    // 2: continuation
    inner: Arc<(Type<T>, Type<T>, Option<Type<T>>)>,
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
        if let Some(cont) = &self.inner.2 {
            cont.collect(queue);
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
        if let Some(cont) = &self.inner.2 {
            cont.upgrade(collected);
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
    fn is(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<Option<()>, super::TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env);
            match other {
                TypeRef::Invoke(v) => {
                    let func_eq = self
                        .inner
                        .0
                        .is(v.inner.0.as_ref_dispatcher(), &mut inner_ctx)?;
                    if func_eq.is_none() {
                        return Ok(None);
                    }
                    let arg_eq = self
                        .inner
                        .1
                        .is(v.inner.1.as_ref_dispatcher(), &mut inner_ctx)?;
                    if arg_eq.is_none() {
                        return Ok(None);
                    }
                    let cont_eq = match (&self.inner.2, &v.inner.2) {
                        (Some(cont1), Some(cont2)) => {
                            cont1.is(cont2.as_ref_dispatcher(), &mut inner_ctx)?
                        }
                        (None, None) => Some(()),
                        _ => None,
                    };
                    if cont_eq.is_none() {
                        return Ok(None);
                    }
                    Ok(Some(()))
                }
                TypeRef::Bound(TypeBound::Top) => Ok(Some(())),
                TypeRef::Specialize(v) => v.has(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Generalize(v) => v.has(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.has(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.has(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.has(self.as_ref_dispatcher(), &mut inner_ctx),
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
            self.inner.2.clone().map(|t| t.reduce(ctx)).transpose()?,
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
        if let Some(cont) = &self.inner.2 {
            format!(
                "CPS({}, {}, {})",
                self.inner.0.represent(path),
                self.inner.1.represent(path),
                cont.represent(path)
            )
        } else {
            format!(
                "TailCall({}, {})",
                self.inner.0.represent(path),
                self.inner.1.represent(path)
            )
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Invoke<T> {
    pub fn new<
        U: AsDispatcher<Type<T>, T>,
        V: AsDispatcher<Type<T>, T>,
        W: AsDispatcher<Type<T>, T>,
    >(
        func: U,
        arg: V,
        continuation: Option<W>,
    ) -> Type<T> {
        let func = func.into_dispatcher();
        let arg = arg.into_dispatcher();
        let continuation = continuation.map(|c| c.into_dispatcher());
        let all_nf = func.is_normal_form()
            && arg.is_normal_form()
            && continuation.as_ref().map_or(true, |c| c.is_normal_form());
        let inner = Arc::new((func, arg, continuation));
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
        self.inner.2.as_ref()
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Invoke<T> {
    /// Flattens a nested computation by composing continuations.
    ///
    /// This method is the core of the scheduler's ability to handle algebraic effects and
    /// deep CPS-style function calls. It addresses the scenario where applying a function
    /// does not yield a final value, but rather another `Invoke` instruction that needs to be
    /// executed.
    ///
    /// ## The Problem: Nested Control Flow
    ///
    /// In a Continuation-Passing Style (CPS) model, a function call like `f(g(x))` is
    /// linearized into a sequence. However, at the instruction level, the execution of
    /// `self` (representing the call to `f`) might result in `v`, where `v` itself is the
    /// instruction for `g(x)`. This creates a nested control flow: `handle(invoke_g, k_f)`.
    /// Our linear scheduler cannot directly execute this tree-like structure.
    ///
    /// ## The Solution: Continuation Composition
    ///
    /// This function "flattens" the nested structure by creating a new, composed continuation.
    /// It transforms a nested computation into a single, linear step for the scheduler.
    ///
    /// ### Formal Semantics
    ///
    /// This function implements the following reduction rule, where `self` is the outer
    /// `Invoke` (representing the context `k`) and `v` is the result of its target function:
    ///
    /// *   **If `v` is a value `val`**:
    ///     The operation is `k(val)`. We simply apply the continuation of `self` to the value.
    ///
    /// *   **If `v` is another instruction `Invoke<A, B, C>`**:
    ///     The operation is `k(Invoke<A, B, C>)`. This is the nested case.
    ///     We transform it according to the rule:
    ///     `k(Invoke<A, B, C>)  --->  Invoke<A, B, (λx. k(C(x)))>`
    ///
    ///     Where:
    ///     *   `self` represents the invocation context that provides `k` (`self.continuation`).
    ///     *   `v` is the inner `Invoke<A, B, C>`.
    ///     *   `A` is `v.func`, `B` is `v.arg`, `C` is `v.continuation`.
    ///     *   `(λx. k(C(x)))` is the new, composed continuation created by this function.
    ///     *   The returned `Invoke` is the flattened instruction `Invoke<A, B, ...>` ready
    ///       for the next scheduler step.
    ///
    /// # Parameters
    ///
    /// * `self`: The outer `Invoke` instruction, whose continuation `k` acts as the context.
    /// * `v`: The result of executing `self.func`. This can be a final value or another `Invoke` instruction.
    /// * `gc`: A reference to the garbage collector for creating new closures.
    ///
    /// # Returns
    ///
    /// * `Ok(Type)`: The next instruction for the scheduler. If `v` was a value,
    ///   this is the result of `k(v)`. If `v` was a nested `Invoke`, this is the new,
    ///   flattened `Invoke` instruction.
    /// * `Err(TypeError)`: If applying the continuation fails.
    pub fn flat_compose<'roots>(
        &self,
        v: Type<T>,
        gc: &mut GC<T>,
        roots: &'roots mut RootStack<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        // The `map` here is used to traverse the type structure without deep recursion
        // if `v` is already a value.
        v.map_inner(&mut FastCycleDetector::new(), |_, ty| match ty {
            // Case 1: The result `v` is another `Invoke` instruction (the nested case).
            TypeRef::Invoke(invoke) => {
                if self.inner.2.is_none() {
                    // Tail call optimization: If there is no outer continuation,
                    return Ok(invoke.clone().dispatch());
                }
                if invoke.inner.2.is_none() {
                    // Tail call optimization: If there is no inner continuation,
                    // we can directly use the outer continuation.
                    return Ok(Invoke::new(
                        &invoke.inner.0,
                        &invoke.inner.1,
                        self.inner.2.as_ref(),
                    ));
                }
                // We are in the k(Invoke<A, B, C>) case.
                // We construct the new continuation: λx. k(C(x))
                // which translates to: λx. self.continuation(invoke.continuation(x))
                Ok(Invoke::new(
                    // A: The target function of the inner instruction.
                    &invoke.inner.0,
                    // B: The argument of the inner instruction.
                    &invoke.inner.1,
                    // The new composed continuation: λx. C(x, k)
                    // Note: In our model, this becomes λx. Invoke(C, x, k)
                    Some(Closure::new(
                        1,                                 // Arity of the new closure (takes one argument `x`)
                        Pattern::new(0, TypeBound::top()), // The parameter `x`
                        // The body of the new closure: Invoke(C, x, k)
                        Invoke::new(
                            // C: The continuation of the inner instruction.
                            invoke.inner.2.as_ref().unwrap(),
                            // x: The value that will be passed to C.
                            Variable::new_debruijn(0),
                            // k: The continuation of the outer instruction (`self`).
                            self.inner.2.as_ref(),
                        ),
                        None::<Type<T>>,
                        ClosureEnv::new(Vec::<Type<T>>::new()), // The new closure captures no variables.
                    )),
                ))
            }
            // Case 2: The result `v` is a final value.
            _ => {
                if self.inner.2.is_none() {
                    // Tail call optimization: If there is no outer continuation,
                    // simply return the value.
                    return Ok(ty.clone_data());
                }
                // We are in the k(val) case.
                // Simply invoke the outer continuation `k` with the value `v`.
                let closure_env: ClosureEnv<Type<T>, T> = ClosureEnv::new(Vec::<Type<T>>::new());
                let param_env: ParamEnv<Type<T>, T> =
                    ParamEnv::from_collector(Collector::new()).unwrap().unwrap();
                let mut rec_assumptions = smallvec::SmallVec::new();
                let mut invoke_ctx: InvokeContext<'_, '_, Type<T>, T> = InvokeContext::new(
                    &v,
                    &closure_env,
                    &param_env,
                    None, // The continuation's own continuation is not needed here.
                    &mut rec_assumptions,
                    gc,
                    roots,
                );
                self.inner.2.as_ref().unwrap().invoke(&mut invoke_ctx)
            }
        })?
    }

    pub fn flat_compose_stack<'roots>(
        &self,
        v: Type<T>,
        gc: &mut GC<T>,
        roots: &'roots mut RootStack<Type<T>, T>,
        cont_stack: &mut Vec<Type<T>>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        if self.inner.2.is_none() {
            // Tail call optimization: If there is no outer continuation,
            return Ok(v);
        }

        v.map_inner(&mut FastCycleDetector::new(), |_, ty| match ty {
            // Case 1: The result `v` is another `Invoke` instruction (the nested case).
            TypeRef::Invoke(invoke) => {
                if invoke.inner.2.is_none() {
                    // Tail call optimization: If there is no inner continuation,
                    // we can directly use the outer continuation.
                    return Ok(Invoke::new(
                        &invoke.inner.0,
                        &invoke.inner.1,
                        self.inner.2.as_ref(),
                    ));
                }

                // We are in the k(Invoke<A, B, C>) case.
                cont_stack.push(self.inner.2.clone().unwrap());
                Ok(ty.clone_data())
            }
            // Case 2: The result `v` is a final value.
            _ => {
                // We are in the k(val) case.
                // Simply invoke the outer continuation `k` with the value `v`.
                let closure_env: ClosureEnv<Type<T>, T> = ClosureEnv::new(Vec::<Type<T>>::new());
                let param_env: ParamEnv<Type<T>, T> =
                    ParamEnv::from_collector(Collector::new()).unwrap().unwrap();
                let mut rec_assumptions: smallvec::SmallVec<
                    [(super::TaggedPtr<()>, Type<T>, bool); 8],
                > = smallvec::SmallVec::new();
                let mut invoke_ctx: InvokeContext<'_, '_, Type<T>, T> = InvokeContext::new(
                    &v,
                    &closure_env,
                    &param_env,
                    None, // The continuation's own continuation is not needed here.
                    &mut rec_assumptions,
                    gc,
                    roots,
                );
                self.inner.2.as_ref().unwrap().invoke(&mut invoke_ctx)
            }
        })?
    }
}
