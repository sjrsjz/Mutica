use std::sync::RwLock;

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, TaggedPtr, Type, TypeCheckContext, TypeError,
        TypeRef,
    },
    util::{
        arc_opt::ArcOpt, cycle_detector::FastCycleDetector, three_valued_logic::ThreeValuedLogic,
    },
};

use crate::types::CoinductiveTypeRef;

// 理论上来说应当把 debruijn_index 直接和 Type 绑定起来（因为Pattern只是一个附加信息）
// 但是为了实现的简洁性，这里就先分开了
pub struct Pattern<T: GcAllocObject<T, Inner = Type<T>>> {
    inner: ArcOpt<(usize, Type<T>, RwLock<ThreeValuedLogic>)>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Pattern<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Pattern<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        let (_, expr, _) = self.inner.as_ref();
        expr.collect(queue);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Pattern<T> {
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        let (_, expr, _) = self.inner.as_ref();
        expr.upgrade(collected);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Pattern<T> {
    fn represent(
        &self,
        path: &mut crate::util::cycle_detector::FastCycleDetector<TaggedPtr<()>>,
    ) -> String {
        let (debruijn_index, expr, _) = self.inner.as_ref();
        format!("λ.{} : {}", debruijn_index, expr.represent(path))
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for Pattern<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;

    fn into_dispatcher(self) -> Type<T> {
        Type::Pattern(self)
    }

    fn as_ref_dispatcher<'a>(&'a self) -> Self::RefDispatcher<'a> {
        TypeRef::Pattern(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for Pattern<T> {
    fn check(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        if ctx.rhs {
            ctx.pattern_env.collect(|pattern_env| {
                let mut inner_ctx =
                    TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.rhs);
                let (debruijn_index, expr, _) = self.inner.as_ref();
                match expr.check(other, &mut inner_ctx)? {
                    ThreeValuedLogic::True => {
                        pattern_env.push((*debruijn_index, other.clone_data()));
                        Ok(ThreeValuedLogic::True)
                    }
                    ThreeValuedLogic::False => Ok(ThreeValuedLogic::False),
                    ThreeValuedLogic::Unknown => Ok(ThreeValuedLogic::Unknown),
                }
            })
        } else {
            let (_, expr, _) = self.inner.as_ref();
            expr.check(other, ctx)
        }
    }

    fn subof(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        if ctx.rhs {
            ctx.pattern_env.collect(|pattern_env| {
                let mut inner_ctx =
                    TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.rhs);
                let (debruijn_index, expr, _) = self.inner.as_ref();
                match expr.subof(other, &mut inner_ctx)? {
                    ThreeValuedLogic::True => {
                        pattern_env.push((*debruijn_index, other.clone_data()));
                        Ok(ThreeValuedLogic::True)
                    }
                    ThreeValuedLogic::False => Ok(ThreeValuedLogic::False),
                    ThreeValuedLogic::Unknown => Ok(ThreeValuedLogic::Unknown),
                }
            })
        } else {
            let (_, expr, _) = self.inner.as_ref();
            expr.subof(other, ctx)
        }
    }

    fn invoke(self, ctx: InvokeContext<Type<T>, T>) -> Result<Type<T>, TypeError<Type<T>, T>> {
        match self.inner.take() {
            Ok(v) => {
                let (_, expr, _) = v;
                expr.invoke(ctx)
            }
            Err(v) => {
                let (_, expr, _) = v.as_ref();
                expr.clone().invoke(ctx)
            }
        }
    }

    fn reduce(
        mut self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        match self.inner.modify(|(debruijn_index, expr, is_nf)| {
            let new_expr = expr.reduce(ctx)?;
            let new_is_nf = new_expr.is_normal_form();
            if let Ok(mut nf_lock) = is_nf.write() {
                *nf_lock = new_is_nf;
            }
            Ok((debruijn_index, new_expr, is_nf))
        })? {
            Some(()) => Ok(self.dispatch()),
            None => {
                let (debruijn_index, expr, _) = self.inner.as_ref();
                let new_expr = expr.clone().reduce(ctx)?;
                Ok(Self::new(*debruijn_index, new_expr))
            }
        }
    }

    fn tagged_ptr(&self) -> super::TaggedPtr<()> {
        super::TaggedPtr::new_unique(&self as *const _ as *const ())
    }

    fn is_normal_form(&self) -> ThreeValuedLogic {
        let (_, _, is_nf) = self.inner.as_ref();
        match is_nf.read() {
            Ok(v) => *v,
            Err(_) => ThreeValuedLogic::False,
        }
    }

    fn recalculate_normal_form(&self, cycle_detector: &mut FastCycleDetector<TaggedPtr<()>>) {
        let (_, expr, is_nf) = self.inner.as_ref();
        expr.recalculate_normal_form(cycle_detector);
        let new_nf = expr.is_normal_form();
        if let Ok(mut nf_lock) = is_nf.write() {
            *nf_lock = new_nf;
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveTypeWithAny<Type<T>, T> for Pattern<T> {
    #[stacksafe::stacksafe]
    fn accept(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        if ctx.rhs {
            let (_, expr, _) = self.inner.as_ref();
            other.check(expr.as_ref_dispatcher(), ctx)
        } else {
            ctx.pattern_env.collect(|pattern_env| {
                let mut inner_ctx =
                    TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.rhs);
                let (debruijn_index, expr, _) = self.inner.as_ref();
                match other.check(expr.as_ref_dispatcher(), &mut inner_ctx)? {
                    ThreeValuedLogic::True => {
                        pattern_env.push((*debruijn_index, other.clone_data()));
                        Ok(ThreeValuedLogic::True)
                    }
                    ThreeValuedLogic::False => Ok(ThreeValuedLogic::False),
                    ThreeValuedLogic::Unknown => Ok(ThreeValuedLogic::Unknown),
                }
            })
        }
    }

    #[stacksafe::stacksafe]
    fn superof(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        if ctx.rhs {
            let (_, expr, _) = self.inner.as_ref();
            other.subof(expr.as_ref_dispatcher(), ctx)
        } else {
            ctx.pattern_env.collect(|pattern_env| {
                let mut inner_ctx =
                    TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.rhs);
                let (debruijn_index, expr, _) = self.inner.as_ref();
                match other.subof(expr.as_ref_dispatcher(), &mut inner_ctx)? {
                    ThreeValuedLogic::True => {
                        pattern_env.push((*debruijn_index, other.clone_data()));
                        Ok(ThreeValuedLogic::True)
                    }
                    ThreeValuedLogic::False => Ok(ThreeValuedLogic::False),
                    ThreeValuedLogic::Unknown => Ok(ThreeValuedLogic::Unknown),
                }
            })
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Pattern<T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new<X: AsDispatcher<Type<T>, T>>(debruijn_index: usize, expr: X) -> Type<T> {
        let expr = expr.into_dispatcher();
        let is_nf = expr.is_normal_form();
        Self {
            inner: ArcOpt::new((debruijn_index, expr, RwLock::new(is_nf))),
        }
        .dispatch()
    }
    pub fn debruijn_index(&self) -> usize {
        self.inner.as_ref().0
    }

    pub fn expr(&self) -> &Type<T> {
        &self.inner.as_ref().1
    }
}
