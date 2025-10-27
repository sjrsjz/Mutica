use std::sync::{Arc, RwLock};

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, TaggedPtr, Type, TypeCheckContext, TypeError,
        TypeRef,
    },
    util::{cycle_detector::FastCycleDetector, three_valued_logic::ThreeValuedLogic},
};

// 理论上来说应当把 debruijn_index 直接和 Type 绑定起来（因为Pattern只是一个附加信息）
// 但是为了实现的简洁性，这里就先分开了
pub struct Pattern<T: GcAllocObject<T, Inner = Type<T>>> {
    is_nf: Arc<RwLock<ThreeValuedLogic>>,
    debruijn_index: usize,
    expr: Arc<Type<T>>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Pattern<T> {
    fn clone(&self) -> Self {
        Self {
            is_nf: self.is_nf.clone(),
            debruijn_index: self.debruijn_index,
            expr: self.expr.clone(),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Pattern<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        self.expr.collect(queue);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Pattern<T> {
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        self.expr.upgrade(collected);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Pattern<T> {
    fn represent(
        &self,
        path: &mut crate::util::cycle_detector::FastCycleDetector<TaggedPtr<()>>,
    ) -> String {
        format!("λ.{} : {}", self.debruijn_index, self.expr.represent(path))
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
    fn fulfill(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<bool, TypeError<Type<T>, T>> {
        if ctx.rhs {
            ctx.pattern_env.collect(|pattern_env| {
                let mut inner_ctx =
                    TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.rhs);
                if self.expr.fulfill(other, &mut inner_ctx)? {
                    pattern_env.push((self.debruijn_index, other.clone_data()));
                    Ok(true)
                } else {
                    Ok(false)
                }
            })
        } else {
            self.expr.fulfill(other, ctx)
        }
    }

    fn invoke(
        &self,
        _ctx: &mut InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.clone().dispatch().into()))
    }

    fn reduce(
        self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Ok(Self::new(
            self.debruijn_index,
            self.expr.as_ref().clone().reduce(ctx)?,
        ))
    }

    fn tagged_ptr(&self) -> super::TaggedPtr<()> {
        super::TaggedPtr::new_unique(&self as *const _ as *const ())
    }

    fn is_normal_form(&self) -> ThreeValuedLogic {
        match self.is_nf.read() {
            Ok(v) => *v,
            Err(_) => ThreeValuedLogic::False,
        }
    }

    fn recalculate_normal_form(&self, cycle_detector: &mut FastCycleDetector<TaggedPtr<()>>) {
        self.expr.recalculate_normal_form(cycle_detector);
        let new_nf = self.expr.is_normal_form();
        if let Ok(mut nf_lock) = self.is_nf.write() {
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
    ) -> Result<bool, TypeError<Type<T>, T>> {
        if ctx.rhs {
            other.fullfill(self.expr.as_ref_dispatcher(), ctx)
        } else {
            ctx.pattern_env.collect(|pattern_env| {
                let mut inner_ctx =
                    TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.rhs);
                if other.fullfill(self.expr.as_ref_dispatcher(), &mut inner_ctx)? {
                    pattern_env.push((self.debruijn_index, other.clone_data()));
                    Ok(true)
                } else {
                    Ok(false)
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
            is_nf: Arc::new(RwLock::new(is_nf)),
            debruijn_index,
            expr: Arc::new(expr),
        }
        .dispatch()
    }
    pub fn debruijn_index(&self) -> usize {
        self.debruijn_index
    }

    pub fn expr(&self) -> &Type<T> {
        &self.expr
    }
}
