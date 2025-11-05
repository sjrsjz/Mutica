use std::sync::{Arc, RwLock};

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, TaggedPtr, Type, TypeCheckContext, TypeError,
        TypeRef, type_bound::TypeBound,
    },
    util::{
        arc_opt::ArcOpt, cycle_detector::FastCycleDetector, three_valued_logic::ThreeValuedLogic,
    },
};

pub struct Namespace<T: GcAllocObject<T, Inner = Type<T>>> {
    inner: ArcOpt<(Arc<str>, Type<T>, RwLock<ThreeValuedLogic>)>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Namespace<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Namespace<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        let (_, expr, _) = self.inner.as_ref();
        expr.collect(queue);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Namespace<T> {
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        let (_, expr, _) = self.inner.as_ref();
        expr.upgrade(collected);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Namespace<T> {
    fn represent(&self, path: &mut FastCycleDetector<TaggedPtr<()>>) -> String {
        let (tag, expr, _) = self.inner.as_ref();
        format!("{}::{}", tag, expr.represent(path))
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for Namespace<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;

    fn into_dispatcher(self) -> Type<T> {
        Type::Namespace(self)
    }

    fn as_ref_dispatcher<'a>(&'a self) -> Self::RefDispatcher<'a> {
        TypeRef::Namespace(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for Namespace<T> {
    fn check(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<bool, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.rhs);
            match other {
                TypeRef::Generalize(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Specialize(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::EqType(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Bound(TypeBound::Top) => Ok(true),
                TypeRef::Namespace(v) => {
                    let (self_tag, self_expr, _) = self.inner.as_ref();
                    let (v_tag, v_expr, _) = v.inner.as_ref();
                    if self_tag == v_tag {
                        self_expr.check(v_expr.as_ref_dispatcher(), &mut inner_ctx)
                    } else {
                        Ok(false)
                    }
                }
                _ => Ok(false),
            }
        })
    }

    fn equals(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut super::TypeCheckContext<Type<T>, T>,
    ) -> Result<bool, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.rhs);
            match other {
                TypeRef::FixPoint(v) => v.equals_any(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.equals_any(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.equals_any(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Namespace(v) => {
                    let (self_tag, self_expr, _) = self.inner.as_ref();
                    let (v_tag, v_expr, _) = v.inner.as_ref();
                    if self_tag == v_tag {
                        self_expr.equals(v_expr.as_ref_dispatcher(), &mut inner_ctx)
                    } else {
                        Ok(false)
                    }
                }
                _ => Ok(false),
            }
        })
    }

    fn reduce(
        mut self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        match self.inner.modify(|(tag, expr, is_nf)| {
            let new_expr = expr.reduce(ctx)?;
            let new_is_nf = new_expr.is_normal_form();
            if let Ok(mut nf_lock) = is_nf.write() {
                *nf_lock = new_is_nf;
            }
            Ok((tag, new_expr, is_nf))
        })? {
            Some(()) => Ok(self.dispatch()),
            None => {
                let (tag, expr, _) = self.inner.as_ref();
                let new_expr = expr.clone().reduce(ctx)?;
                Ok(Self::new(tag.clone(), new_expr))
            }
        }
    }

    fn invoke(self, ctx: InvokeContext<Type<T>, T>) -> Result<Type<T>, TypeError<Type<T>, T>> {
        match self.inner.take() {
            Ok((_, expr, _)) => expr.invoke(ctx),
            Err(v) => {
                let (_, expr, _) = v.as_ref();
                expr.clone().invoke(ctx)
            }
        }
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

impl<T: GcAllocObject<T, Inner = Type<T>>> Namespace<T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new<I: AsDispatcher<Type<T>, T>, S: Into<Arc<str>>>(tag: S, expr: I) -> Type<T> {
        let tag = tag.into();
        let expr = expr.into_dispatcher();
        let is_nf = expr.is_normal_form();
        Self {
            inner: ArcOpt::new((tag, expr, RwLock::new(is_nf))),
        }
        .dispatch()
    }

    pub fn expr(&self) -> &Type<T> {
        &self.inner.as_ref().1
    }

    pub fn tag(&self) -> &str {
        &self.inner.as_ref().0
    }
}
