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

pub struct Namespace<T: GcAllocObject<T>> {
    is_nf: bool,
    tag: Arc<String>,
    expr: Arc<Type<T>>,
}

impl<T: GcAllocObject<T>> Clone for Namespace<T> {
    fn clone(&self) -> Self {
        Self {
            is_nf: self.is_nf,
            tag: self.tag.clone(),
            expr: self.expr.clone(),
        }
    }
}

impl<T: GcAllocObject<T>> GCTraceable<T> for Namespace<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        self.expr.collect(queue);
    }
}

impl<T: GcAllocObject<T>> GcAllocObject<T> for Namespace<T> {}

impl<T: GcAllocObject<T>> Rootable<T> for Namespace<T> {
    fn upgrade<'roots>(&self, collected: &'roots mut Vec<GCArc<T>>) {
        self.expr.upgrade(collected);
    }
}

impl<T: GcAllocObject<T>> Representable for Namespace<T> {
    fn represent(&self, path: &mut FastCycleDetector<*const ()>) -> String {
        format!("{}::{}", self.tag, self.expr.represent(path))
    }
}

impl<T: GcAllocObject<T>> AsDispatcher<Type<T>, T> for Namespace<T> {
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

impl<T: GcAllocObject<T>> CoinductiveType<Type<T>, T> for Namespace<T> {
    fn is(
        &self,
        other: &Type<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<Option<()>, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env);
            match other {
                Type::Namespace(v) => {
                    if self.tag == v.tag {
                        self.expr.is(&v.expr, &mut inner_ctx)
                    } else {
                        Ok(None)
                    }
                }
                Type::Bound(TypeBound::Top) => Ok(Some(())),
                Type::Generalize(v) => v.has(self, &mut inner_ctx),
                Type::Specialize(v) => v.has(self, &mut inner_ctx),
                Type::FixPoint(v) => v.has(self, &mut inner_ctx),
                Type::Pattern(v) => v.has(self, &mut inner_ctx),
                Type::Variable(v) => v.has(self, &mut inner_ctx),
                _ => Ok(None),
            }
        })
    }

    fn reduce(
        self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        let expr = self.expr.as_ref().clone().reduce(ctx)?;
        Ok(Self::new(self.tag, expr))
    }

    fn invoke(
        &self,
        ctx: &mut InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        self.expr.invoke(ctx)
    }

    fn is_normal_form(&self) -> bool {
        self.is_nf
    }
}

impl<T: GcAllocObject<T>> Namespace<T> {
    pub fn new<I: AsDispatcher<Type<T>, T>, S: Into<Arc<String>>>(tag: S, expr: I) -> Type<T> {
        let expr = expr.into_dispatcher();
        let is_nf = expr.is_normal_form();
        Self {
            is_nf,
            tag: tag.into(),
            expr: Arc::new(expr),
        }
        .dispatch()
    }

    pub fn expr(&self) -> &Type<T> {
        &self.expr
    }

    pub fn tag(&self) -> &str {
        &self.tag
    }
}
