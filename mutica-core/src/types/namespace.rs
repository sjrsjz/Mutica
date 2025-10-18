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

pub struct Namespace<T: GcAllocObject<T, Inner = Type<T>>> {
    is_nf: bool,
    tag: Arc<String>,
    expr: Arc<Type<T>>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Namespace<T> {
    fn clone(&self) -> Self {
        Self {
            is_nf: self.is_nf,
            tag: self.tag.clone(),
            expr: self.expr.clone(),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Namespace<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        self.expr.collect(queue);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GcAllocObject<T> for Namespace<T> {
    type Inner = Type<T>;
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Namespace<T> {
    fn upgrade<'roots>(&self, collected: &'roots mut Vec<GCArc<T>>) {
        self.expr.upgrade(collected);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Namespace<T> {
    fn represent(&self, path: &mut FastCycleDetector<*const ()>) -> String {
        format!("{}::{}", self.tag, self.expr.represent(path))
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
    fn fulfill(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<Option<()>, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env);
            match other {
                TypeRef::Generalize(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Specialize(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Bound(TypeBound::Top) => Ok(Some(())),
                TypeRef::Namespace(v) => {
                    if self.tag == v.tag {
                        self.expr.fulfill(v.expr.as_ref_dispatcher(), &mut inner_ctx)
                    } else {
                        Ok(None)
                    }
                }
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

impl<T: GcAllocObject<T, Inner = Type<T>>> Namespace<T> {
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
