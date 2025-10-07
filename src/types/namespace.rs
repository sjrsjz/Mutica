use std::sync::Arc;

use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        AsTypeRef, CoinductiveType, CoinductiveTypeWithAny, Representable, TypeCheckContext, ReductionContext, InvokeContext, Rootable,
        StabilizedType, Type, TypeError,
        fixpoint::FixPointInner,
        type_bound::TypeBound,
    },
    util::cycle_detector::FastCycleDetector,
};

#[derive(Clone)]
pub struct Namespace {
    tag: String,
    expr: Arc<Type>,
}
impl GCTraceable<FixPointInner> for Namespace {
    fn collect(
        &self,
        queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<FixPointInner>>,
    ) {
        self.expr.collect(queue);
    }
}

impl Rootable for Namespace {
    fn upgrade(&self, collected: &mut smallvec::SmallVec<[arc_gc::arc::GCArc<FixPointInner>; 8]>) {
        self.expr.upgrade(collected);
    }
}

impl Representable for Namespace {
    fn represent(&self, path: &mut FastCycleDetector<*const ()>) -> String {
        format!("{}::{}", self.tag, self.expr.represent(path))
    }
}

impl CoinductiveType<Type, StabilizedType> for Namespace {
    fn dispatch(self) -> Type {
        Type::Namespace(self)
    }

    fn is(&self, other: &Type, ctx: &mut TypeCheckContext) -> Result<Option<()>, TypeError> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.pattern_mode);
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

    fn reduce(&self, ctx: &mut ReductionContext) -> Result<StabilizedType, TypeError> {
        Ok(Self::new(&self.tag, self.expr.reduce(ctx)?))
    }

    fn invoke(&self, ctx: &mut InvokeContext) -> Result<StabilizedType, TypeError> {
        self.expr.invoke(ctx)
    }
}

impl Namespace {
    pub fn new<T: Into<String>, I: AsTypeRef>(tag: T, expr: I) -> StabilizedType {
        Self {
            tag: tag.into(),
            expr: Arc::new(expr.into_type()),
        }
        .dispatch()
        .stabilize()
    }

    pub fn expr(&self) -> &Type {
        &self.expr
    }

    pub fn tag(&self) -> &str {
        &self.tag
    }
}
