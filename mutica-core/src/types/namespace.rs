use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::{
    types::{
        AsType, CoinductiveType, CoinductiveTypeWithAny, InvokeContext, ReductionContext,
        Representable, Rootable, Type, TypeCheckContext, TypeError, fixpoint::FixPointInner,
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
    fn upgrade<'roots>(&self, collected: &'roots mut Vec<GCArc<FixPointInner>>) {
        self.expr.upgrade(collected);
    }
}

impl Representable for Namespace {
    fn represent(&self, path: &mut FastCycleDetector<*const ()>) -> String {
        format!("{}::{}", self.tag, self.expr.represent(path))
    }
}

impl CoinductiveType<Type> for Namespace {
    fn dispatch(self) -> Type {
        Type::Namespace(self)
    }

    fn is(&self, other: &Type, ctx: &mut TypeCheckContext) -> Result<Option<()>, TypeError> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.assumptions,
                ctx.closure_env,
                pattern_env,
                ctx.pattern_mode,
            );
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

    fn reduce(&self, ctx: &mut ReductionContext) -> Result<Type, TypeError> {
        Ok(Self::new(&self.tag, self.expr.reduce(ctx)?))
    }

    fn invoke(&self, ctx: &mut InvokeContext) -> Result<Type, TypeError> {
        self.expr.invoke(ctx)
    }
}

impl Namespace {
    pub fn new<T: Into<String>, I: AsType>(tag: T, expr: I) -> Type {
        Self {
            tag: tag.into(),
            expr: Arc::new(expr.into_type()),
        }
        .dispatch()
    }

    pub fn expr(&self) -> &Type {
        &self.expr
    }

    pub fn tag(&self) -> &str {
        &self.tag
    }
}
