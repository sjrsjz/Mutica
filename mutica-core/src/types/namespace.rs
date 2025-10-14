use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::{
    types::{
        AsType, CoinductiveType, CoinductiveTypeWithAny, InvokeContext, ReductionContext,
        Representable, Rootable, Type, TypeCheckContext, TypeEnum, TypeError,
        fixpoint::FixPointInner, type_bound::TypeBound,
    },
    util::cycle_detector::FastCycleDetector,
};

#[derive(Clone)]
pub struct Namespace {
    tag: Box<String>,
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
        Type::new(TypeEnum::Namespace(self))
    }

    fn is(&self, other: &Type, ctx: &mut TypeCheckContext) -> Result<Option<()>, TypeError> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.assumptions,
                ctx.closure_env,
                pattern_env,
                ctx.pattern_mode,
            );
            match &other.ty {
                TypeEnum::Namespace(v) => {
                    if self.tag == v.tag {
                        self.expr.is(&v.expr, &mut inner_ctx)
                    } else {
                        Ok(None)
                    }
                }
                TypeEnum::Bound(TypeBound::Top) => Ok(Some(())),
                TypeEnum::Generalize(v) => v.has(self, &mut inner_ctx),
                TypeEnum::Specialize(v) => v.has(self, &mut inner_ctx),
                TypeEnum::FixPoint(v) => v.has(self, &mut inner_ctx),
                TypeEnum::Pattern(v) => v.has(self, &mut inner_ctx),
                TypeEnum::Variable(v) => v.has(self, &mut inner_ctx),
                _ => Ok(None),
            }
        })
    }

    fn reduce(self, ctx: &mut ReductionContext) -> Result<Type, TypeError> {
        let expr = self.expr.as_ref().clone().reduce(ctx)?;
        Ok(Self::new(self.tag, expr))
    }

    fn invoke(&self, ctx: &mut InvokeContext) -> Result<Type, TypeError> {
        self.expr.invoke(ctx)
    }
}

impl Namespace {
    pub fn new<T: Into<Box<String>>, I: AsType>(tag: T, expr: I) -> Type {
        let expr = expr.into_type();
        let is_nf = expr.is_nf();
        let ty = Self {
            tag: tag.into(),
            expr: Arc::new(expr),
        };
        if is_nf {
            ty.dispatch_nf()
        } else {
            ty.dispatch()
        }
    }
    fn dispatch_nf(self) -> Type {
        Type::new_nf(TypeEnum::Namespace(self))
    }

    pub fn expr(&self) -> &Type {
        &self.expr
    }

    pub fn tag(&self) -> &str {
        &self.tag
    }
}
