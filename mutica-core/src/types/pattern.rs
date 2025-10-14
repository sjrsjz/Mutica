use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::types::{
    AsType, CoinductiveType, CoinductiveTypeWithAny, InvokeContext, ReductionContext,
    Representable, Rootable, Type, TypeCheckContext, TypeError, fixpoint::FixPointInner,
};

#[derive(Clone)]
// 理论上来说应当把 debruijn_index 直接和 Type 绑定起来（因为Pattern只是一个附加信息）
// 但是为了实现的简洁性，这里就先分开了
pub struct Pattern {
    debruijn_index: usize,
    expr: Arc<Type>,
}

impl GCTraceable<FixPointInner> for Pattern {
    fn collect(
        &self,
        queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<FixPointInner>>,
    ) {
        self.expr.collect(queue);
    }
}

impl Rootable for Pattern {
    fn upgrade(&self, collected: &mut Vec<GCArc<FixPointInner>>) {
        self.expr.upgrade(collected);
    }
}

impl Representable for Pattern {
    fn represent(
        &self,
        path: &mut crate::util::cycle_detector::FastCycleDetector<*const ()>,
    ) -> String {
        format!("λ.{} : {}", self.debruijn_index, self.expr.represent(path))
    }
}

impl CoinductiveType<Type> for Pattern {
    fn dispatch(self) -> Type {
        Type::Pattern(self)
    }

    fn is(&self, other: &Type, ctx: &mut TypeCheckContext) -> Result<Option<()>, TypeError> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.assumptions,
                ctx.closure_env,
                pattern_env,
                ctx.pattern_mode,
            );
            if !ctx.pattern_mode {
                // 由于Pattern的特殊性，Pattern只能和Pattern进行比较，否则可能破坏alpha等价性
                return match other {
                    Type::Pattern(v) => {
                        if self.debruijn_index == v.debruijn_index {
                            self.expr.is(&v.expr, &mut inner_ctx)
                        } else {
                            Ok(None)
                        }
                    }
                    _ => Ok(None),
                };
            }
            self.expr.is(other, &mut inner_ctx)
        })
    }

    fn invoke(&self, _ctx: &mut InvokeContext) -> Result<Type, TypeError> {
        Err(TypeError::NonApplicableType(self.clone().dispatch().into()))
    }

    fn reduce(&self, ctx: &mut ReductionContext) -> Result<Type, TypeError> {
        Ok(Self::new(self.debruijn_index, self.expr.reduce(ctx)?))
    }

    fn tagged_ptr(&self) -> super::TaggedPtr<()> {
        super::TaggedPtr::new_unique(&self as *const _ as *const ())
    }
}

impl CoinductiveTypeWithAny<Type> for Pattern {
    fn has<V: CoinductiveType<Type> + Clone>(
        &self,
        other: &V,
        ctx: &mut TypeCheckContext,
    ) -> Result<Option<()>, TypeError> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.assumptions,
                ctx.closure_env,
                pattern_env,
                ctx.pattern_mode,
            );
            if !ctx.pattern_mode {
                // 由于调用has的other一定不是Pattern类型
                return Ok(None);
            }
            if other.is(&self.expr, &mut inner_ctx)?.is_some() {
                pattern_env.push((self.debruijn_index, other.clone().dispatch()));
                Ok(Some(()))
            } else {
                Ok(None)
            }
        })
    }
}

impl Pattern {
    pub fn new<T: AsType>(debruijn_index: usize, expr: T) -> Type {
        Self {
            debruijn_index,
            expr: Arc::new(expr.into_type()),
        }
        .dispatch()
    }
}
