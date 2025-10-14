use crate::{
    types::{
        CoinductiveType, CoinductiveTypeWithAny, InvokeContext, ReductionContext, Representable,
        Rootable, Type, TypeCheckContext, TypeError, fixpoint::FixPointInner,
        type_bound::TypeBound,
    },
    util::cycle_detector::FastCycleDetector,
};
use arc_gc::traceable::GCTraceable;

#[derive(Clone)]
pub enum Variable {
    Deburijn(isize),
    Continuation,
}

impl GCTraceable<FixPointInner> for Variable {
    fn collect(
        &self,
        _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<FixPointInner>>,
    ) {
    }
}

impl Rootable for Variable {}

impl CoinductiveType<Type> for Variable {
    fn dispatch(self) -> Type {
        Type::Variable(self)
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
                Type::Bound(TypeBound::Top) => Ok(Some(())),
                Type::Generalize(v) => v.has(self, &mut inner_ctx),
                Type::Specialize(v) => v.has(self, &mut inner_ctx),
                Type::FixPoint(v) => v.has(self, &mut inner_ctx),
                Type::Variable(v) => {
                    match (self, v) {
                        (Variable::Deburijn(self_idx), Variable::Deburijn(v_idx)) => {
                            if *self_idx >= 0 || *v_idx >= 0 {
                                return Ok(if self_idx == v_idx { Some(()) } else { None });
                            }
                            // 如果都是负数,说明都是闭包内的变量
                            // 需要从闭包环境中取出对应的类型进行比较
                            let l = (-1 - *self_idx) as usize;
                            let r = (-1 - *v_idx) as usize;

                            let value_l = ctx.closure_env.0.get(l)?;
                            let value_r = ctx.closure_env.1.get(r)?;
                            value_l.is(value_r, &mut inner_ctx)
                        }
                        (Variable::Continuation, Variable::Continuation) => Ok(Some(())), // κ 只能和 κ 匹配
                        _ => Ok(None),
                    }
                }
                Type::Pattern(v) => v.has(self, &mut inner_ctx),
                _ => match self {
                    Variable::Deburijn(idx) if *idx >= 0 => Ok(None),
                    Variable::Deburijn(idx) => {
                        let l = (-1 - *idx) as usize;
                        let value = ctx.closure_env.0.get(l)?;
                        value.is(other, &mut inner_ctx)
                    }
                    Variable::Continuation => Ok(None),
                },
            }
        })
    }

    fn reduce(&self, ctx: &mut ReductionContext) -> Result<Type, TypeError> {
        match self {
            Variable::Deburijn(idx) if *idx >= 0 => Ok(ctx
                .param_env
                .get(*idx as usize)
                .map(|t| t.clone())
                .unwrap_or(TypeBound::bottom())),
            Variable::Deburijn(idx) => ctx
                .closure_env
                .get((-1 - *idx) as usize)
                .map(|t| t.clone()),
            Variable::Continuation => match ctx.continuation {
                Some(t) => Ok(t.clone()),
                None => Err(TypeError::MissingContinuation),
            },
        }
    }

    fn invoke(&self, _ctx: &mut InvokeContext) -> Result<Type, TypeError> {
        Err(TypeError::NonApplicableType(
            self.clone().dispatch().into(),
        ))
    }
}

impl CoinductiveTypeWithAny<Type> for Variable {
    fn has<V: CoinductiveType<Type>>(
        &self,
        other: &V,
        ctx: &mut TypeCheckContext,
    ) -> Result<Option<()>, TypeError> {
        ctx.pattern_env.collect(|pattern_env| match self {
            Variable::Deburijn(idx) if *idx >= 0 => Ok(None),
            Variable::Deburijn(idx) => {
                let r = (-1 - *idx) as usize;
                let value = ctx.closure_env.1.get(r)?;
                let mut inner_ctx = TypeCheckContext::new(
                    ctx.assumptions,
                    ctx.closure_env,
                    pattern_env,
                    ctx.pattern_mode,
                );
                other.is(value, &mut inner_ctx)
            }
            Variable::Continuation => Ok(None),
        })
    }
}

impl Representable for Variable {
    fn represent(&self, _path: &mut FastCycleDetector<*const ()>) -> String {
        match self {
            Variable::Deburijn(index) => format!("λ.{}", index),
            Variable::Continuation => "κ".to_string(),
        }
    }
}

impl Variable {
    pub fn new_deburijn(debruijn_index: isize) -> Type {
        Self::Deburijn(debruijn_index).dispatch()
    }
    pub fn new_continuation() -> Type {
        Self::Continuation.dispatch()
    }
}
