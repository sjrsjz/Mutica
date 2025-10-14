use crate::{
    types::{
        CoinductiveType, CoinductiveTypeWithAny, InvokeContext, ReductionContext, Representable,
        Rootable, Type, TypeCheckContext, TypeEnum, TypeError, fixpoint::FixPointInner,
        type_bound::TypeBound,
    },
    util::cycle_detector::FastCycleDetector,
};
use arc_gc::traceable::GCTraceable;

#[derive(Clone)]
pub struct Variable {
    index: isize,
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
    // Variable 永远不能是 NF 类型，因为 reduce 随时能把它替换掉
    fn dispatch(self) -> Type {
        Type::new(TypeEnum::Variable(self))
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
                TypeEnum::Bound(TypeBound::Top) => Ok(Some(())),
                TypeEnum::Generalize(v) => v.has(self, &mut inner_ctx),
                TypeEnum::Specialize(v) => v.has(self, &mut inner_ctx),
                TypeEnum::FixPoint(v) => v.has(self, &mut inner_ctx),
                TypeEnum::Variable(v) => {
                    let self_idx = self.index;
                    let v_idx = v.index;
                    if self_idx >= 0 || v_idx >= 0 {
                        return Ok(if self_idx == v_idx { Some(()) } else { None });
                    }
                    // 如果都是负数,说明都是闭包内的变量
                    // 需要从闭包环境中取出对应的类型进行比较
                    let l = (-1 - self_idx) as usize;
                    let r = (-1 - v_idx) as usize;

                    let value_l = ctx.closure_env.0.get(l)?;
                    let value_r = ctx.closure_env.1.get(r)?;
                    value_l.is(value_r, &mut inner_ctx)
                }
                TypeEnum::Pattern(v) => v.has(self, &mut inner_ctx),
                _ => {
                    if self.index >= 0 {
                        // 如果是正数,说明是全局变量,无法确定类型
                        return Ok(None);
                    }
                    let r = (-1 - self.index) as usize;
                    let value = ctx.closure_env.1.get(r)?;
                    value.is(other, &mut inner_ctx)
                }
            }
        })
    }

    fn reduce(self, ctx: &mut ReductionContext) -> Result<Type, TypeError> {
        let idx = self.index;
        if idx >= 0 {
            Ok(ctx
                .param_env
                .get(idx as usize)
                .map(|t| t.clone())
                .unwrap_or(TypeBound::bottom()))
        } else {
            ctx.closure_env.get((-1 - idx) as usize).map(|t| t.clone())
        }
    }

    fn invoke(&self, _ctx: &mut InvokeContext) -> Result<Type, TypeError> {
        Err(TypeError::NonApplicableType(self.clone().dispatch().into()))
    }
}

impl CoinductiveTypeWithAny<Type> for Variable {
    fn has<V: CoinductiveType<Type>>(
        &self,
        other: &V,
        ctx: &mut TypeCheckContext,
    ) -> Result<Option<()>, TypeError> {
        ctx.pattern_env.collect(|pattern_env| {
            if self.index >= 0 {
                Ok(None)
            } else {
                let r = (-1 - self.index) as usize;
                let value = ctx.closure_env.1.get(r)?;
                let mut inner_ctx = TypeCheckContext::new(
                    ctx.assumptions,
                    ctx.closure_env,
                    pattern_env,
                    ctx.pattern_mode,
                );
                other.is(value, &mut inner_ctx)
            }
        })
    }
}

impl Representable for Variable {
    fn represent(&self, _path: &mut FastCycleDetector<*const ()>) -> String {
        format!("λ.{}", self.index)
    }
}

impl Variable {
    pub fn new_deburijn(debruijn_index: isize) -> Type {
        Variable {
            index: debruijn_index,
        }
        .dispatch()
    }
}
