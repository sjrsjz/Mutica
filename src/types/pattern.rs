use std::sync::Arc;

use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        AsTypeRef, CoinductiveType, CoinductiveTypeWithAny, Representable, Rootable,
        StabilizedType, TaggedPtr, Type, TypeError,
        closure::{ClosureEnv, ParamEnv},
        fixpoint::FixPointInner,
    },
    util::collector::Collector,
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
    fn upgrade(&self, collected: &mut smallvec::SmallVec<[arc_gc::arc::GCArc<FixPointInner>; 8]>) {
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

impl CoinductiveType<Type, StabilizedType> for Pattern {
    fn dispatch(self) -> Type {
        Type::Pattern(self)
    }

    fn is(
        &self,
        other: &Type,
        assumptions: &mut smallvec::SmallVec<[(super::TaggedPtr<()>, super::TaggedPtr<()>); 8]>,
        closure_env: (&ClosureEnv, &ClosureEnv),
        pattern_env: &mut Collector<(usize, Type)>,
        pattern_mode: bool,
    ) -> Result<Option<()>, TypeError> {
        pattern_env.collect(|pattern_env| {
            if !pattern_mode {
                // 由于Pattern的特殊性，Pattern只能和Pattern进行比较，否则可能破坏alpha等价性
                return match other {
                    Type::Pattern(v) => {
                        if self.debruijn_index == v.debruijn_index {
                            self.expr.is(
                                &v.expr,
                                assumptions,
                                closure_env,
                                pattern_env,
                                pattern_mode,
                            )
                        } else {
                            Ok(None)
                        }
                    }
                    _ => Ok(None),
                };
            }
            self.expr
                .is(other, assumptions, closure_env, pattern_env, pattern_mode)
        })
    }

    fn apply(
        &self,
        v: &Type,
        context: &ClosureEnv,
        p: &ParamEnv,
        rec_assumptions: &mut smallvec::SmallVec<[(TaggedPtr<()>, Type, bool); 8]>,
        gc: &mut arc_gc::gc::GC<FixPointInner>,
    ) -> Result<StabilizedType, TypeError> {
        self.expr.apply(v, context, p, rec_assumptions, gc)
    }

    fn reduce(
        &self,
        v: &ClosureEnv,
        p: &ParamEnv,
        rec_assumptions: &mut smallvec::SmallVec<[(TaggedPtr<()>, Type, bool); 8]>,
        gc: &mut arc_gc::gc::GC<FixPointInner>,
    ) -> Result<StabilizedType, TypeError> {
        self.expr.reduce(v, p, rec_assumptions, gc)
    }

    fn tagged_ptr(&self) -> super::TaggedPtr<()> {
        super::TaggedPtr::new_unique(&self as *const _ as *const ())
    }
}

impl CoinductiveTypeWithAny<Type, StabilizedType> for Pattern {
    fn has<V: CoinductiveType<Type, StabilizedType> + Clone>(
        &self,
        other: &V,
        assumptions: &mut smallvec::SmallVec<[(super::TaggedPtr<()>, super::TaggedPtr<()>); 8]>,
        closure_env: (&ClosureEnv, &ClosureEnv),
        pattern_env: &mut Collector<(usize, Type)>,
        pattern_mode: bool,
    ) -> Result<Option<()>, TypeError> {
        pattern_env.collect(|pattern_env| {
            if !pattern_mode {
                // 由于调用has的other一定不是Pattern类型
                return Ok(None);
            }
            if other
                .is(
                    &self.expr,
                    assumptions,
                    closure_env,
                    pattern_env,
                    pattern_mode,
                )?
                .is_some()
            {
                pattern_env.push((self.debruijn_index, other.clone().dispatch()));
                Ok(Some(()))
            } else {
                Ok(None)
            }
        })
    }
}

impl Pattern {
    pub fn new<T: AsTypeRef>(debruijn_index: usize, expr: T) -> StabilizedType {
        Self {
            debruijn_index,
            expr: Arc::new(expr.as_type_ref().clone()),
        }
        .dispatch()
        .stabilize()
    }
}
