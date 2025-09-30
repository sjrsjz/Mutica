use std::sync::Arc;

use arc_gc::{gc::GC, traceable::GCTraceable};

use crate::{
    types::{
        AsTypeRef, CoinductiveType, CoinductiveTypeWithAny, Representable, Rootable,
        StabilizedType, TaggedPtr, Type, TypeError,
        closure::{ClosureEnv, ParamEnv},
        fixpoint::FixPointInner,
        type_bound::TypeBound,
    },
    util::{collector::Collector, cycle_detector::FastCycleDetector},
};

#[derive(Clone)]
pub struct Effect {
    pair: Arc<(Type, Type)>,
}

impl GCTraceable<FixPointInner> for Effect {
    fn collect(
        &self,
        queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<FixPointInner>>,
    ) {
        self.pair.0.collect(queue);
        self.pair.1.collect(queue);
    }
}

impl Rootable for Effect {
    fn upgrade(&self, collected: &mut smallvec::SmallVec<[arc_gc::arc::GCArc<FixPointInner>; 8]>) {
        self.pair.0.upgrade(collected);
        self.pair.1.upgrade(collected);
    }
}

impl CoinductiveType<Type, StabilizedType> for Effect {
    fn dispatch(self) -> Type {
        Type::Effect(self)
    }

    fn is(
        &self,
        other: &Type,
        assumptions: &mut smallvec::SmallVec<[(TaggedPtr<()>, TaggedPtr<()>); 8]>,
        closure_env: (&ClosureEnv, &ClosureEnv),
        pattern_env: &mut Collector<(usize, Type)>,
        pattern_mode: bool,
    ) -> Result<Option<()>, super::TypeError> {
        pattern_env.collect(|pattern_env| match other {
            Type::Bound(TypeBound::Top) => Ok(Some(())),
            Type::Effect(v) => {
                let payload_match = self.pair.0.is(
                    &v.pair.0,
                    assumptions,
                    closure_env,
                    pattern_env,
                    pattern_mode,
                )?;
                if payload_match.is_none() {
                    return Ok(None);
                }
                let then_match = self.pair.1.is(
                    &v.pair.1,
                    assumptions,
                    closure_env,
                    pattern_env,
                    pattern_mode,
                )?;
                if then_match.is_none() {
                    return Ok(None);
                }
                Ok(Some(()))
            }
            Type::Specialize(v) => v.has(self, assumptions, closure_env, pattern_env, pattern_mode),
            Type::Generalize(v) => v.has(self, assumptions, closure_env, pattern_env, pattern_mode),
            Type::FixPoint(v) => v.has(self, assumptions, closure_env, pattern_env, pattern_mode),
            Type::Pattern(v) => v.has(self, assumptions, closure_env, pattern_env, pattern_mode),
            Type::Variable(v) => v.has(self, assumptions, closure_env, pattern_env, pattern_mode),
            _ => Ok(None),
        })
    }

    fn reduce(
        &self,
        v: &ClosureEnv,
        p: &ParamEnv,
        rec_assumptions: &mut smallvec::SmallVec<[(TaggedPtr<()>, Type, bool); 8]>,
        gc: &mut GC<FixPointInner>,
    ) -> Result<StabilizedType, TypeError> {
        let payload = self.pair.0.reduce(v, p, rec_assumptions, gc)?;
        let then = self.pair.1.reduce(v, p, rec_assumptions, gc)?;
        Ok(Self::new(payload, then))
    }

    fn apply(
        &self,
        v: &Type,
        context: &ClosureEnv,
        p: &ParamEnv,
        rec_assumptions: &mut smallvec::SmallVec<[(TaggedPtr<()>, Type, bool); 8]>,
        gc: &mut GC<FixPointInner>,
    ) -> Result<StabilizedType, TypeError> {
        self.pair.1.apply(v, context, p, rec_assumptions, gc)
    }
}

impl Representable for Effect {
    fn represent(&self, path: &mut FastCycleDetector<*const ()>) -> String {
        format!(
            "Effect<{}, {}>",
            self.pair.0.represent(path),
            self.pair.1.represent(path)
        )
    }
}

impl Effect {
    pub fn new<U: AsTypeRef, V: AsTypeRef>(payload: U, then: V) -> StabilizedType {
        Self {
            pair: Arc::new((payload.as_type_ref().clone(), then.as_type_ref().clone())),
        }
        .dispatch()
        .stabilize()
    }

    pub fn payload(&self) -> &Type {
        &self.pair.0
    }

    pub fn then(&self) -> &Type {
        &self.pair.1
    }
}
