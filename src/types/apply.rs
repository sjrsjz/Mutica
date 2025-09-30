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
pub struct Apply {
    pair: Arc<(Type, Type)>,
}

impl GCTraceable<FixPointInner> for Apply {
    fn collect(
        &self,
        queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<FixPointInner>>,
    ) {
        self.pair.0.collect(queue);
        self.pair.1.collect(queue);
    }
}

impl Rootable for Apply {
    fn upgrade(&self, collected: &mut smallvec::SmallVec<[arc_gc::arc::GCArc<FixPointInner>; 8]>) {
        self.pair.0.upgrade(collected);
        self.pair.1.upgrade(collected);
    }
}

impl CoinductiveType<Type, StabilizedType> for Apply {
    fn dispatch(self) -> Type {
        Type::Apply(self)
    }

    fn is(
        &self,
        other: &Type,
        assumptions: &mut smallvec::SmallVec<[(TaggedPtr<()>, TaggedPtr<()>); 8]>,
        closure_env: (&ClosureEnv, &ClosureEnv),
        pattern_env: &mut Collector<(usize, Type)>,
        pattern_mode: bool,
    ) -> Result<Option<()>, super::TypeError> {
        match other {
            Type::Apply(v) => {
                let func_match = self.pair.0.is(
                    &v.pair.0,
                    assumptions,
                    closure_env,
                    pattern_env,
                    pattern_mode,
                )?;
                if func_match.is_none() {
                    return Ok(None);
                }
                let arg_match = self.pair.1.is(
                    &v.pair.1,
                    assumptions,
                    closure_env,
                    pattern_env,
                    pattern_mode,
                )?;
                if arg_match.is_none() {
                    return Ok(None);
                }
                Ok(Some(()))
            }
            Type::Bound(TypeBound::Top) => Ok(Some(())),
            Type::Specialize(v) => v.has(self, assumptions, closure_env, pattern_env, pattern_mode),
            Type::Generalize(v) => v.has(self, assumptions, closure_env, pattern_env, pattern_mode),
            Type::FixPoint(v) => v.has(self, assumptions, closure_env, pattern_env, pattern_mode),
            Type::Pattern(v) => v.has(self, assumptions, closure_env, pattern_env, pattern_mode),
            Type::Variable(v) => v.has(self, assumptions, closure_env, pattern_env, pattern_mode),
            _ => Ok(None),
        }
    }

    fn reduce(
        &self,
        v: &ClosureEnv,
        p: &ParamEnv,
        rec_assumptions: &mut smallvec::SmallVec<[(TaggedPtr<()>, Type, bool); 8]>,
        gc: &mut GC<FixPointInner>,
    ) -> Result<StabilizedType, super::TypeError> {
        let f = self.pair.0.reduce(v, p, rec_assumptions, gc)?;
        let value = self.pair.1.reduce(v, p, rec_assumptions, gc)?;
        f.apply(&value, v, p, rec_assumptions, gc)
    }

    fn apply(
        &self,
        _v: &Type,
        _context: &ClosureEnv,
        _p: &ParamEnv,
        _rec_assumptions: &mut smallvec::SmallVec<[(TaggedPtr<()>, Type, bool); 8]>,
        _gc: &mut GC<FixPointInner>,
    ) -> Result<StabilizedType, super::TypeError> {
        Err(TypeError::NonApplicableType(
            self.clone().dispatch().stabilize().into(),
        ))
    }
}

impl Representable for Apply {
    fn represent(&self, path: &mut FastCycleDetector<*const ()>) -> String {
        format!(
            "({})[{}]",
            self.pair.0.represent(path),
            self.pair.1.represent(path)
        )
    }
}

impl Apply {
    pub fn new<U: AsTypeRef, V: AsTypeRef>(func: U, arg: V) -> StabilizedType {
        Self {
            pair: Arc::new((func.as_type_ref().clone(), arg.as_type_ref().clone())),
        }
        .dispatch()
        .stabilize()
    }
}
