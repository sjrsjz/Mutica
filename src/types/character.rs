use arc_gc::{gc::GC, traceable::GCTraceable};

use crate::{
    types::{
        CoinductiveType, CoinductiveTypeWithAny, Representable, Rootable, StabilizedType,
        TaggedPtr, Type, TypeError,
        closure::{ClosureEnv, ParamEnv},
        fixpoint::FixPointInner,
        type_bound::TypeBound,
    },
    util::{collector::Collector, cycle_detector::FastCycleDetector},
};

#[derive(Clone)]
pub struct Character {}

impl GCTraceable<FixPointInner> for Character {
    fn collect(
        &self,
        _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<FixPointInner>>,
    ) {
    }
}

impl Rootable for Character {}

impl CoinductiveType<Type, StabilizedType> for Character {
    fn dispatch(self) -> Type {
        Type::Char(self)
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
            Type::Char(_) => Ok(Some(())),
            Type::Bound(TypeBound::Top) => Ok(Some(())),
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
        _v: &ClosureEnv,
        _p: &ParamEnv,
        _rec_assumptions: &mut smallvec::SmallVec<[(TaggedPtr<()>, Type, bool); 8]>,
        _gc: &mut GC<FixPointInner>,
    ) -> Result<StabilizedType, TypeError> {
        Ok(self.clone().dispatch().stabilize())
    }

    fn apply(
        &self,
        _v: &Type,
        _context: &ClosureEnv,
        _p: &ParamEnv,
        _rec_assumptions: &mut smallvec::SmallVec<[(TaggedPtr<()>, Type, bool); 8]>,
        _gc: &mut GC<FixPointInner>,
    ) -> Result<StabilizedType, TypeError> {
        Err(TypeError::NonApplicableType(
            self.clone().dispatch().stabilize().into(),
        ))
    }
}

impl Representable for Character {
    fn represent(&self, _path: &mut FastCycleDetector<*const ()>) -> String {
        "Char".to_string()
    }
}

impl Character {
    pub fn new() -> StabilizedType {
        Self {}.dispatch().stabilize()
    }
}
