use arc_gc::{gc::GC, traceable::GCTraceable};

use crate::{
    types::{
        CoinductiveType, CoinductiveTypeWithAny, Representable, Rootable, StabilizedType,
        TaggedPtr, Type, TypeError,
        closure::{ClosureEnv, ParamEnv},
        fixpoint::FixPointInner,
    },
    util::{collector::Collector, cycle_detector::FastCycleDetector},
};

#[derive(Clone)]
pub enum TypeBound {
    Top,
    Bottom,
}

impl GCTraceable<FixPointInner> for TypeBound {
    fn collect(
        &self,
        _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<FixPointInner>>,
    ) {
    }
}

impl Rootable for TypeBound {}

impl CoinductiveType<Type, StabilizedType> for TypeBound {
    fn is(
        &self,
        other: &Type,
        assumptions: &mut smallvec::SmallVec<[(TaggedPtr<()>, TaggedPtr<()>); 8]>,
        closure_env: (&ClosureEnv, &ClosureEnv),
        pattern_env: &mut Collector<(usize, Type)>,
        pattern_mode: bool,
    ) -> Result<Option<()>, TypeError> {
        pattern_env.collect(|pattern_env| {
            if let Type::Pattern(p) = other {
                return p.has(self, assumptions, closure_env, pattern_env, pattern_mode);
            }
            match (self, other) {
                (TypeBound::Bottom, _) => Ok(Some(())),
                (TypeBound::Top, Type::Bound(TypeBound::Top)) => Ok(Some(())),
                _ => Ok(None),
            }
        })
    }

    fn dispatch(self) -> Type {
        Type::Bound(self)
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

impl Representable for TypeBound {
    fn represent(&self, _path: &mut FastCycleDetector<*const ()>) -> String {
        match self {
            TypeBound::Top => "⊤".to_string(),
            TypeBound::Bottom => "⊥".to_string(),
        }
    }
    fn display(&self, _path: &mut FastCycleDetector<*const ()>) -> String {
        match self {
            TypeBound::Top => "true".to_string(),
            TypeBound::Bottom => "false".to_string(),
        }
    }
}

impl TypeBound {
    pub fn top() -> StabilizedType {
        Self::Top.dispatch().stabilize()
    }

    pub fn bottom() -> StabilizedType {
        Self::Bottom.dispatch().stabilize()
    }
}
