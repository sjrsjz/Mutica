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
use arc_gc::{gc::GC, traceable::GCTraceable};

#[derive(Clone)]
pub struct Variable {
    debruijn_index: isize,
}

impl GCTraceable<FixPointInner> for Variable {
    fn collect(
        &self,
        _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<FixPointInner>>,
    ) {
    }
}

impl Rootable for Variable {}

impl CoinductiveType<Type, StabilizedType> for Variable {
    fn dispatch(self) -> Type {
        Type::Variable(self)
    }

    fn is(
        &self,
        other: &Type,
        assumptions: &mut smallvec::SmallVec<[(TaggedPtr<()>, TaggedPtr<()>); 8]>,
        closure_env: (&ClosureEnv, &ClosureEnv),
        pattern_env: &mut Collector<(usize, Type)>,
        pattern_mode: bool,
    ) -> Result<Option<()>, TypeError> {
        pattern_env.collect(|pattern_env| match other {
            Type::Bound(TypeBound::Top) => Ok(Some(())),
            Type::Generalize(v) => v.has(self, assumptions, closure_env, pattern_env, pattern_mode),
            Type::Specialize(v) => v.has(self, assumptions, closure_env, pattern_env, pattern_mode),
            Type::FixPoint(v) => v.has(self, assumptions, closure_env, pattern_env, pattern_mode),
            Type::Variable(v) => {
                if self.debruijn_index >= 0 || v.debruijn_index >= 0 {
                    return Ok(if self.debruijn_index == v.debruijn_index {
                        Some(())
                    } else {
                        None
                    });
                }
                // 如果都是负数，说明都是闭包内的变量
                // 需要从闭包环境中取出对应的类型进行比较
                let l = (-1 - self.debruijn_index) as usize;
                let r = (-1 - v.debruijn_index) as usize;

                let value_l = closure_env.0.get(l)?;
                let value_r = closure_env.1.get(r)?;
                value_l.is(value_r, assumptions, closure_env, pattern_env, pattern_mode)
            }
            Type::Pattern(v) => v.has(self, assumptions, closure_env, pattern_env, pattern_mode),
            _ => {
                if self.debruijn_index >= 0 {
                    Ok(None)
                } else {
                    let l = (-1 - self.debruijn_index) as usize;
                    let value = closure_env.0.get(l)?;
                    value.is(other, assumptions, closure_env, pattern_env, pattern_mode)
                }
            }
        })
    }

    fn reduce(
        &self,
        v: &ClosureEnv,
        p: &ParamEnv,
        _rec_assumptions: &mut smallvec::SmallVec<[(TaggedPtr<()>, Type, bool); 8]>,
        _gc: &mut GC<FixPointInner>,
    ) -> Result<StabilizedType, TypeError> {
        if self.debruijn_index >= 0 {
            Ok(p.get(self.debruijn_index as usize)
                .map(|t| t.clone())
                .unwrap_or(TypeBound::bottom()))
        } else {
            v.get((-1 - self.debruijn_index) as usize)
                .map(|t| t.clone().stabilize())
        }
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

impl CoinductiveTypeWithAny<Type, StabilizedType> for Variable {
    fn has<V: CoinductiveType<Type, StabilizedType>>(
        &self,
        other: &V,
        assumptions: &mut smallvec::SmallVec<[(TaggedPtr<()>, TaggedPtr<()>); 8]>,
        closure_env: (&ClosureEnv, &ClosureEnv),
        pattern_env: &mut Collector<(usize, Type)>,
        pattern_mode: bool,
    ) -> Result<Option<()>, TypeError> {
        pattern_env.collect(|pattern_env| {
            if self.debruijn_index >= 0 {
                return Ok(None);
            }
            let r = (-1 - self.debruijn_index) as usize;
            let value = closure_env.1.get(r)?;
            other.is(value, assumptions, closure_env, pattern_env, pattern_mode)
        })
    }
}

impl Representable for Variable {
    fn represent(&self, _path: &mut FastCycleDetector<*const ()>) -> String {
        format!("λ.{}", self.debruijn_index)
    }
}

impl Variable {
    pub fn new(debruijn_index: isize) -> StabilizedType {
        Self { debruijn_index }.dispatch().stabilize()
    }
}
