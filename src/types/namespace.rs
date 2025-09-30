use std::sync::Arc;

use arc_gc::{gc::GC, traceable::GCTraceable};

use crate::{
    types::{
        AsTypeRef, CoinductiveType, CoinductiveTypeWithAny, Representable, Rootable,
        StabilizedType, TaggedPtr, Type,
        closure::{ClosureEnv, ParamEnv},
        fixpoint::FixPointInner,
        type_bound::TypeBound,
    },
    util::collector::Collector,
};

#[derive(Clone)]
pub struct Namespace {
    tag: String,
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
    fn upgrade(&self, collected: &mut smallvec::SmallVec<[arc_gc::arc::GCArc<FixPointInner>; 8]>) {
        self.expr.upgrade(collected);
    }
}

impl Representable for Namespace {
    fn represent(
        &self,
        path: &mut crate::util::cycle_detector::FastCycleDetector<*const ()>,
    ) -> String {
        format!("{}::{}", self.tag, self.expr.represent(path))
    }
}

impl CoinductiveType<Type, StabilizedType> for Namespace {
    fn dispatch(self) -> Type {
        Type::Namespace(self)
    }

    fn is(
        &self,
        other: &Type,
        assumptions: &mut smallvec::SmallVec<[(super::TaggedPtr<()>, super::TaggedPtr<()>); 8]>,
        closure_env: (&super::closure::ClosureEnv, &super::closure::ClosureEnv),
        pattern_env: &mut Collector<(usize, Type)>,
        pattern_mode: bool,
    ) -> Result<Option<()>, super::TypeError> {
        pattern_env.collect(|pattern_env| match other {
            Type::Namespace(v) => {
                if self.tag == v.tag {
                    self.expr
                        .is(&v.expr, assumptions, closure_env, pattern_env, pattern_mode)
                } else {
                    Ok(None)
                }
            }
            Type::Bound(TypeBound::Top) => Ok(Some(())),
            Type::Generalize(v) => v.has(self, assumptions, closure_env, pattern_env, pattern_mode),
            Type::Specialize(v) => v.has(self, assumptions, closure_env, pattern_env, pattern_mode),
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
    ) -> Result<StabilizedType, super::TypeError> {
        Ok(Self::new(
            &self.tag,
            self.expr.reduce(v, p, rec_assumptions, gc)?,
        ))
    }

    fn apply(
        &self,
        v: &Type,
        context: &ClosureEnv,
        p: &ParamEnv,
        rec_assumptions: &mut smallvec::SmallVec<[(TaggedPtr<()>, Type, bool); 8]>,
        gc: &mut GC<FixPointInner>,
    ) -> Result<StabilizedType, super::TypeError> {
        self.expr.apply(v, context, p, rec_assumptions, gc)
    }
}

impl Namespace {
    pub fn new<T: Into<String>, I: AsTypeRef>(tag: T, expr: I) -> StabilizedType {
        Self {
            tag: tag.into(),
            expr: Arc::new(expr.as_type_ref().clone()),
        }
        .dispatch()
        .stabilize()
    }

    pub fn expr(&self) -> &Type {
        &self.expr
    }

    pub fn tag(&self) -> &str {
        &self.tag
    }
}
