use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, Type, TypeCheckContext, TypeError, TypeRef,
    },
    util::cycle_detector::FastCycleDetector,
};

#[derive(Debug, Clone)]
pub enum TypeBound {
    Top,
    Bottom,
}

impl<T: GcAllocObject<T>> GCTraceable<T> for TypeBound {
    fn collect(&self, _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {}
}

impl<T: GcAllocObject<T>> GcAllocObject<T> for TypeBound {
    type Inner = Type<T>;
}

impl<T: GcAllocObject<T>> Rootable<T> for TypeBound {}

impl<T: GcAllocObject<T>> AsDispatcher<Type<T>, T> for TypeBound {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;

    fn into_dispatcher(self) -> Type<T> {
        Type::Bound(self)
    }

    fn as_ref_dispatcher<'a>(&'a self) -> Self::RefDispatcher<'a> {
        TypeRef::Bound(self)
    }
}

impl<T: GcAllocObject<T>> CoinductiveType<Type<T>, T> for TypeBound {
    fn is(
        &self,
        other: &Type<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<Option<()>, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env);
            if let Type::Pattern(p) = other {
                return p.has(self, &mut inner_ctx);
            }
            match (self, other) {
                (TypeBound::Bottom, _) => Ok(Some(())),
                (TypeBound::Top, Type::Bound(TypeBound::Top)) => Ok(Some(())),
                _ => Ok(None),
            }
        })
    }

    fn is_normal_form(&self) -> bool {
        true
    }

    fn reduce(
        self,
        _ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Ok(self.dispatch())
    }

    fn invoke(
        &self,
        _ctx: &mut InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.clone().dispatch().into()))
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
    pub fn top<T: GcAllocObject<T>>() -> Type<T> {
        Self::Top.dispatch()
    }

    pub fn bottom<T: GcAllocObject<T>>() -> Type<T> {
        Self::Bottom.dispatch()
    }
}
