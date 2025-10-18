use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, Type, TypeCheckContext, TypeError, TypeRef,
    },
    util::cycle_detector::FastCycleDetector,
};

pub enum TypeBound<T: GcAllocObject<T, Inner = Type<T>>> {
    Top,
    Bottom,
    PandomData(std::marker::PhantomData<T>),
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for TypeBound<T> {
    fn clone(&self) -> Self {
        match self {
            TypeBound::Top => TypeBound::Top,
            TypeBound::Bottom => TypeBound::Bottom,
            TypeBound::PandomData(_) => TypeBound::PandomData(std::marker::PhantomData),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for TypeBound<T> {
    fn collect(&self, _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {}
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GcAllocObject<T> for TypeBound<T> {
    type Inner = Type<T>;
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for TypeBound<T> {}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for TypeBound<T> {
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

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for TypeBound<T> {
    fn fulfill(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<Option<()>, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env);
            match other {
                // 这些都是规则变换类型，他们必须被优先处理
                TypeRef::Specialize(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Generalize(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                _ => match self {
                    TypeBound::Top => match other {
                        TypeRef::Bound(TypeBound::Top) => Ok(Some(())),
                        _ => Ok(None),
                    },
                    TypeBound::Bottom => Ok(Some(())), // ⊥ 可以满足任何类型
                    TypeBound::PandomData(_) => Ok(None),
                },
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

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for TypeBound<T> {
    fn represent(&self, _path: &mut FastCycleDetector<*const ()>) -> String {
        match self {
            TypeBound::Top => "⊤".to_string(),
            TypeBound::Bottom => "⊥".to_string(),
            TypeBound::PandomData(_) => "<?>".to_string(),
        }
    }
    fn display(&self, _path: &mut FastCycleDetector<*const ()>) -> String {
        match self {
            TypeBound::Top => "true".to_string(),
            TypeBound::Bottom => "false".to_string(),
            TypeBound::PandomData(_) => "<?>".to_string(),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> TypeBound<T> {
    pub fn top() -> Type<T> {
        Self::Top.dispatch()
    }

    pub fn bottom() -> Type<T> {
        Self::Bottom.dispatch()
    }
}
