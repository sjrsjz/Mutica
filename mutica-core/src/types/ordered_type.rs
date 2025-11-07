use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, Representable, Type,
        TypeCheckContext, TypeError, TypeRef, type_bound::TypeBound,
    },
    util::{rootstack::Rootable, three_valued_logic::ThreeValuedLogic},
};

pub struct OrderedType<T: GcAllocObject<T, Inner = Type<T>>> {
    level: usize,
    _phantom: std::marker::PhantomData<T>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for OrderedType<T> {
    fn clone(&self) -> Self {
        Self {
            level: self.level,
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for OrderedType<T> {
    fn collect(&self, _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {}
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for OrderedType<T> {}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for OrderedType<T> {
    fn represent(
        &self,
        _path: &mut crate::util::cycle_detector::FastCycleDetector<crate::types::TaggedPtr<()>>,
    ) -> String {
        format!("Type<level={}>", self.level)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> OrderedType<T> {
    pub fn level(&self) -> usize {
        self.level
    }

    #[allow(clippy::new_ret_no_self)]
    pub fn new(level: usize) -> Type<T> {
        Self {
            level,
            _phantom: std::marker::PhantomData,
        }
        .dispatch()
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for OrderedType<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;

    fn into_dispatcher(self) -> Type<T> {
        Type::OrderedType(self)
    }

    fn as_ref_dispatcher<'a>(&'a self) -> Self::RefDispatcher<'a> {
        TypeRef::OrderedType(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for OrderedType<T> {
    fn check(
        &self,
        other: crate::types::TypeRef<T>,
        ctx: &mut crate::types::TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, crate::types::TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx = crate::types::TypeCheckContext::new(
                ctx.assumptions,
                ctx.closure_env,
                pattern_env,
                ctx.rhs,
            );
            match other {
                TypeRef::Any(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::All(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::EqOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::SubOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Bound(crate::types::type_bound::TypeBound::Top) => {
                    Ok(ThreeValuedLogic::True)
                }
                TypeRef::OrderedType(v) => Ok((self.level < v.level).into()),
                _ => Ok(ThreeValuedLogic::False),
            }
        })
    }

    fn subof(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut super::TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.rhs);
            match other {
                TypeRef::Any(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::All(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Bound(TypeBound::Top) => Ok(ThreeValuedLogic::True),
                TypeRef::OrderedType(v) => Ok((self.level == v.level).into()),
                _ => Ok(ThreeValuedLogic::False),
            }
        })
    }

    fn invoke(
        self,
        _ctx: super::InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.dispatch().into()))
    }

    fn recalculate_normal_form(
        &self,
        _cycle_detector: &mut crate::util::cycle_detector::FastCycleDetector<super::TaggedPtr<()>>,
    ) {
    }

    fn is_normal_form(&self) -> ThreeValuedLogic {
        ThreeValuedLogic::True
    }

    fn reduce(
        self,
        _ctx: &mut super::ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Ok(self.dispatch())
    }
}
