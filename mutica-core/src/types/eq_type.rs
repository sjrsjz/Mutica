use std::sync::{Arc, RwLock};

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, Representable,
        Rootable, TaggedPtr, Type, TypeCheckContext, TypeError, TypeRef, type_bound::TypeBound,
    },
    util::{cycle_detector::FastCycleDetector, three_valued_logic::ThreeValuedLogic},
};

pub struct EqType<T: GcAllocObject<T, Inner = Type<T>>> {
    value: Arc<Type<T>>,
    is_nf: Arc<RwLock<ThreeValuedLogic>>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for EqType<T> {
    fn clone(&self) -> Self {
        Self {
            value: self.value.clone(),
            is_nf: self.is_nf.clone(),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for EqType<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        self.value.collect(queue);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for EqType<T> {
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        self.value.upgrade(collected);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for EqType<T> {
    fn represent(
        &self,
        path: &mut crate::util::cycle_detector::FastCycleDetector<TaggedPtr<()>>,
    ) -> String {
        format!("Eq<{}>", self.value.represent(path))
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for EqType<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;

    fn into_dispatcher(self) -> Type<T> {
        Type::EqType(self)
    }

    fn as_ref_dispatcher<'a>(&'a self) -> Self::RefDispatcher<'a> {
        TypeRef::EqType(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for EqType<T> {
    fn check(
        &self,
        other: TypeRef<T>,
        ctx: &mut super::TypeCheckContext<Type<T>, T>,
    ) -> Result<bool, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.rhs);
            match other {
                TypeRef::Generalize(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Specialize(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Bound(TypeBound::Top) => Ok(true),
                // 我们实际上无法找到EqType所归属的任何类型类，因此只能简单地拒绝其他类型。
                _ => Ok(false),
            }
        })
    }

    fn equals(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut super::TypeCheckContext<Type<T>, T>,
    ) -> Result<bool, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.rhs);
            match other {
                TypeRef::FixPoint(v) => v.equals_any(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.equals_any(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.equals_any(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::EqType(v) => self
                    .value
                    .as_ref()
                    .equals(v.value.as_ref_dispatcher(), &mut inner_ctx),
                _ => Ok(false),
            }
        })
    }

    fn reduce(
        self,
        ctx: &mut super::ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        self.value.as_ref().clone().reduce(ctx).map(Self::new)
    }

    fn invoke(
        &self,
        _ctx: &mut super::InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.clone().dispatch().into()))
    }

    fn is_normal_form(&self) -> ThreeValuedLogic {
        match self.is_nf.read() {
            Ok(v) => *v,
            Err(_) => ThreeValuedLogic::False,
        }
    }

    fn recalculate_normal_form(&self, cycle_detector: &mut FastCycleDetector<TaggedPtr<()>>) {
        self.value.recalculate_normal_form(cycle_detector);
        let new_nf = self.value.is_normal_form();
        if let Ok(mut nf_lock) = self.is_nf.write() {
            *nf_lock = new_nf;
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveTypeWithAny<Type<T>, T> for EqType<T> {
    fn accept(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<bool, TypeError<Type<T>, T>> {
        other.equals(self.value.as_ref_dispatcher(), ctx)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> EqType<T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new<X: AsDispatcher<Type<T>, T>>(value: X) -> Type<T> {
        let value = value.into_dispatcher();
        let is_nf = value.is_normal_form();
        Self {
            value: Arc::new(value),
            is_nf: Arc::new(RwLock::new(is_nf)),
        }
        .dispatch()
    }

    pub fn value(&self) -> &Type<T> {
        self.value.as_ref()
    }
}
