use std::sync::Arc;

use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, TaggedPtr, Type, TypeCheckContext, TypeError,
        TypeRef, float_value::FloatValue
    },
    util::{cycle_detector::FastCycleDetector, source_info::SourceLocation, three_valued_logic::ThreeValuedLogic},
};

pub struct Float<T: GcAllocObject<T, Inner = Type<T>>> {
    source_info: Option<Arc<SourceLocation>>,
    _phantom: std::marker::PhantomData<T>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Float<T> {
    fn clone(&self) -> Self {
        Self {
            source_info: self.source_info.clone(),
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Float<T> {
    fn collect(&self, _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {}
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Float<T> {}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for Float<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;
    fn as_ref_dispatcher(&self) -> Self::RefDispatcher<'_> {
        TypeRef::<T>::Float(self)
    }

    fn into_dispatcher(self) -> Type<T> {
        Type::<T>::Float(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for Float<T> {
    fn check(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.rhs);
            match other {
                TypeRef::All(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Any(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::EqOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::SubOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Bound(v) if matches!(&v.kind, crate::types::type_bound::TypeBoundKind::Top) => Ok(ThreeValuedLogic::True),
                TypeRef::OrderedType(v) => Ok((v.level() == 0).into()),
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
                TypeRef::Bound(v) if matches!(&v.kind, crate::types::type_bound::TypeBoundKind::Top) => Ok(ThreeValuedLogic::True),
                TypeRef::Float(_) => Ok(ThreeValuedLogic::True),
                _ => Ok(ThreeValuedLogic::False),
            }
        })
    }

    fn reduce(
        self,
        _ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Ok(self.dispatch())
    }

    fn invoke(self, ctx: InvokeContext<Type<T>, T>) -> Result<Type<T>, TypeError<Type<T>, T>> {
        ctx.arg
            .take(&mut FastCycleDetector::new(), |_, arg| match arg {
                Type::FloatValue(_) => Ok(arg),
                Type::IntegerValue(v) => {
                    let float_value = FloatValue::<T>::new(v.value() as f64, None);
                    Ok(float_value)
                }
                _ => Err(super::TypeError::TypeMismatch(
                    (arg, "FloatValue or IntegerValue".into()).into(),
                )),
            })?
            .unwrap_or(Err(TypeError::UnresolvableType(
                "Could not resolve argument".into(),
            )))
    }

    fn is_normal_form(&self) -> ThreeValuedLogic {
        ThreeValuedLogic::True
    }

    fn recalculate_normal_form(&self, _: &mut FastCycleDetector<TaggedPtr<()>>) {}

    fn source_info(&self) -> Option<&SourceLocation> {
        self.source_info.as_deref()
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Float<T> {
    fn represent(&self, _path: &mut FastCycleDetector<TaggedPtr<()>>) -> String {
        "Integer".to_string()
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Float<T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(source_info: Option<Arc<SourceLocation>>) -> Type<T> {
        Float {
            source_info,
            _phantom: std::marker::PhantomData,
        }
        .dispatch()
    }
}
