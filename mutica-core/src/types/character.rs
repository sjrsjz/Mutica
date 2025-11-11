use std::sync::Arc;

use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, TaggedPtr, Type, TypeCheckContext, TypeError,
        TypeRef, character_value::CharacterValue
    },
    util::{cycle_detector::FastCycleDetector, source_info::SourceLocation, three_valued_logic::ThreeValuedLogic},
};

pub struct Character<T: GcAllocObject<T, Inner = Type<T>>> {
    source_info: Option<Arc<SourceLocation>>,
    _phantom: std::marker::PhantomData<T>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Character<T> {
    fn clone(&self) -> Self {
        Self {
            source_info: self.source_info.clone(),
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Character<T> {
    fn collect(&self, _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {}
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Character<T> {}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for Character<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;
    fn as_ref_dispatcher(&self) -> Self::RefDispatcher<'_> {
        TypeRef::<T>::Char(self)
    }

    fn into_dispatcher(self) -> Type<T> {
        Type::<T>::Char(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for Character<T> {
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
                TypeRef::Char(_) => Ok(ThreeValuedLogic::True),
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
                Type::IntegerValue(iv) => {
                    let v = iv.value();
                    if v > std::char::MAX as i64 || v < 0 {
                        return Err(TypeError::TypeMismatch(
                            (iv.dispatch(), "Expected a valid Unicode code point".into()).into(),
                        ));
                    }
                    Ok(CharacterValue::new(std::char::from_u32(v as u32).unwrap(), None))
                }
                Type::CharValue(_) => Ok(arg),
                _ => Err(TypeError::TypeMismatch(
                    (arg, "IntegerValue or CharValue".into()).into(),
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

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Character<T> {
    fn represent(&self, _path: &mut FastCycleDetector<TaggedPtr<()>>) -> String {
        "Char".to_string()
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Character<T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(source_info: Option<Arc<SourceLocation>>) -> Type<T> {
        Character {
            source_info,
            _phantom: std::marker::PhantomData,
        }
        .dispatch()
    }
}
