use std::sync::Arc;

use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, CollectorExt, GcAllocObject,
        InvokeContext, ReductionContext, Representable, Rootable, TaggedPtr, Type,
        TypeCheckContext, TypeError, TypeRef, natural_number::NaturalNumber,
    },
    util::{
        cycle_detector::FastCycleDetector, source_info::SourceLocation,
        three_valued_logic::ThreeValuedLogic,
    },
};

pub struct NaturalNumberSet<T: GcAllocObject<T, Inner = Type<T>>> {
    source_info: Option<Arc<SourceLocation>>,
    _phantom: std::marker::PhantomData<T>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for NaturalNumberSet<T> {
    fn clone(&self) -> Self {
        Self { source_info: self.source_info.clone(), _phantom: std::marker::PhantomData }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for NaturalNumberSet<T> {
    fn collect(&self, _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {}
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for NaturalNumberSet<T> {}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for NaturalNumberSet<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;
    fn as_ref_dispatcher(&self) -> Self::RefDispatcher<'_> {
        TypeRef::<T>::NaturalNumberSet(self)
    }

    fn into_dispatcher(self) -> Type<T> {
        Type::<T>::NaturalNumberSet(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for NaturalNumberSet<T> {
    fn check(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        ctx.pattern_collector.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.collected,
            );
            match other {
                TypeRef::All(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Any(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Constraint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::SubOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                _ => Ok(ThreeValuedLogic::False),
            }
        })
    }

    fn subof(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut super::TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        ctx.pattern_collector.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.collected,
            );
            match other {
                TypeRef::Any(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::All(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::NaturalNumberSet(_) => Ok(ThreeValuedLogic::True),
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
                Type::NaturalNumber(_) => Ok(arg),
                Type::FloatValue(v) => {
                    Ok(NaturalNumber::new(v.value() as usize, self.source_info.clone()))
                }
                _ => Err(super::TypeError::TypeMismatch(
                    (arg, "NaturalNumber | FloatValue expected".into()).into(),
                )),
            })?
            .unwrap_or(Err(TypeError::UnresolvableType(self.dispatch().into())))
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        self.source_info.as_ref()
    }

    fn report_source_info(&self) -> crate::types::TypeReport {
        if let Some(loc) = &self.source_info {
            let span = loc.span().clone();
            let filepath = loc.source().filepath().to_string();
            ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                .with_message(format!("Float type at {}", filepath))
                .with_label(
                    ariadne::Label::new((filepath, span)).with_message("Float type defined here"),
                )
                .finish()
        } else {
            ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                .with_message("Float type has no source location")
                .with_label(
                    ariadne::Label::new(("<unknown>".to_string(), 0..0))
                        .with_message("Location unknown"),
                )
                .finish()
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for NaturalNumberSet<T> {
    fn represent(
        &self,
        _path: &mut FastCycleDetector<TaggedPtr<()>>,
        _depth: usize,
        _max_depth: usize,
    ) -> String {
        "Nat".to_string()
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> NaturalNumberSet<T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(source_info: Option<Arc<SourceLocation>>) -> Type<T> {
        NaturalNumberSet { source_info, _phantom: std::marker::PhantomData }.dispatch()
    }
}
