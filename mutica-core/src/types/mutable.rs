use std::sync::Arc;

use arc_gc::{
    arc::{GCArc, GCArcWeak},
    gc::GC,
    traceable::GCTraceable,
};

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeRef, CoinductiveTypeWithAny, GcAllocObject,
        Representable, Rootable, TaggedPtr, Type, TypeCheckContext, TypeError, TypeRef,
    },
    util::{
        collector::CollectorExt, cycle_detector::FastCycleDetector, rootstack::RootStack,
        source_info::SourceLocation, three_valued_logic::ThreeValuedLogic,
    },
};

pub struct Mutable<T: GcAllocObject<T, Inner = Type<T>>> {
    reference: GCArcWeak<T>,
    source_info: Option<Arc<SourceLocation>>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Mutable<T> {
    fn clone(&self) -> Self {
        Self { reference: self.reference.clone(), source_info: self.source_info.clone() }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Mutable<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        queue.push_back(self.reference.clone());
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Mutable<T> {
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        if let Some(strong) = self.reference.upgrade() {
            collected.push(strong);
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Mutable<T> {
    fn represent(
        &self,
        path: &mut crate::util::cycle_detector::FastCycleDetector<TaggedPtr<()>>,
        depth: usize,
        max_depth: usize,
    ) -> String {
        if depth > max_depth {
            return "...".to_string();
        }
        match self.reference.upgrade() {
            Some(strong) => match strong.as_ref().get_mutable_value() {
                Some(value) => {
                    format!("Mut<{}>", value.represent(path, depth + 1, max_depth))
                }
                None => "Mut<!EmptySlot>".to_string(),
            },
            None => "Mut<!DanglingPointer>".to_string(),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for Mutable<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;

    fn into_dispatcher(self) -> Type<T> {
        Type::Mutable(self)
    }

    fn as_ref_dispatcher<'a>(&'a self) -> Self::RefDispatcher<'a> {
        TypeRef::Mutable(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for Mutable<T> {
    fn check(
        &self,
        other: TypeRef<T>,
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
                TypeRef::Any(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::All(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Constraint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::SubOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Mutable(v) => {
                    match (self.reference.upgrade(), v.reference.upgrade()) {
                        (Some(self_strong), Some(v_strong)) => {
                            let mut path = FastCycleDetector::new();
                            self_strong
                                .as_ref()
                                .map_mutable_value(&mut path, |path, self_value| {
                                    v_strong.as_ref().map_mutable_value(path, |_, v_value| {
                                        self_value.check(v_value, &mut inner_ctx)
                                    })
                                })
                                .flatten()
                                .unwrap_or(Ok(ThreeValuedLogic::Unknown))
                        }
                        _ => Ok(ThreeValuedLogic::Unknown), // one of the references is dead
                    }
                }
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
                TypeRef::Pattern(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Mutable(v) => {
                    match (self.reference.upgrade(), v.reference.upgrade()) {
                        (Some(self_strong), Some(v_strong)) => {
                            let mut path = FastCycleDetector::new();
                            self_strong
                                .as_ref()
                                .map_mutable_value(&mut path, |path, self_value| {
                                    v_strong.as_ref().map_mutable_value(path, |_, v_value| {
                                        self_value.subof(v_value, &mut inner_ctx)
                                    })
                                })
                                .flatten()
                                .unwrap_or(Ok(ThreeValuedLogic::Unknown))
                        }
                        _ => Ok(ThreeValuedLogic::Unknown), // one of the references is dead
                    }
                }
                _ => Ok(ThreeValuedLogic::False),
            }
        })
    }

    fn reduce(
        self,
        ctx: &mut super::ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        match self.reference.upgrade() {
            Some(strong) => strong
                .as_ref()
                .map_mutable_value(&mut FastCycleDetector::new(), |_, value| {
                    value.clone_data().reduce(ctx)
                })
                .transpose()?
                .map(|reduced| Self::new(reduced, self.source_info.clone(), ctx.gc, ctx.roots))
                .ok_or_else(|| TypeError::UnresolvableType(self.dispatch().into())),
            None => Err(TypeError::UnresolvableType(self.dispatch().into())),
        }
    }

    fn invoke(
        self,
        _ctx: super::InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.dispatch().into()))
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        self.source_info.as_ref()
    }

    fn report_source_info(&self) -> crate::types::TypeReport {
        if let Some(loc) = self.source_info() {
            let span = loc.span().clone();
            let filepath = loc.source().filepath().to_string();
            ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                .with_message(format!("Mutable type at {}", filepath))
                .with_label(
                    ariadne::Label::new((filepath, span)).with_message("Mutable type defined here"),
                )
                .finish()
        } else {
            ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                .with_message("Mutable type has no source location")
                .with_label(
                    ariadne::Label::new(("<unknown>".to_string(), 0..0))
                        .with_message("Location unknown"),
                )
                .finish()
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Mutable<T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new<X: AsDispatcher<Type<T>, T>>(
        value: X,
        source_info: Option<Arc<SourceLocation>>,
        gc: &mut GC<T>,
        root_stack: &mut RootStack<Type<T>, T>,
    ) -> Type<T> {
        let gc_arc = gc.create(T::new_mutable_slot(value.into_dispatcher()));
        let reference = gc_arc.as_weak();
        root_stack.push(gc_arc);
        Mutable { reference, source_info }.dispatch()
    }

    pub fn reference(&self) -> Option<GCArc<T>> {
        self.reference.upgrade()
    }

    pub fn assign<X: AsDispatcher<Type<T>, T>>(
        &self,
        new_value: X,
    ) -> Result<(), TypeError<Type<T>, T>> {
        match self.reference.upgrade() {
            Some(strong) => {
                strong.as_ref().set_mutable_value(new_value.into_dispatcher())?;
                Ok(())
            }
            None => Err(TypeError::UnresolvableType(self.clone().dispatch().into())),
        }
    }
}
