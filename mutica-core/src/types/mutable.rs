use std::sync::Arc;

use arc_gc::{
    arc::{GCArc, GCArcWeak},
    gc::GC,
    traceable::GCTraceable,
};

use crate::{
    as_type,
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeRef, CoinductiveTypeWithAny, CollectorExt,
        GcAllocObject, Representable, Rootable, TaggedPtr, Type, TypeCheckContext, TypeError,
        TypeRef,
    },
    util::{
        cycle_detector::FastCycleDetector, rootstack::RootStack, source_info::SourceLocation,
        three_valued_logic::ThreeValuedLogic,
    },
};

pub struct Mutable<U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    reference: GCArcWeak<V>,
    source_info: Option<Arc<SourceLocation>>,
    _phantom: std::marker::PhantomData<(U, V)>,
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Clone for Mutable<U, V> {
    fn clone(&self) -> Self {
        Self {
            reference: self.reference.clone(),
            source_info: self.source_info.clone(),
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> GCTraceable<V> for Mutable<U, V> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<V>>) {
        queue.push_back(self.reference.clone());
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Rootable<V> for Mutable<U, V> {
    fn upgrade(&self, collected: &mut Vec<GCArc<V>>) {
        if let Some(strong) = self.reference.upgrade() {
            collected.push(strong);
        }
    }

    fn rootless(&self) -> bool {
        false
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Mutable<Type<T>, T> {
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

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for Mutable<Type<T>, T> {
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

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for Mutable<Type<T>, T> {
    fn check(
        &self,
        other: TypeRef<T>,
        ctx: &mut super::TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        ctx.pattern_collector.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.coinductive_assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.bound_generic_variables,
                ctx.allocators,
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
                ctx.coinductive_assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.bound_generic_variables,
                ctx.allocators,
            );
            match other {
                TypeRef::Any(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::All(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
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
        &self,
        ctx: &mut super::ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        match self.reference.upgrade() {
            Some(strong) => match strong.as_ref().get_mutable_value() {
                Some(value) => {
                    for r in ctx.rec_assumptions.iter_mut().rev() {
                        if r.0 == value.tagged_ptr() {
                            r.2 = true; // mark as used
                            return Ok(r.1.clone());
                        }
                    }
                    let temp_mutable = Self::new(
                        self.clone().dispatch(),
                        self.source_info.clone(),
                        ctx.gc,
                        ctx.roots,
                    );
                    ctx.rec_assumptions.push((value.tagged_ptr(), temp_mutable.clone(), false));
                    let result = value.reduce(ctx);
                    let (_, _, used) = ctx.rec_assumptions.pop().unwrap();
                    if used {
                        as_type!(&temp_mutable, Type::Mutable).assign(result?)?;
                        Ok(temp_mutable)
                    } else {
                        result.map(|reduced| {
                            Self::new(reduced, self.source_info.clone(), ctx.gc, ctx.roots)
                        })
                    }
                }
                None => Err(TypeError::UnresolvableType(self.clone().dispatch().into())),
            },
            None => Err(TypeError::UnresolvableType(self.clone().dispatch().into())),
        }
    }

    fn invoke(
        &self,
        _ctx: super::InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.clone().dispatch().into()))
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

impl<T: GcAllocObject<T, Inner = Type<T>>> Mutable<Type<T>, T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new<X: AsDispatcher<Type<T>, T>>(
        value: X,
        source_info: Option<Arc<SourceLocation>>,
        gc: &mut GC<T>,
        roots: &mut RootStack<Type<T>, T>,
    ) -> Type<T> {
        let gc_arc = gc.create(T::new_mutable_slot(value.into_dispatcher()));
        let reference = gc_arc.as_weak();
        roots.push(gc_arc);
        Mutable { reference, source_info, _phantom: std::marker::PhantomData }.dispatch()
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
