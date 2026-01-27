use std::sync::Arc;

use crate::types::{CoinductiveTypeRef, CollectorExt};
use crate::util::source_info::SourceLocation;
use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, TaggedPtr, Type, TypeCheckContext, TypeError,
        TypeRef,
    },
    util::{cycle_detector::FastCycleDetector, three_valued_logic::ThreeValuedLogic},
};
use arc_gc::traceable::GCTraceable;

pub enum Variable<T: GcAllocObject<T, Inner = Type<T>>> {
    ContextVariable {
        bind_name: Arc<str>,
        source_info: Option<Arc<SourceLocation>>,
        _phantom: std::marker::PhantomData<T>,
    },
    PatternVariable {
        bind_name: Arc<str>,
        source_info: Option<Arc<SourceLocation>>,
        _phantom: std::marker::PhantomData<T>,
    },
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Variable<T> {
    fn clone(&self) -> Self {
        match self {
            Variable::ContextVariable { bind_name, source_info, _phantom } => {
                Variable::ContextVariable {
                    bind_name: bind_name.clone(),
                    source_info: source_info.clone(),
                    _phantom: std::marker::PhantomData,
                }
            }
            Variable::PatternVariable { bind_name, source_info, _phantom } => {
                Variable::PatternVariable {
                    bind_name: bind_name.clone(),
                    source_info: source_info.clone(),
                    _phantom: std::marker::PhantomData,
                }
            }
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Variable<T> {
    fn collect(&self, _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {}
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Variable<T> {}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for Variable<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;

    fn into_dispatcher(self) -> Type<T> {
        Type::Variable(self)
    }

    fn as_ref_dispatcher<'a>(&'a self) -> Self::RefDispatcher<'a> {
        TypeRef::Variable(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for Variable<T> {
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
                TypeRef::Any(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::All(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Constraint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::SubOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                _ => match self {
                    Variable::PatternVariable { .. } => Ok(ThreeValuedLogic::Unknown),
                    Variable::ContextVariable { bind_name, .. } => {
                        if let Some(ty) = ctx.lhs_env.lookup(bind_name) {
                            ty.check(other, &mut inner_ctx)
                        } else {
                            Ok(ThreeValuedLogic::Unknown)
                        }
                    }
                },
            }
        })
    }

    fn subof(
        &self,
        other: Self::RefDispatcher<'_>,
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
                TypeRef::Any(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::All(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),

                _ => Ok(ThreeValuedLogic::Unknown),
            }
        })
    }

    fn reduce(
        self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        match self {
            Variable::PatternVariable { bind_name, .. } => {
                if let Some(ty) = ctx.pattern_environment.lookup(&bind_name) {
                    Ok(ty.clone())
                } else {
                    Err(TypeError::UnboundEnvironmentVariable(
                        bind_name.to_string().into_boxed_str(),
                    ))
                }
            }
            Variable::ContextVariable { bind_name, .. } => {
                if let Some(ty) = ctx.capture_environment.lookup(&bind_name) {
                    Ok(ty.clone())
                } else {
                    Err(TypeError::UnboundContextVariable(bind_name.to_string().into_boxed_str()))
                }
            }
        }
    }

    fn invoke(self, _ctx: InvokeContext<Type<T>, T>) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.clone().dispatch().into()))
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        match self {
            Variable::ContextVariable { source_info, .. } => source_info.as_ref(),
            Variable::PatternVariable { source_info, .. } => source_info.as_ref(),
        }
    }

    fn report_source_info(&self) -> crate::types::TypeReport {
        if let Some(loc) = &self.source_info() {
            let span = loc.span().clone();
            let filepath = loc.source().filepath().to_string();
            ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                .with_message(format!("Type variable at {}", filepath))
                .with_label(
                    ariadne::Label::new((filepath, span))
                        .with_message("Type variable defined here"),
                )
                .finish()
        } else {
            ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                .with_message("Type variable has no source location")
                .with_label(
                    ariadne::Label::new(("<unknown>".to_string(), 0..0))
                        .with_message("Location unknown"),
                )
                .finish()
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveTypeWithAny<Type<T>, T> for Variable<T> {
    #[stacksafe::stacksafe]
    fn accept(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        other.check(
            (match self {
                Variable::PatternVariable { .. } => return Ok(ThreeValuedLogic::Unknown),
                Variable::ContextVariable { bind_name, .. } => {
                    match ctx.rhs_env.lookup(bind_name) {
                        Some(ty) => ty.clone(),
                        None => return Ok(ThreeValuedLogic::Unknown),
                    }
                }
            })
            .as_ref_dispatcher(),
            ctx,
        )
    }

    #[stacksafe::stacksafe]
    fn superof(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        other.subof(
            (match self {
                Variable::PatternVariable { .. } => return Ok(ThreeValuedLogic::Unknown),
                Variable::ContextVariable { bind_name, .. } => {
                    match ctx.rhs_env.lookup(bind_name) {
                        Some(ty) => ty.clone(),
                        None => return Ok(ThreeValuedLogic::Unknown),
                    }
                }
            })
            .as_ref_dispatcher(),
            ctx,
        )
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Variable<T> {
    fn represent(
        &self,
        _path: &mut FastCycleDetector<TaggedPtr<()>>,
        _depth: usize,
        _max_depth: usize,
    ) -> String {
        match self {
            Variable::ContextVariable { bind_name, .. } => format!("c.{}", bind_name),
            Variable::PatternVariable { bind_name, .. } => format!("λ.{}", bind_name),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Variable<T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new_context(
        bind_name: impl Into<Arc<str>>,
        source_info: Option<Arc<SourceLocation>>,
    ) -> Type<T> {
        Variable::ContextVariable {
            bind_name: bind_name.into(),
            source_info,
            _phantom: std::marker::PhantomData,
        }
        .dispatch()
    }

    #[allow(clippy::new_ret_no_self)]
    pub fn new_pattern(
        bind_name: impl Into<Arc<str>>,
        source_info: Option<Arc<SourceLocation>>,
    ) -> Type<T> {
        Variable::PatternVariable {
            bind_name: bind_name.into(),
            source_info,
            _phantom: std::marker::PhantomData,
        }
        .dispatch()
    }

    pub fn bind_name(&self) -> &Arc<str> {
        match self {
            Variable::ContextVariable { bind_name, .. } => bind_name,
            Variable::PatternVariable { bind_name, .. } => bind_name,
        }
    }
}
