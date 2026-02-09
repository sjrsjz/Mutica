use std::sync::Arc;

use crate::types::unify::capture_env::CaptureEnvLookupError;
use crate::types::{CoinductiveTypeRef, CollectorExt, TypeOfContext};
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

pub enum Variable<U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    ContextVariable {
        bind_name: Arc<str>,
        source_info: Option<Arc<SourceLocation>>,
        _phantom: std::marker::PhantomData<(U, V)>,
    },
    ArgumentVariable {
        bind_name: Arc<str>,
        source_info: Option<Arc<SourceLocation>>,
        _phantom: std::marker::PhantomData<(U, V)>,
    },
    PatternVariable {
        bind_name: Arc<str>,
        layer: usize,
        source_info: Option<Arc<SourceLocation>>,
        _phantom: std::marker::PhantomData<(U, V)>,
    },
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Clone for Variable<U, V> {
    fn clone(&self) -> Self {
        match self {
            Variable::ContextVariable { bind_name, source_info, _phantom } => {
                Variable::ContextVariable {
                    bind_name: bind_name.clone(),
                    source_info: source_info.clone(),
                    _phantom: std::marker::PhantomData,
                }
            }
            Variable::ArgumentVariable { bind_name, source_info, _phantom } => {
                Variable::ArgumentVariable {
                    bind_name: bind_name.clone(),
                    source_info: source_info.clone(),
                    _phantom: std::marker::PhantomData,
                }
            }
            Variable::PatternVariable { bind_name, source_info, layer, _phantom } => {
                Variable::PatternVariable {
                    bind_name: bind_name.clone(),
                    source_info: source_info.clone(),
                    layer: *layer,
                    _phantom: std::marker::PhantomData,
                }
            }
        }
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> GCTraceable<V> for Variable<U, V> {
    fn collect(&self, _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<V>>) {}
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Rootable<V> for Variable<U, V> {
    fn rootless(&self) -> bool {
        true
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for Variable<Type<T>, T> {
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

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for Variable<Type<T>, T> {
    fn check(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        ctx.pattern_collector.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.coinductive_assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.bound_generic_variables,
                
            );
            match other {
                TypeRef::Any(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::All(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Constraint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::SubOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                _ => match self {
                    Variable::ArgumentVariable { .. } => Ok(ThreeValuedLogic::Unknown),
                    Variable::ContextVariable { bind_name, .. } => {
                        if let Some(ty) = ctx.lhs_env.lookup(bind_name).ok().flatten() {
                            ty.check(other, &mut inner_ctx)
                        } else {
                            Ok(ThreeValuedLogic::Unknown)
                        }
                    }
                    Variable::PatternVariable { bind_name, layer, .. } => {
                        if let Some(ty) = inner_ctx
                            .bound_generic_variables
                            .lookup(bind_name, *layer, inner_ctx.bound_generic_variables.is_lhs())
                            .ok_or_else(|| {
                                TypeError::GenericLayerOverflow(self.clone().dispatch().into())
                            })?
                        {
                            ty.clone().check(other, &mut inner_ctx)
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
                ctx.coinductive_assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.bound_generic_variables,
                
            );
            match other {
                TypeRef::Any(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::All(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Variable(other_varible) => {
                    let is_lhs = inner_ctx.bound_generic_variables.is_lhs();
                    match (self, other_varible, inner_ctx.bound_generic_variables) {
                        (
                            Variable::ArgumentVariable { bind_name: self_bind_name, .. },
                            Variable::ArgumentVariable { bind_name: other_bind_name, .. },
                            _,
                        ) => {
                            // println!(
                            //     "Checking ArgumentVariable subtype: {} <: {}",
                            //     self_bind_name, other_bind_name
                            // );
                            // println!(
                            //     " bound_generic_layers: {:?}",
                            //     inner_ctx.bound_generic_variables
                            // );
                            // println!("is_lhs: {}", is_lhs);
                            if let Some(binding) = inner_ctx.bound_generic_variables.find_param_layer(0)
                                && binding.check_subtype_assumption(self_bind_name, other_bind_name, is_lhs)
                            {
                                // println!("Pass");
                                return Ok(ThreeValuedLogic::True);
                            }
                        }
                        (
                            Variable::PatternVariable { bind_name: self_bind_name, layer: self_layer, .. },
                            Variable::PatternVariable { bind_name: other_bind_name, layer: other_layer, .. },
                            ..
                        ) if self_layer == other_layer => {
                            let layer = inner_ctx.bound_generic_variables.find_generic_layer(*self_layer);
                            if let Some(binding) = layer
                                && binding.check_subtype_assumption(self_bind_name, other_bind_name, is_lhs)
                            {
                                return Ok(ThreeValuedLogic::True);
                            }
                        }
                        (
                            Variable::ContextVariable { bind_name: self_bind_name, .. },
                            Variable::ContextVariable { bind_name: other_bind_name, .. },
                            _,
                        ) => {
                            // 当两边都是 ContextVariable 时，检查它们是否都溯源到未绑定的 ArgumentVariable
                            // lookup 返回 Err(CaptureEnvLookupError::Argument(layer)) 时，layer 表示在哪一层找到的未绑定 Argument
                            if let (
                                Err(CaptureEnvLookupError::Argument(lhs_layer)),
                                Err(CaptureEnvLookupError::Argument(rhs_layer)),
                            ) = (
                                ctx.lhs_env.lookup(self_bind_name),
                                ctx.rhs_env.lookup(other_bind_name),
                            ) && lhs_layer == rhs_layer
                            {
                                // 使用 lhs_layer 找到对应层级的 GenericBinding
                                if let Some(binding) =
                                    inner_ctx.bound_generic_variables.find_param_layer(lhs_layer + 1) // +1 因为 layer 表示当前层，而我们需要查找当前层的父层所绑定的假设
                                    &&  binding.check_subtype_assumption(self_bind_name, other_bind_name, is_lhs)
                                {
                                    return Ok(ThreeValuedLogic::True);
                                }
                            }
                        }
                        _ => {}
                    }
                    match self {
                        Variable::ArgumentVariable { .. } => Ok(ThreeValuedLogic::Unknown),
                        Variable::ContextVariable { bind_name, .. } => {
                            if let Some(ty) = ctx.lhs_env.lookup(bind_name).ok().flatten() {
                                ty.subof(other, &mut inner_ctx)
                            } else {
                                Ok(ThreeValuedLogic::Unknown)
                            }
                        }
                        Variable::PatternVariable { bind_name, layer, .. } => {
                            let ty = inner_ctx
                                .bound_generic_variables
                                .lookup(bind_name, *layer, inner_ctx.bound_generic_variables.is_lhs())
                                .ok_or_else(|| {
                                    println!(
                                        "Generic layer overflow when checking subtype: P_{}.{} <: ...",
                                        layer, bind_name
                                    );
                                    TypeError::GenericLayerOverflow(self.clone().dispatch().into())
                                })?;
                            if let Some(ty) = ty
                            {
                                ty.clone().subof(other, &mut inner_ctx)
                            } else {
                                Ok(ThreeValuedLogic::Unknown)
                            }
                        }
                    }
                }
                _ => match self {
                    Variable::ArgumentVariable { .. } => Ok(ThreeValuedLogic::Unknown),
                    Variable::ContextVariable { bind_name, .. } => {
                        if let Some(ty) = ctx.lhs_env.lookup(bind_name).ok().flatten() {
                            ty.subof(other, &mut inner_ctx)
                        } else {
                            Ok(ThreeValuedLogic::Unknown)
                        }
                    }
                    Variable::PatternVariable { bind_name, layer, .. } => {
                        if let Some(ty) = inner_ctx
                            .bound_generic_variables
                            .lookup(bind_name, *layer, inner_ctx.bound_generic_variables.is_lhs())
                            .ok_or_else(|| {
                                TypeError::GenericLayerOverflow(self.clone().dispatch().into())
                            })?
                        {
                            ty.clone().subof(other, &mut inner_ctx)
                        } else {
                            Ok(ThreeValuedLogic::Unknown)
                        }
                    }
                },
            }
        })
    }

    fn reduce(
        &self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        match self {
            Variable::ArgumentVariable { bind_name, .. } => {
                if let Some(ty) = ctx
                    .solved_argument
                    .iter()
                    .find(|(name, _)| name.as_ref() == bind_name.as_ref())
                    .map(|(_, ty)| ty)
                {
                    match ty.get_bound() {
                        Some(ty) => Ok(ty.clone()),
                        None => {
                            Err(TypeError::UnboundArgument(bind_name.to_string().into_boxed_str()))
                        }
                    }
                } else {
                    Err(TypeError::MissingVariable(bind_name.to_string().into_boxed_str()))
                }
            }
            Variable::ContextVariable { bind_name, .. } => {
                if let Some(ty) = ctx.capture_env.lookup(bind_name).map_err(|_| {
                    TypeError::MissingVariable(bind_name.to_string().into_boxed_str())
                })? {
                    Ok(ty.clone())
                } else {
                    Err(TypeError::MissingVariable(bind_name.to_string().into_boxed_str()))
                }
            }
            Variable::PatternVariable { .. } => Ok(self.clone().dispatch()),
        }
    }

    fn invoke(&self, _ctx: InvokeContext<Type<T>, T>) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.clone().dispatch().into()))
    }

    fn type_of(&self, _ctx: &mut TypeOfContext<Type<T>, T>) -> Result<Type<T>, TypeError<Type<T>, T>> {
        panic!("Variable::type_of should not be called")
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        match self {
            Variable::ContextVariable { source_info, .. } => source_info.as_ref(),
            Variable::ArgumentVariable { source_info, .. } => source_info.as_ref(),
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

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveTypeWithAny<Type<T>, T>
    for Variable<Type<T>, T>
{
    #[stacksafe::stacksafe]
    fn accept(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        other.check(
            (match self {
                Variable::ArgumentVariable { .. } => return Ok(ThreeValuedLogic::Unknown),
                Variable::ContextVariable { bind_name, .. } => {
                    match ctx.rhs_env.lookup(bind_name).ok().flatten() {
                        Some(ty) => ty.clone(),
                        None => return Ok(ThreeValuedLogic::Unknown),
                    }
                }
                Variable::PatternVariable { bind_name, layer, .. } => {
                    if let Some(ty) = ctx
                        .bound_generic_variables
                        .lookup(bind_name, *layer, ctx.bound_generic_variables.is_lhs())
                        .ok_or_else(|| {
                            TypeError::GenericLayerOverflow(self.clone().dispatch().into())
                        })?
                    {
                        ty.clone()
                    } else {
                        return Ok(ThreeValuedLogic::Unknown);
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
                Variable::ArgumentVariable { .. } => return Ok(ThreeValuedLogic::Unknown),
                Variable::ContextVariable { bind_name, .. } => {
                    match ctx.rhs_env.lookup(bind_name).ok().flatten() {
                        Some(ty) => ty.clone(),
                        None => return Ok(ThreeValuedLogic::Unknown),
                    }
                }
                Variable::PatternVariable { bind_name, layer, .. } => {
                    if let Some(ty) = ctx
                        .bound_generic_variables
                        .lookup(bind_name, *layer, ctx.bound_generic_variables.is_lhs())
                        .ok_or_else(|| {
                            TypeError::GenericLayerOverflow(self.clone().dispatch().into())
                        })?
                    {
                        ty.clone()
                    } else {
                        return Ok(ThreeValuedLogic::Unknown);
                    }
                }
            })
            .as_ref_dispatcher(),
            ctx,
        )
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Variable<Type<T>, T> {
    fn represent(
        &self,
        _path: &mut FastCycleDetector<TaggedPtr<()>>,
        _depth: usize,
        _max_depth: usize,
    ) -> String {
        match self {
            Variable::ContextVariable { bind_name, .. } => format!("capture.{}", bind_name),
            Variable::ArgumentVariable { bind_name, .. } => format!("arg.{}", bind_name),
            Variable::PatternVariable { bind_name, layer, .. } => {
                format!("P_{}.{}", layer, bind_name)
            }
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Variable<Type<T>, T> {
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

    pub fn new_argument(
        bind_name: impl Into<Arc<str>>,
        source_info: Option<Arc<SourceLocation>>,
    ) -> Type<T> {
        Variable::ArgumentVariable {
            bind_name: bind_name.into(),
            source_info,
            _phantom: std::marker::PhantomData,
        }
        .dispatch()
    }

    pub fn new_pattern(
        bind_name: impl Into<Arc<str>>,
        layer: usize,
        source_info: Option<Arc<SourceLocation>>,
    ) -> Type<T> {
        Variable::PatternVariable {
            bind_name: bind_name.into(),
            layer,
            source_info,
            _phantom: std::marker::PhantomData,
        }
        .dispatch()
    }

    pub fn bind_name(&self) -> &Arc<str> {
        match self {
            Variable::ContextVariable { bind_name, .. } => bind_name,
            Variable::ArgumentVariable { bind_name, .. } => bind_name,
            Variable::PatternVariable { bind_name, .. } => bind_name,
        }
    }
}
