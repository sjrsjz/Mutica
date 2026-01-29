use std::sync::Arc;

use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, CollectorExt, GcAllocObject,
        InvokeContext, PatternCollector, ReductionContext, Representable, Rootable, TaggedPtr,
        Type, TypeCheckContext, TypeError, TypeRef, closure::Closure, fixpoint::FixPoint,
        float_value::FloatValue, invoke::Invoke, natural_number::NaturalNumber, sequence::Sequence,
        unify::EnvironmentStack, variable::Variable,
    },
    util::{
        cycle_detector::FastCycleDetector, source_info::SourceLocation,
        three_valued_logic::ThreeValuedLogic,
    },
};

pub struct Opcode<T: GcAllocObject<T, Inner = Type<T>>> {
    pub kind: OpcodeKind,
    source_info: Option<Arc<SourceLocation>>,
    _phantom: std::marker::PhantomData<T>,
}

pub enum OpcodeKind {
    // Super type
    Opcode,
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Less,
    Greater,
    Is,
    Neg,
    Assign,
    SetFixPoint,
    BuildFixPoint,
    // I/O
    IO(Box<String>),
    Pandom,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Opcode<T> {
    fn clone(&self) -> Self {
        Self {
            kind: self.kind.clone(),
            source_info: self.source_info.clone(),
            _phantom: std::marker::PhantomData,
        }
    }
}

impl Clone for OpcodeKind {
    fn clone(&self) -> Self {
        match self {
            OpcodeKind::Opcode => OpcodeKind::Opcode,
            OpcodeKind::Add => OpcodeKind::Add,
            OpcodeKind::Sub => OpcodeKind::Sub,
            OpcodeKind::Mul => OpcodeKind::Mul,
            OpcodeKind::Div => OpcodeKind::Div,
            OpcodeKind::Mod => OpcodeKind::Mod,
            OpcodeKind::Less => OpcodeKind::Less,
            OpcodeKind::Greater => OpcodeKind::Greater,
            OpcodeKind::Is => OpcodeKind::Is,
            OpcodeKind::Neg => OpcodeKind::Neg,
            OpcodeKind::Assign => OpcodeKind::Assign,
            OpcodeKind::SetFixPoint => OpcodeKind::SetFixPoint,
            OpcodeKind::BuildFixPoint => OpcodeKind::BuildFixPoint,
            OpcodeKind::IO(v) => OpcodeKind::IO(v.clone()),
            OpcodeKind::Pandom => OpcodeKind::Pandom,
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Opcode<T> {
    fn collect(&self, _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {}
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Opcode<T> {}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for Opcode<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;

    fn into_dispatcher(self) -> Type<T> {
        Type::Opcode(self)
    }

    fn as_ref_dispatcher<'a>(&'a self) -> Self::RefDispatcher<'a> {
        TypeRef::Opcode(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for Opcode<T> {
    fn check(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        ctx.pattern_collector.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.instance_assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.collected_bindings,
            );
            match other {
                TypeRef::All(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Any(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Constraint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::SubOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Opcode(v) => match (&self.kind, &v.kind) {
                    (OpcodeKind::Add, OpcodeKind::Opcode)
                    | (OpcodeKind::Sub, OpcodeKind::Opcode)
                    | (OpcodeKind::Mul, OpcodeKind::Opcode)
                    | (OpcodeKind::Div, OpcodeKind::Opcode)
                    | (OpcodeKind::Mod, OpcodeKind::Opcode)
                    | (OpcodeKind::Less, OpcodeKind::Opcode)
                    | (OpcodeKind::Greater, OpcodeKind::Opcode)
                    | (OpcodeKind::Is, OpcodeKind::Opcode)
                    | (OpcodeKind::Neg, OpcodeKind::Opcode)
                    | (OpcodeKind::SetFixPoint, OpcodeKind::Opcode)
                    | (OpcodeKind::BuildFixPoint, OpcodeKind::Opcode)
                    | (OpcodeKind::IO(_), OpcodeKind::Opcode)
                    | (OpcodeKind::Pandom, OpcodeKind::Opcode) => Ok(ThreeValuedLogic::True),
                    _ => Ok(ThreeValuedLogic::False),
                },
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
                ctx.instance_assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.collected_bindings,
            );
            match other {
                TypeRef::Any(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::All(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Opcode(v) => Ok(match (&self.kind, &v.kind) {
                    (OpcodeKind::Opcode, OpcodeKind::Opcode)
                    | (OpcodeKind::Add, OpcodeKind::Add)
                    | (OpcodeKind::Sub, OpcodeKind::Sub)
                    | (OpcodeKind::Mul, OpcodeKind::Mul)
                    | (OpcodeKind::Div, OpcodeKind::Div)
                    | (OpcodeKind::Mod, OpcodeKind::Mod)
                    | (OpcodeKind::Less, OpcodeKind::Less)
                    | (OpcodeKind::Greater, OpcodeKind::Greater)
                    | (OpcodeKind::Is, OpcodeKind::Is)
                    | (OpcodeKind::Neg, OpcodeKind::Neg)
                    | (OpcodeKind::SetFixPoint, OpcodeKind::SetFixPoint)
                    | (OpcodeKind::BuildFixPoint, OpcodeKind::BuildFixPoint) => {
                        ThreeValuedLogic::True
                    }
                    (OpcodeKind::IO(a), OpcodeKind::IO(b)) => (a == b).into(),
                    (OpcodeKind::Pandom, OpcodeKind::Pandom) => ThreeValuedLogic::True,
                    _ => ThreeValuedLogic::False,
                }),
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
        let self_clone = self.clone();
        ctx.arg
            .take(&mut FastCycleDetector::new(), |_, arg| match &self.kind {
                OpcodeKind::Opcode => Err(TypeError::NonApplicableType(self.dispatch().into())),
                OpcodeKind::Assign => {
                    if let Type::Sequence(tuple) = &arg && tuple.is_tuple(){
                        if tuple.len() == 2 {
                            let left = tuple.get_prefix_value(0).unwrap();
                            let right = tuple.get_prefix_value(1).unwrap();
                            left.map(&mut FastCycleDetector::new(), |_, left_val| {
                                match left_val {
                                    TypeRef::Mutable(var) => {
                                        var.assign(right)?;
                                        Ok(Sequence::unit(ctx.source_info.cloned()))
                                    }
                                    _ => Err(TypeError::TypeMismatch(
                                        (left_val.clone_data().dispatch(), "Mutable".into()).into(),
                                    )),
                                }
                            })?.unwrap_or(Err(TypeError::UnresolvableType(
                                self.dispatch().into(),
                            )))
                        } else {
                            Err(TypeError::TypeMismatch(
                                (tuple.clone().dispatch(), "(Mutable, Any)".into()).into(),
                            ))
                        }
                    } else {
                        Err(TypeError::TypeMismatch(
                            (arg, "Finite Sequence".into()).into(),
                        ))
                    }
                }
                OpcodeKind::SetFixPoint => {
                    if let Type::Sequence(tuple) = &arg && tuple.is_tuple(){
                        if tuple.len() == 2 {
                            let left = tuple.get_prefix_value(0).unwrap();
                            let right = tuple.get_prefix_value(1).unwrap();
                            match left {
                                Type::FixPoint(fixpoint) => {
                                    fixpoint.set(right)?;
                                    Ok(fixpoint.clone().dispatch())
                                }
                                _ => Err(TypeError::TypeMismatch(
                                    (left.clone().dispatch(), "FixPoint".into()).into(),
                                )),
                            }
                        } else {
                            Err(TypeError::TypeMismatch(
                                (tuple.clone().dispatch(), "(FixPoint, Any)".into()).into(),
                            ))
                        }
                    } else {
                        Err(TypeError::TypeMismatch(
                            (arg, "Finite Sequence".into()).into(),
                        ))
                    }
                }
                OpcodeKind::BuildFixPoint => {
                    let place_holder = FixPoint::new_placeholder(ctx.gc, ctx.roots);
                    let invoke = Invoke::new(
                            Self::new(OpcodeKind::SetFixPoint, None).dispatch(),
                            Sequence::new_tuple(vec![place_holder.clone(), Variable::new_argument("var#fixpoint", None)], None),
                            None::<Type<T>>,
                            None::<Type<T>>,None);

                    let call_back: Type<T> = Closure::lazy(None, "var#fixpoint", invoke, ctx.environment)?;
                    Ok(Invoke::new(arg, place_holder, Some(call_back), None::<Type<_>>, ctx.source_info.cloned()))
                }
                OpcodeKind::IO(v) => Err(TypeError::RuntimeError(std::sync::Arc::new(
                    std::io::Error::other(
                        format!("Unhandled IO operation: {}", v),
                    ),
                ))),
                OpcodeKind::Neg => {
                    if let Type::FloatValue(n) = arg {
                        Ok(FloatValue::new(-n.value(), ctx.source_info.cloned()))
                    } else {
                        Err(TypeError::TypeMismatch(
                            (arg, "FloatValue".into()).into(),
                        ))
                    }
                }
                OpcodeKind::Is => {
                    if let Type::Sequence(tuple) = &arg && tuple.is_tuple() {
                        if tuple.len() == 4 {
                            let left = tuple.get_prefix_value(0).unwrap();
                            let right = tuple.get_prefix_value(1).unwrap();
                            let true_branch = tuple.get_prefix_value(2).unwrap();
                            let false_branch = tuple.get_prefix_value(3).unwrap();
                            let mut assumptions = smallvec::SmallVec::new();
                            let mut env_stack = EnvironmentStack::new();
                            let mut type_check_ctx = TypeCheckContext::new(
                                &mut assumptions,
                                PatternCollector::None,
                                ctx.environment,
                                ctx.environment,
                                &mut env_stack
                            );
                            match left.check(right.as_ref_dispatcher(), &mut type_check_ctx) {
                                Ok(res) => Ok(if let ThreeValuedLogic::True = res { true_branch.clone() } else { false_branch.clone() }),
                                Err(e) => Err(e),
                            }
                        } else {
                            Err(TypeError::TypeMismatch(
                                (
                                    tuple.clone().dispatch(),
                                    "(Value, Type, TrueCase, FalseCase)".into()
                                )
                                    .into(),
                            ))
                        }
                    } else {
                        Err(TypeError::TypeMismatch(
                            (arg, "Finite Sequence".into()).into(),
                        ))
                    }
                }
                OpcodeKind::Add
                | OpcodeKind::Sub
                | OpcodeKind::Mul
                | OpcodeKind::Div
                | OpcodeKind::Mod => {
                    if let Type::Sequence(tuple) = &arg && tuple.is_tuple() {
                        if tuple.len() == 2 {
                            let left = tuple.get_prefix_value(0).unwrap();
                            let right = tuple.get_prefix_value(1).unwrap();
                        match (left, right) {
                            (Type::NaturalNumber(l), Type::NaturalNumber(r)) => match &self.kind {
                                OpcodeKind::Add => Ok(NaturalNumber::new(l.value().wrapping_add(r.value()), ctx.source_info.cloned())),
                                OpcodeKind::Sub => Ok(NaturalNumber::new(l.value().wrapping_sub(r.value()), ctx.source_info.cloned())),
                                OpcodeKind::Mul => Ok(NaturalNumber::new(l.value().wrapping_mul(r.value()), ctx.source_info.cloned())),
                                OpcodeKind::Div => {
                                    if r.value() == 0 {
                                        Err(TypeError::TypeMismatch(
                                            (r.clone().dispatch(), "Non-zero nature number".into()).into(),
                                        ))
                                    } else {
                                        Ok(NaturalNumber::new(l.value() / r.value(), ctx.source_info.cloned()))
                                    }
                                }
                                OpcodeKind::Mod => {
                                    if r.value() == 0 {
                                        Err(TypeError::TypeMismatch(
                                            (r.clone().dispatch(), "Non-zero nature number".into()).into(),
                                        ))
                                    } else {
                                        Ok(NaturalNumber::new(l.value() % r.value(), ctx.source_info.cloned()))
                                    }
                                }
                                _ => unreachable!(),
                            },
                            (Type::FloatValue(l), Type::FloatValue(r)) => match &self.kind {
                                OpcodeKind::Add => Ok(FloatValue::new(l.value() + r.value(), ctx.source_info.cloned())),
                                OpcodeKind::Sub => Ok(FloatValue::new(l.value() - r.value(), ctx.source_info.cloned())),
                                OpcodeKind::Mul => Ok(FloatValue::new(l.value() * r.value(), ctx.source_info.cloned())),
                                OpcodeKind::Div => {
                                    if r.value() == 0.0 {
                                        Err(TypeError::TypeMismatch(
                                            (r.clone().dispatch(), "Non-zero float".into()).into(),
                                        ))
                                    } else {
                                        Ok(FloatValue::new(l.value() / r.value(), ctx.source_info.cloned()))
                                    }
                                }
                                OpcodeKind::Mod => {
                                    if r.value() == 0.0 {
                                        Err(TypeError::TypeMismatch(
                                            (r.clone().dispatch(), "Non-zero float".into()).into(),
                                        ))
                                    } else {
                                        Ok(FloatValue::new(l.value() % r.value(), ctx.source_info.cloned()))
                                    }
                                }
                                _ => unreachable!(),
                            },
                            (Type::Sequence(l), Type::Sequence(r)) => match &self.kind {
                                OpcodeKind::Add => Ok(l.add(r, ctx.environment)?.dispatch()),
                                _ => Err(TypeError::RuntimeError(std::sync::Arc::new(
                                    std::io::Error::other(
                                        "Only 'Add' operation is supported for Sequence types",
                                    ),
                                ))),
                            },
                            (Type::Closure(l), Type::Closure(r)) => match &self.kind {
                                OpcodeKind::Add => Ok(l.clone().impls(r.clone(), ctx.source_info.cloned())?),
                                _ => Err(TypeError::RuntimeError(std::sync::Arc::new(
                                    std::io::Error::other(
                                        "Only 'Add' operation is supported for Closure types",
                                    ),
                                ))),
                            },
                            (l, r) => Err(TypeError::TypeMismatch(
                                (
                                    Sequence::new_tuple(vec![l, r], self.source_info.clone()),
                                    "(Finite Sequence, Finite Sequence) | (FloatValue, FloatValue) | (Closure, Closure) | (Tuple, Tuple)".into()
                                )
                                    .into(),
                            )),
                        }
                        } else {
                            Err(TypeError::TypeMismatch(
                                (tuple.clone().dispatch(), "Finite Sequence".into()).into(),
                            ))
                        }
                    } else {
                        Err(TypeError::TypeMismatch(
                            (arg, "Finite Sequence".into()).into(),
                        ))
                    }
                }
                OpcodeKind::Less | OpcodeKind::Greater => {
                    if let Type::Sequence(tuple) = &arg && tuple.is_tuple() {
                        if tuple.len() == 4 {
                            let left = tuple.get_prefix_value(0).unwrap();
                            let right = tuple.get_prefix_value(1).unwrap();
                            let true_branch = tuple.get_prefix_value(2).unwrap();
                            let false_branch = tuple.get_prefix_value(3).unwrap();
                            match (left, right) {
                                (Type::Sequence(l), Type::Sequence(r)) if l.is_tuple() && r.is_tuple()=> {
                                    let condition = match &self.kind {
                                        OpcodeKind::Less => l.len() < r.len(),
                                        OpcodeKind::Greater => l.len() > r.len(),
                                        _ => unreachable!(),
                                    };
                                    let result = if condition {
                                        true_branch
                                    } else {
                                        false_branch
                                    };
                                    Ok(result.clone())
                                }
                                (Type::NaturalNumber(l), Type::NaturalNumber(r)) => {
                                    let condition = match &self.kind {
                                        OpcodeKind::Less => l.value() < r.value(),
                                        OpcodeKind::Greater => l.value() > r.value(),
                                        _ => unreachable!(),
                                    };
                                    let result = if condition {
                                        true_branch
                                    } else {
                                        false_branch
                                    };
                                    Ok(result.clone())
                                }
                                (Type::FloatValue(l), Type::FloatValue(r)) => {
                                    let condition = match &self.kind {
                                        OpcodeKind::Less => l.value() < r.value(),
                                        OpcodeKind::Greater => l.value() > r.value(),
                                        _ => unreachable!(),
                                    };
                                    let result = if condition {
                                        true_branch
                                    } else {
                                        false_branch
                                    };
                                    Ok(result.clone())
                                }
                                (l, r) => Err(TypeError::TypeMismatch(
                                    (
                                        Sequence::new_tuple(vec![l, r], self.source_info.clone()),
                                        "(Finite Sequence, Finite Sequence, Any, Any) | (FloatValue, FloatValue, Any, Any) | (NaturalNumber, NaturalNumber, Any, Any)".into()
                                    )
                                        .into(),
                                )),
                            }
                        } else {
                            Err(TypeError::TypeMismatch(
                                (
                                    tuple.clone().dispatch(),
                                    "(Value, Value, TrueCase, FalseCase)".into()
                                )
                                    .into(),
                            ))
                        }
                    } else {
                        Err(TypeError::TypeMismatch(
                            (arg, "Finite Sequence".into()).into(),
                        ))
                    }
                }
                OpcodeKind::Pandom => {
                    unreachable!()
                }
            })?.unwrap_or(Err(TypeError::UnresolvableType(self_clone.dispatch().into())))
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        self.source_info.as_ref()
    }

    fn report_source_info(&self) -> crate::types::TypeReport {
        if let Some(loc) = &self.source_info {
            let span = loc.span().clone();
            let filepath = loc.source().filepath().to_string();
            ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                .with_message(format!("Opcode type at {}", filepath))
                .with_label(
                    ariadne::Label::new((filepath, span)).with_message("Opcode defined here"),
                )
                .finish()
        } else {
            ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                .with_message("Opcode type has no source location")
                .with_label(
                    ariadne::Label::new(("<unknown>".to_string(), 0..0))
                        .with_message("Location unknown"),
                )
                .finish()
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Opcode<T> {
    fn represent(
        &self,
        _path: &mut FastCycleDetector<TaggedPtr<()>>,
        _depth: usize,
        _max_depth: usize,
    ) -> String {
        match &self.kind {
            OpcodeKind::Opcode => "Opcode".to_string(),
            OpcodeKind::Add => "Add".to_string(),
            OpcodeKind::Sub => "Sub".to_string(),
            OpcodeKind::Mul => "Mul".to_string(),
            OpcodeKind::Div => "Div".to_string(),
            OpcodeKind::Mod => "Mod".to_string(),
            OpcodeKind::Less => "Less".to_string(),
            OpcodeKind::Greater => "Greater".to_string(),
            OpcodeKind::Neg => "Neg".to_string(),
            OpcodeKind::Is => "Is".to_string(),
            OpcodeKind::Assign => "Assign".to_string(),
            OpcodeKind::SetFixPoint => "SetFixPoint".to_string(),
            OpcodeKind::BuildFixPoint => "BuildFixPoint".to_string(),
            OpcodeKind::IO(v) => format!("IO({})", v),
            OpcodeKind::Pandom => "Pandom".to_string(),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Opcode<T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(kind: OpcodeKind, source_info: Option<Arc<SourceLocation>>) -> Type<T> {
        Self { kind, source_info, _phantom: std::marker::PhantomData }.dispatch()
    }
}
