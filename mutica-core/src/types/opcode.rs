use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        closure::{Closure, ClosureEnv}, fixpoint::FixPoint, float_value::FloatValue, integer_value::IntegerValue, invoke::Invoke, pattern::Pattern, tuple::Tuple, type_bound::TypeBound, variable::Variable, AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext, ReductionContext, Representable, Rootable, TaggedPtr, Type, TypeCheckContext, TypeError, TypeRef
    },
    util::{
        collector::Collector, cycle_detector::FastCycleDetector,
        three_valued_logic::ThreeValuedLogic,
    },
};

pub enum Opcode<T: GcAllocObject<T, Inner = Type<T>>> {
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
    Set,
    BuildFixPoint,
    // I/O
    IO(Box<String>),
    Pandom(std::marker::PhantomData<T>),
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Opcode<T> {
    fn clone(&self) -> Self {
        match self {
            Opcode::Opcode => Opcode::Opcode,
            Opcode::Add => Opcode::Add,
            Opcode::Sub => Opcode::Sub,
            Opcode::Mul => Opcode::Mul,
            Opcode::Div => Opcode::Div,
            Opcode::Mod => Opcode::Mod,
            Opcode::Less => Opcode::Less,
            Opcode::Greater => Opcode::Greater,
            Opcode::Is => Opcode::Is,
            Opcode::Neg => Opcode::Neg,
            Opcode::Set => Opcode::Set,
            Opcode::BuildFixPoint => Opcode::BuildFixPoint,
            Opcode::IO(v) => Opcode::IO(v.clone()),
            Opcode::Pandom(_) => Opcode::Pandom(std::marker::PhantomData),
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
    ) -> Result<bool, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.rhs);
            match other {
                TypeRef::Specialize(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Generalize(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::EqType(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Bound(TypeBound::Top) => Ok(true),
                TypeRef::Opcode(v) => match (self, v) {
                    (Opcode::Add, Opcode::Opcode) |
                    (Opcode::Sub, Opcode::Opcode) |
                    (Opcode::Mul, Opcode::Opcode) |
                    (Opcode::Div, Opcode::Opcode) |
                    (Opcode::Mod, Opcode::Opcode) |
                    (Opcode::Less, Opcode::Opcode) |
                    (Opcode::Greater, Opcode::Opcode) |
                    (Opcode::Is, Opcode::Opcode) |
                    (Opcode::Neg, Opcode::Opcode) |
                    (Opcode::Set, Opcode::Opcode) |
                    (Opcode::BuildFixPoint, Opcode::Opcode) |
                    (Opcode::IO(_), Opcode::Opcode) |
                    (Opcode::Pandom(_), Opcode::Opcode) => Ok(true),
                    _ => Ok(false),
                },
                TypeRef::OrderedType(v) if matches!(self, Opcode::Opcode) => Ok(v.level() == 0),
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
                TypeRef::Opcode(v) => Ok(match (self, v) {
                    (Opcode::Opcode, Opcode::Opcode) |
                    (Opcode::Add, Opcode::Add) |
                    (Opcode::Sub, Opcode::Sub) |
                    (Opcode::Mul, Opcode::Mul) |
                    (Opcode::Div, Opcode::Div) |
                    (Opcode::Mod, Opcode::Mod) |
                    (Opcode::Less, Opcode::Less) |
                    (Opcode::Greater, Opcode::Greater) |
                    (Opcode::Is, Opcode::Is) |
                    (Opcode::Neg, Opcode::Neg) |
                    (Opcode::Set, Opcode::Set) |
                    (Opcode::BuildFixPoint, Opcode::BuildFixPoint) => true,
                    (Opcode::IO(a), Opcode::IO(b)) => a == b,
                    (Opcode::Pandom(_), Opcode::Pandom(_)) => true,
                    _ => false,
                }),
                _ => Ok(false),
            }
        })
    }



    fn reduce(
        self,
        _ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Ok(self.dispatch())
    }

    fn invoke(
        &self,
        ctx: &mut InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        ctx.arg
            .map(&mut FastCycleDetector::new(), |_, arg| match self {
                Opcode::Opcode => Err(TypeError::NonApplicableType(self.clone().dispatch().into())),
                Opcode::Set => {
                    if let TypeRef::Tuple(tuple) = arg {
                        if tuple.len() == 2 {
                            let left = tuple.get(0).unwrap();
                            let right = tuple.get(1).unwrap();
                            match left {
                                Type::FixPoint(fixpoint) => {
                                    fixpoint.set(right)?;
                                    Ok(left.clone())
                                }
                                _ => Err(TypeError::TypeMismatch(
                                    (ctx.arg.clone(), "FixPoint".into()).into(),
                                )),
                            }
                        } else {
                            Err(TypeError::TypeMismatch(
                                (ctx.arg.clone(), "(FixPoint, Any)".into()).into(),
                            ))
                        }
                    } else {
                        Err(TypeError::TypeMismatch(
                            (ctx.arg.clone(), "Tuple".into()).into(),
                        ))
                    }
                }
                Opcode::BuildFixPoint => {
                    let place_holder = FixPoint::new_placeholder(ctx.gc, ctx.roots);
                    let call_back: Type<T> = Closure::new(
                        vec![(Pattern::new(0, TypeBound::<T>::top()), Invoke::new(
                            Opcode::Set.dispatch(), 
                            Tuple::new(vec![place_holder.clone(), Variable::new_debruijn(0)]), 
                            None::<Type<T>>, 
                            None::<Type<T>>),0)],
                        vec![ClosureEnv::new(Vec::<Type<T>>::new())]
                    );
                    Ok(Invoke::new(arg.clone_data(), place_holder, Some(call_back), None::<Type<_>>))
                }
                Opcode::IO(v) => Err(TypeError::RuntimeError(std::sync::Arc::new(
                    std::io::Error::other(
                        format!("Unhandled IO operation: {}", v),
                    ),
                ))),
                Opcode::Neg => {
                    if let TypeRef::IntegerValue(n) = arg {
                        Ok(IntegerValue::new(-n.value()))
                    } else if let TypeRef::FloatValue(n) = arg {
                        Ok(FloatValue::new(-n.value()))
                    } else {
                        Err(TypeError::TypeMismatch(
                            (ctx.arg.clone(), "IntegerValue | FloatValue".into()).into(),
                        ))
                    }
                }
                Opcode::Is => {
                    if let TypeRef::Tuple(tuple) = arg {
                        if tuple.len() == 2 {
                            let left = tuple.get(0).unwrap();
                            let right = tuple.get(1).unwrap();
                            let empty_closure = ClosureEnv::new(Vec::<Type<T>>::new());
                            let mut assumptions = smallvec::SmallVec::new();
                            let mut pattern_env = Collector::new_disabled();
                            let mut type_check_ctx = TypeCheckContext::new(
                                &mut assumptions,
                                (&empty_closure, &empty_closure),
                                &mut pattern_env,
                                false,
                            );
                            match left.check(right.as_ref_dispatcher(), &mut type_check_ctx) {
                                Ok(res) => Ok(if res {
                                    TypeBound::top()
                                } else {
                                    TypeBound::bottom()
                                }),
                                Err(e) => Err(e),
                            }
                        } else {
                            Err(TypeError::TypeMismatch(
                                (ctx.arg.clone(), "(Any, Any)".into()).into(),
                            ))
                        }
                    } else {
                        Err(TypeError::TypeMismatch(
                            (ctx.arg.clone(), "Tuple".into()).into(),
                        ))
                    }
                }
                Opcode::Add
                | Opcode::Sub
                | Opcode::Mul
                | Opcode::Div
                | Opcode::Mod
                | Opcode::Less
                | Opcode::Greater => {
                    if let TypeRef::Tuple(tuple) = arg {
                        if tuple.len() == 2 {
                            let left = tuple.get(0).unwrap();
                            let right = tuple.get(1).unwrap();
                            left.map(&mut FastCycleDetector::new(), |_, left| {
                                right.map(&mut FastCycleDetector::new(), |_, right| {
                                    match (left, right) {
                            (TypeRef::IntegerValue(l), TypeRef::IntegerValue(r)) => match self {
                                Opcode::Add => Ok(IntegerValue::new(l.value() + r.value())),
                                Opcode::Sub => Ok(IntegerValue::new(l.value() - r.value())),
                                Opcode::Mul => Ok(IntegerValue::new(l.value() * r.value())),
                                Opcode::Div => {
                                    if r.value() == 0 {
                                        Err(TypeError::TypeMismatch(
                                            (ctx.arg.clone(), "Non-zero integer".into()).into(),
                                        ))
                                    } else {
                                        Ok(IntegerValue::new(l.value() / r.value()))
                                    }
                                }
                                Opcode::Mod => {
                                    if r.value() == 0 {
                                        Err(TypeError::TypeMismatch(
                                            (ctx.arg.clone(), "Non-zero integer".into()).into(),
                                        ))
                                    } else {
                                        Ok(IntegerValue::new(l.value() % r.value()))
                                    }
                                }
                                Opcode::Less => Ok(if l.value() < r.value() {
                                    TypeBound::top()
                                } else {
                                    TypeBound::bottom()
                                }),
                                Opcode::Greater => Ok(if l.value() > r.value() {
                                    TypeBound::top()
                                } else {
                                    TypeBound::bottom()
                                }),
                                _ => unreachable!(),
                            },
                            (TypeRef::FloatValue(l), TypeRef::FloatValue(r)) => match self {
                                Opcode::Add => Ok(FloatValue::new(l.value() + r.value())),
                                Opcode::Sub => Ok(FloatValue::new(l.value() - r.value())),
                                Opcode::Mul => Ok(FloatValue::new(l.value() * r.value())),
                                Opcode::Div => {
                                    if r.value() == 0.0 {
                                        Err(TypeError::TypeMismatch(
                                            (ctx.arg.clone(), "Non-zero float".into()).into(),
                                        ))
                                    } else {
                                        Ok(FloatValue::new(l.value() / r.value()))
                                    }
                                }
                                Opcode::Mod => {
                                    if r.value() == 0.0 {
                                        Err(TypeError::TypeMismatch(
                                            (ctx.arg.clone(), "Non-zero float".into()).into(),
                                        ))
                                    } else {
                                        Ok(FloatValue::new(l.value() % r.value()))
                                    }
                                }
                                Opcode::Less => Ok(if l.value() < r.value() {
                                    TypeBound::top()
                                } else {
                                    TypeBound::bottom()
                                }),
                                Opcode::Greater => Ok(if l.value() > r.value() {
                                    TypeBound::top()
                                } else {
                                    TypeBound::bottom()
                                }),
                                _ => unreachable!(),
                            },
                            (TypeRef::Closure(l), TypeRef::Closure(r)) => match self {
                                Opcode::Add => Ok(l.impls(r)),
                                _ => Err(TypeError::RuntimeError(std::sync::Arc::new(
                                    std::io::Error::other(
                                        "Only 'Add' operation is supported for Closure types",
                                    ),
                                ))),
                            },
                            (TypeRef::Tuple(l), TypeRef::Tuple(r)) => match self {
                                Opcode::Add => Ok(l.concat(r)),
                                _ => Err(TypeError::RuntimeError(std::sync::Arc::new(
                                    std::io::Error::other(
                                        "Only 'Add' operation is supported for Tuple types",
                                    ),
                                ))),
                            },
                            _ => Err(TypeError::TypeMismatch(
                                (
                                    ctx.arg.clone(),
                                    "(IntegerValue, IntegerValue) | (FloatValue, FloatValue) | (Closure, Closure) | (Tuple, Tuple)".into()
                                )
                                    .into(),
                            )),
                        }
                                })?.unwrap_or(Err(TypeError::UnresolvableType("Could not resolve right argument".into())))
                            })?.unwrap_or(Err(TypeError::UnresolvableType("Could not resolve left argument".into())))
                        } else {
                            Err(TypeError::TypeMismatch(
                                (ctx.arg.clone(), "Tuple".into()).into(),
                            ))
                        }
                    } else {
                        Err(TypeError::TypeMismatch(
                            (ctx.arg.clone(), "Tuple".into()).into(),
                        ))
                    }
                }
                Opcode::Pandom(_) => {
                    unreachable!()
                }
            })?.unwrap_or(Err(TypeError::UnresolvableType("Could not resolve argument".into())))
    }

    fn is_normal_form(&self) -> ThreeValuedLogic {
        ThreeValuedLogic::True
    }

    fn recalculate_normal_form(&self, _: &mut FastCycleDetector<TaggedPtr<()>>) {}
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Opcode<T> {
    fn represent(&self, _path: &mut FastCycleDetector<TaggedPtr<()>>) -> String {
        match self {
            Opcode::Opcode => "Opcode".to_string(),
            Opcode::Add => "Add".to_string(),
            Opcode::Sub => "Sub".to_string(),
            Opcode::Mul => "Mul".to_string(),
            Opcode::Div => "Div".to_string(),
            Opcode::Mod => "Mod".to_string(),
            Opcode::Less => "Less".to_string(),
            Opcode::Greater => "Greater".to_string(),
            Opcode::Neg => "Neg".to_string(),
            Opcode::Is => "Is".to_string(),
            Opcode::Set => "Set".to_string(),
            Opcode::BuildFixPoint => "InjectFixPointPlaceholder".to_string(),
            Opcode::IO(v) => format!("IO({})", v),
            Opcode::Pandom(_) => "Pandom".to_string(),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Opcode<T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(op: Opcode<T>) -> Type<T> {
        op.dispatch()
    }
}
