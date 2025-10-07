use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        CoinductiveType, CoinductiveTypeWithAny, InvokeContext, ReductionContext, Representable,
        Rootable, StabilizedType, Type, TypeCheckContext, TypeError, closure::ClosureEnv,
        fixpoint::FixPointInner, integer_value::IntegerValue, type_bound::TypeBound,
    },
    util::{collector::Collector, cycle_detector::FastCycleDetector},
};

#[derive(Debug, Clone)]
pub enum Opcode {
    Opcode, // 所有操作码的超类型
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Less,
    Greater,
    Is,
}

impl GCTraceable<FixPointInner> for Opcode {
    fn collect(
        &self,
        _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<FixPointInner>>,
    ) {
    }
}

impl Rootable for Opcode {}

impl CoinductiveType<Type, StabilizedType> for Opcode {
    fn dispatch(self) -> Type {
        Type::Opcode(self)
    }

    fn is(&self, other: &Type, ctx: &mut TypeCheckContext) -> Result<Option<()>, TypeError> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.assumptions,
                ctx.closure_env,
                pattern_env,
                ctx.pattern_mode,
            );
            match other {
                Type::Opcode(Opcode::Opcode) => Ok(Some(())),
                Type::Opcode(v) => Ok(
                    if std::mem::discriminant(self) == std::mem::discriminant(v) {
                        Some(())
                    } else {
                        None
                    },
                ),
                Type::Bound(TypeBound::Top) => Ok(Some(())),
                Type::Specialize(v) => v.has(self, &mut inner_ctx),
                Type::Generalize(v) => v.has(self, &mut inner_ctx),
                Type::FixPoint(v) => v.has(self, &mut inner_ctx),
                Type::Pattern(v) => v.has(self, &mut inner_ctx),
                Type::Variable(v) => v.has(self, &mut inner_ctx),
                _ => Ok(None),
            }
        })
    }

    fn reduce(&self, _ctx: &mut ReductionContext) -> Result<StabilizedType, TypeError> {
        Ok(self.clone().dispatch().stabilize())
    }

    fn invoke(&self, ctx: &mut InvokeContext) -> Result<StabilizedType, TypeError> {
        match self {
            Opcode::Opcode => Err(TypeError::NonApplicableType(
                self.clone().dispatch().stabilize().into(),
            )),
            Opcode::Is => {
                if let Type::Tuple(tuple) = ctx.arg {
                    if tuple.len() == 2 {
                        let left = &tuple.types()[0];
                        let right = &tuple.types()[1];
                        let empty_closure = ClosureEnv::new(Vec::<Type>::new());
                        let mut assumptions = smallvec::SmallVec::new();
                        let mut pattern_env = Collector::new();
                        let mut type_check_ctx = TypeCheckContext::new(
                            &mut assumptions,
                            (&empty_closure, &empty_closure),
                            &mut pattern_env,
                            false,
                        );
                        match left.is(right, &mut type_check_ctx) {
                            Ok(res) => Ok(if res.is_some() {
                                TypeBound::top()
                            } else {
                                TypeBound::bottom()
                            }),
                            Err(e) => Err(e),
                        }
                    } else {
                        Err(TypeError::TypeMismatch(
                            ctx.arg.clone().stabilize().into(),
                            "(Any, Any)".to_string(),
                        ))
                    }
                } else {
                    Err(TypeError::TypeMismatch(
                        ctx.arg.clone().stabilize().into(),
                        "Tuple".to_string(),
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
                if let Type::Tuple(tuple) = ctx.arg {
                    if tuple.len() == 2 {
                        let left = &tuple.types()[0];
                        let right = &tuple.types()[1];
                        match (left, right) {
                            (Type::IntegerValue(l), Type::IntegerValue(r)) => match self {
                                Opcode::Add => Ok(IntegerValue::new(l.value() + r.value())),
                                Opcode::Sub => Ok(IntegerValue::new(l.value() - r.value())),
                                Opcode::Mul => Ok(IntegerValue::new(l.value() * r.value())),
                                Opcode::Div => {
                                    if r.value() == 0 {
                                        Err(TypeError::TypeMismatch(
                                            ctx.arg.clone().stabilize().into(),
                                            "Non-zero".to_string(),
                                        ))
                                    } else {
                                        Ok(IntegerValue::new(l.value() / r.value()))
                                    }
                                }
                                Opcode::Mod => {
                                    if r.value() == 0 {
                                        Err(TypeError::TypeMismatch(
                                            ctx.arg.clone().stabilize().into(),
                                            "Non-zero".to_string(),
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
                            // (Type::Tuple(l), Type::Tuple(r)) => {

                            // }
                            _ => Err(TypeError::TypeMismatch(
                                ctx.arg.clone().stabilize().into(),
                                "(IntegerValue, IntegerValue)".to_string(),
                            )),
                        }
                    } else {
                        Err(TypeError::TypeMismatch(
                            ctx.arg.clone().stabilize().into(),
                            "Tuple".to_string(),
                        ))
                    }
                } else {
                    Err(TypeError::TypeMismatch(
                        ctx.arg.clone().stabilize().into(),
                        "Tuple".to_string(),
                    ))
                }
            }
        }
    }
}

impl Representable for Opcode {
    fn represent(&self, _path: &mut FastCycleDetector<*const ()>) -> String {
        format!("{:?}", self)
    }
}

impl Opcode {
    pub fn new(op: Opcode) -> StabilizedType {
        op.dispatch().stabilize()
    }
}
