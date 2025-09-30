use arc_gc::{gc::GC, traceable::GCTraceable};

use crate::{
    types::{
        CoinductiveType, CoinductiveTypeWithAny, Representable, Rootable, StabilizedType,
        TaggedPtr, Type, TypeError,
        closure::{ClosureEnv, ParamEnv},
        fixpoint::FixPointInner,
        integer_value::IntegerValue,
        type_bound::TypeBound,
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

    fn is(
        &self,
        other: &Type,
        assumptions: &mut smallvec::SmallVec<[(TaggedPtr<()>, TaggedPtr<()>); 8]>,
        closure_env: (&ClosureEnv, &ClosureEnv),
        pattern_env: &mut Collector<(usize, Type)>,
        pattern_mode: bool,
    ) -> Result<Option<()>, super::TypeError> {
        pattern_env.collect(|pattern_env| match other {
            Type::Opcode(Opcode::Opcode) => Ok(Some(())),
            Type::Opcode(v) => Ok(
                if std::mem::discriminant(self) == std::mem::discriminant(v) {
                    Some(())
                } else {
                    None
                },
            ),
            Type::Bound(TypeBound::Top) => Ok(Some(())),
            Type::Specialize(v) => v.has(self, assumptions, closure_env, pattern_env, pattern_mode),
            Type::Generalize(v) => v.has(self, assumptions, closure_env, pattern_env, pattern_mode),
            Type::FixPoint(v) => v.has(self, assumptions, closure_env, pattern_env, pattern_mode),
            Type::Pattern(v) => v.has(self, assumptions, closure_env, pattern_env, pattern_mode),
            Type::Variable(v) => v.has(self, assumptions, closure_env, pattern_env, pattern_mode),
            _ => Ok(None),
        })
    }

    fn reduce(
        &self,
        _v: &ClosureEnv,
        _p: &ParamEnv,
        _rec_assumptions: &mut smallvec::SmallVec<[(TaggedPtr<()>, Type, bool); 8]>,
        _gc: &mut GC<FixPointInner>,
    ) -> Result<StabilizedType, TypeError> {
        Ok(self.clone().dispatch().stabilize())
    }

    fn apply(
        &self,
        v: &Type,
        _context: &ClosureEnv,
        _p: &ParamEnv,
        _rec_assumptions: &mut smallvec::SmallVec<[(TaggedPtr<()>, Type, bool); 8]>,
        _gc: &mut GC<FixPointInner>,
    ) -> Result<StabilizedType, TypeError> {
        match self {
            Opcode::Opcode => Err(TypeError::NonApplicableType(
                self.clone().dispatch().stabilize().into(),
            )),
            Opcode::Is => {
                if let Type::Tuple(tuple) = v {
                    if tuple.len() == 2 {
                        let left = &tuple.types()[0];
                        let right = &tuple.types()[1];
                        let empty_closure = ClosureEnv::new(Vec::<Type>::new());
                        match left.is(
                            right,
                            &mut smallvec::SmallVec::new(),
                            (&empty_closure, &empty_closure),
                            &mut Collector::new(),
                            false,
                        ) {
                            Ok(res) => Ok(if res.is_some() {
                                TypeBound::top()
                            } else {
                                TypeBound::bottom()
                            }),
                            Err(e) => Err(e),
                        }
                    } else {
                        Err(TypeError::TypeMismatch(
                            v.clone().stabilize().into(),
                            "(Any, Any)".to_string(),
                        ))
                    }
                } else {
                    Err(TypeError::TypeMismatch(
                        v.clone().stabilize().into(),
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
                if let Type::Tuple(tuple) = v {
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
                                            v.clone().stabilize().into(),
                                            "Non-zero".to_string(),
                                        ))
                                    } else {
                                        Ok(IntegerValue::new(l.value() / r.value()))
                                    }
                                }
                                Opcode::Mod => {
                                    if r.value() == 0 {
                                        Err(TypeError::TypeMismatch(
                                            v.clone().stabilize().into(),
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
                                v.clone().stabilize().into(),
                                "(IntegerValue, IntegerValue)".to_string(),
                            )),
                        }
                    } else {
                        Err(TypeError::TypeMismatch(
                            v.clone().stabilize().into(),
                            "Tuple".to_string(),
                        ))
                    }
                } else {
                    Err(TypeError::TypeMismatch(
                        v.clone().stabilize().into(),
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
