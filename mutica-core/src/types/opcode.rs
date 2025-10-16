use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, Type, TypeCheckContext, TypeError, TypeRef,
        closure::ClosureEnv, integer_value::IntegerValue, type_bound::TypeBound,
    },
    util::{collector::Collector, cycle_detector::FastCycleDetector},
};

#[derive(Debug, Clone)]
pub enum Opcode {
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
    // I/O
    IO(Box<String>),
}

impl<T: GcAllocObject<T>> GCTraceable<T> for Opcode {
    fn collect(&self, _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {}
}

impl<T: GcAllocObject<T>> GcAllocObject<T> for Opcode {
    type Inner = Type<T>;
}

impl<T: GcAllocObject<T>> Rootable<T> for Opcode {}

impl<T: GcAllocObject<T>> AsDispatcher<Type<T>, T> for Opcode {
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

impl<T: GcAllocObject<T>> CoinductiveType<Type<T>, T> for Opcode {
    fn is(
        &self,
        other: &Type<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<Option<()>, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env);
            match other {
                Type::Opcode(Opcode::Opcode) => Ok(Some(())),
                Type::Opcode(v) => match (self, v) {
                    (Opcode::Opcode, _) => Ok(Some(())),
                    (_, Opcode::Opcode) => Ok(Some(())),
                    (Opcode::Add, Opcode::Add)
                    | (Opcode::Sub, Opcode::Sub)
                    | (Opcode::Mul, Opcode::Mul)
                    | (Opcode::Div, Opcode::Div)
                    | (Opcode::Mod, Opcode::Mod)
                    | (Opcode::Less, Opcode::Less)
                    | (Opcode::Greater, Opcode::Greater)
                    | (Opcode::Is, Opcode::Is) => Ok(Some(())),
                    (Opcode::IO(a), Opcode::IO(b)) => Ok(if a == b { Some(()) } else { None }),
                    _ => Ok(None),
                },
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
        match self {
            Opcode::Opcode => Err(TypeError::NonApplicableType(self.clone().dispatch().into())),
            Opcode::IO(v) => Err(TypeError::RuntimeError(std::sync::Arc::new(
                std::io::Error::new(
                    std::io::ErrorKind::Other,
                    format!("Unhandled IO operation: {}", v),
                ),
            ))),
            // Opcode::Input => {
            //     // 从stdin读取一行输入，返回List<CharacterValue>类型
            //     let mut input = String::new();
            //     std::io::stdin()
            //         .read_line(&mut input)
            //         .map_err(|e| TypeError::RuntimeError(std::sync::Arc::new(e)))?;
            //     let chars = input
            //         .chars()
            //         .map(|c| CharacterValue::new(c))
            //         .collect::<Vec<_>>();
            //     Ok(List::new(chars))
            // }
            // Opcode::Print => {
            //     // 打印参数
            //     print!("{}", ctx.arg.display(&mut FastCycleDetector::new()));
            //     Ok(Tuple::new(Vec::<Type>::new()))
            // }
            Opcode::Is => {
                if let Type::Tuple(tuple) = ctx.arg {
                    if tuple.len() == 2 {
                        let left = &tuple.types()[0];
                        let right = &tuple.types()[1];
                        let empty_closure = ClosureEnv::new(Vec::<Type<T>>::new());
                        let mut assumptions = smallvec::SmallVec::new();
                        let mut pattern_env = Collector::new_disabled();
                        let mut type_check_ctx = TypeCheckContext::new(
                            &mut assumptions,
                            (&empty_closure, &empty_closure),
                            &mut pattern_env,
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
                                            (ctx.arg.clone(), "Non-zero".into()).into(),
                                        ))
                                    } else {
                                        Ok(IntegerValue::new(l.value() / r.value()))
                                    }
                                }
                                Opcode::Mod => {
                                    if r.value() == 0 {
                                        Err(TypeError::TypeMismatch(
                                            (ctx.arg.clone(), "Non-zero".into()).into(),
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
                                (ctx.arg.clone(), "(IntegerValue, IntegerValue)".into()).into(),
                            )),
                        }
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
        }
    }

    fn is_normal_form(&self) -> bool {
        true
    }
}

impl Representable for Opcode {
    fn represent(&self, _path: &mut FastCycleDetector<*const ()>) -> String {
        format!("{:?}", self)
    }
}

impl Opcode {
    pub fn new<T: GcAllocObject<T>>(op: Opcode) -> Type<T> {
        op.dispatch()
    }
}
