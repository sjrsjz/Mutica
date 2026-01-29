pub mod stack;

use std::{future::Future, io::Write, pin::Pin, sync::Arc};

use arc_gc::gc::GC;

use crate::{
    scheduler::stack::{Stack, StackView},
    types::{
        AsDispatcher, CoinductiveType, GcAllocObject, InvokeContext, Representable, Type,
        TypeError, TypeRef,
        character_value::CharacterValue,
        closure::Closure,
        invoke::{Invoke, InvokeCountinuationStyle},
        natural_number::NaturalNumber,
        sequence::Sequence,
        unify::Environment,
        variable::Variable,
    },
    util::{cycle_detector::FastCycleDetector, rootstack::RootStack, source_info::SourceLocation},
};

pub enum ContinuationOrHandler<T: GcAllocObject<T, Inner = Type<T>>> {
    Continuation(Type<T>),
    PerformHandler(Type<T>),
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for ContinuationOrHandler<T> {
    fn clone(&self) -> Self {
        match self {
            ContinuationOrHandler::Continuation(v) => {
                ContinuationOrHandler::Continuation(v.clone())
            }
            ContinuationOrHandler::PerformHandler(v) => {
                ContinuationOrHandler::PerformHandler(v.clone())
            }
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> ContinuationOrHandler<T> {
    pub fn as_type(&self) -> &Type<T> {
        match self {
            ContinuationOrHandler::Continuation(v) => v,
            ContinuationOrHandler::PerformHandler(v) => v,
        }
    }
}

pub type AsyncIoHandler<T> = Box<
    dyn Fn(
            &Type<T>,
            &Type<T>,
        )
            -> Pin<Box<dyn Future<Output = Result<Option<Type<T>>, TypeError<Type<T>, T>>> + Send>>
        + Send
        + Sync,
>;

pub struct LinearScheduler<T: GcAllocObject<T, Inner = Type<T>>> {
    outer_io_handler: Option<AsyncIoHandler<T>>,
    cont_stack: Stack<ContinuationOrHandler<T>>,
    current_type: Option<Type<T>>,
    roots: RootStack<Type<T>, T>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> LinearScheduler<T> {
    pub fn new(initial_type: Type<T>, outer_io_handler: Option<AsyncIoHandler<T>>) -> Self {
        let mut roots = RootStack::new();
        roots.attach(&initial_type);
        Self { outer_io_handler, cont_stack: Stack::new(), current_type: Some(initial_type), roots }
    }

    async fn io(
        &mut self,
        f: &Type<T>,
        arg: &Type<T>,
        source_info: Option<&Arc<SourceLocation>>,
    ) -> Result<Option<Type<T>>, TypeError<Type<T>, T>> {
        if let Some(outer_handler) = &self.outer_io_handler
            && let Some(result) = outer_handler(f, arg).await?
        {
            return Ok(Some(result));
        }
        f.map(&mut FastCycleDetector::new(), |_, f| {
            if !matches!(f, TypeRef::Opcode(_)) {
                return Ok(None);
            }
            let TypeRef::Opcode(op) = f else { unreachable!() };
            if !matches!(&op.kind, crate::types::opcode::OpcodeKind::IO(_)) {
                return Ok(None);
            }
            let crate::types::opcode::OpcodeKind::IO(io_name) = &op.kind else { unreachable!() };
            match io_name.as_ref().as_str() {
                // 基本IO操作
                "print" => {
                    let str = arg.display(&mut FastCycleDetector::new(), 0, usize::MAX);
                    print!("{}", str);
                    Ok(Some(Sequence::unit(source_info.cloned())))
                }
                "println" => {
                    let str = arg.display(&mut FastCycleDetector::new(), 0, usize::MAX);
                    println!("{}", str);
                    Ok(Some(Sequence::unit(source_info.cloned())))
                }
                "input" => {
                    let mut input = String::new();
                    std::io::stdin().read_line(&mut input).unwrap();
                    let chars = input
                        .chars()
                        .map(|c| CharacterValue::new(c, source_info.cloned()))
                        .collect::<Vec<_>>();
                    Ok(Some(Sequence::new_tuple(chars, source_info.cloned())))
                }
                "flush" => {
                    use std::io;
                    io::stdout().flush().unwrap();
                    Ok(Some(Sequence::unit(source_info.cloned())))
                }
                // 类型表示相关
                "repr" => {
                    let repr = arg.represent(&mut FastCycleDetector::new(), 0, usize::MAX);
                    let chars = repr
                        .chars()
                        .map(|c| CharacterValue::new(c, source_info.cloned()))
                        .collect::<Vec<_>>();
                    Ok(Some(Sequence::new_tuple(chars, source_info.cloned())))
                }
                "display" => {
                    let disp = arg.display(&mut FastCycleDetector::new(), 0, usize::MAX);
                    let chars = disp
                        .chars()
                        .map(|c| CharacterValue::new(c, source_info.cloned()))
                        .collect::<Vec<_>>();
                    Ok(Some(Sequence::new_tuple(chars, source_info.cloned())))
                }
                // 代数效应相关
                "perform" => Err(TypeError::Perform(arg.clone().into())),
                // 类型结构描述相关
                "tuple_len" => arg
                    .map(&mut FastCycleDetector::new(), |_, arg| match arg {
                        TypeRef::Sequence(v) => {
                            Ok(Some(NaturalNumber::new(v.len(), source_info.cloned())))
                        }
                        _ => Err(TypeError::TypeMismatch(
                            (arg.clone_data(), "Tuple | List".into()).into(),
                        )),
                    })?
                    .unwrap_or(Err(TypeError::UnresolvableType(arg.clone().dispatch().into()))),
                "as_tuple" => arg
                    .map(&mut FastCycleDetector::new(), |_, arg| match arg {
                        TypeRef::Sequence(_) => Ok(Some(arg.clone_data())),
                        TypeRef::Any(v) => {
                            let mut elements = Vec::new();
                            for ty in v.types() {
                                elements.push(ty.clone());
                            }
                            Ok(Some(Sequence::new_tuple(elements, source_info.cloned())))
                        }
                        TypeRef::All(v) => {
                            let mut elements = Vec::new();
                            for ty in v.types() {
                                elements.push(ty.clone());
                            }
                            Ok(Some(Sequence::new_tuple(elements, source_info.cloned())))
                        }
                        _ => Err(TypeError::TypeMismatch(
                            (arg.clone_data(), "Tuple | List | AnyOf | AllOf".into()).into(),
                        )),
                    })?
                    .unwrap_or(Err(TypeError::UnresolvableType(arg.clone().dispatch().into()))),

                _ => Ok(None),
            }
        })?
        .unwrap_or(Err(TypeError::UnresolvableType(f.clone().dispatch().into())))
    }

    pub async fn step(&mut self, gc: &mut GC<T>) -> Result<bool, TypeError<Type<T>, T>> {
        let empty_env = Environment::placeholder();
        // 在 await 之前完成所有需要 rec_assumptions 的工作
        let current_type = self.current_type.take().ok_or_else(|| {
            TypeError::RuntimeError(Arc::new(std::io::Error::other("No current type to step")))
        })?;

        // 检查是否是 Invoke 类型
        let is_invoke = matches!(current_type.as_ref_dispatcher(), TypeRef::Invoke(_));

        let (next_type, updated) = if is_invoke {
            // 处理 Invoke 类型
            let (func, arg, continuation_style, source_info) =
                if let Type::Invoke(invoke) = current_type {
                    invoke.take()
                } else {
                    unreachable!()
                };

            let io_result = self.io(&func, &arg, source_info.as_ref()).await;

            let io_result = match io_result {
                Ok(v) => v,
                Err(TypeError::Perform(v)) => {
                    let mut continuations = vec![];
                    let perform_handler = loop {
                        match self.cont_stack.pop_and_auto_defork() {
                            Some(ContinuationOrHandler::Continuation(cont)) => {
                                // 保护从栈弹出的 continuation（离开栈保护区域）
                                self.roots.attach(&cont);
                                continuations.push(cont)
                            }
                            Some(ContinuationOrHandler::PerformHandler(handler)) => {
                                // 保护从栈弹出的 handler（离开栈保护区域）
                                self.roots.attach(&handler);
                                break handler;
                            }
                            None => {
                                return Err(TypeError::MissingPerformHandler(Box::new(func)));
                            }
                        }
                    };

                    let (mut continuation, handler, k0_is_identity) = match continuation_style {
                        InvokeCountinuationStyle::TailCall => {
                            (Closure::identity(None, empty_env.view())?, None, true)
                        }
                        InvokeCountinuationStyle::WithContinuation(v) => (v, None, false),
                        InvokeCountinuationStyle::WithPerformHandler(h) => {
                            (Closure::identity(None, empty_env.view())?, Some(h), true)
                        }
                        InvokeCountinuationStyle::WithBoth(c, h) => (c, Some(h), false),
                    };

                    if !continuations.is_empty() {
                        // 把“当前 Invoke 自带的 continuation”与“栈上捕获的 continuations”组合成一个 continuation。
                        // 组合后的 continuation 行为等价于：v -> k1(k2(...(kn(k0(v)))...))
                        // 其中 k0 是当前 Invoke 的 continuation（可能为 identity），kn..k1 来自 cont_stack。

                        let mut chain = Vec::with_capacity(
                            continuations.len() + if k0_is_identity { 0 } else { 1 },
                        );
                        if !k0_is_identity {
                            chain.push(continuation);
                        }
                        chain.extend(continuations.into_iter());

                        // 用 Invoke 链构造一个闭包：参数名可以复用（每层 Closure::lazy 都会形成新的作用域）。
                        let bind_name: Arc<str> = Arc::from("var#continuation_value");

                        // 从外到内折叠：最后一次调用使用 TailCall（continuation=None）以保持 TCO。
                        let mut next_cont: Option<Type<T>> = None;
                        for func in chain.into_iter().rev() {
                            let arg = Variable::new_argument(bind_name.as_ref(), None);
                            let invoke =
                                Invoke::new(func, arg, next_cont.clone(), None::<Type<T>>, None);
                            let new_closure =
                                Closure::lazy(None, bind_name.clone(), invoke, empty_env.view())?;
                            next_cont = Some(new_closure);
                        }

                        continuation = next_cont.expect("Continuation chain must be non-empty");
                    }

                    let inner_closure = Closure::lazy(
                        None,
                        Arc::from("var#continuation"),
                        Invoke::new(
                            Variable::new_argument("var#continuation", None),
                            *v,
                            None::<Type<T>>,
                            handler,
                            None,
                        ),
                        empty_env.view(),
                    )?;

                    let perform_invoke = Invoke::new(
                        perform_handler.clone(),
                        continuation,
                        Some(inner_closure),
                        None::<Type<T>>,
                        source_info.clone(),
                    );
                    self.current_type = Some(perform_invoke);
                    return Ok(true);
                }
                Err(e) => return Err(e),
            };

            let invoke_result = match io_result {
                Some(io_result) => io_result,
                None => {
                    // 在独立作用域中处理 invoke,确保 rec_assumptions 被 drop
                    let mut rec_assumptions = smallvec::SmallVec::new();
                    let invoke_context = InvokeContext::new(
                        arg,
                        empty_env.view(),
                        &mut rec_assumptions,
                        gc,
                        &mut self.roots,
                        source_info.as_ref(),
                    );
                    func.invoke(invoke_context)?
                }
            };

            match continuation_style {
                InvokeCountinuationStyle::TailCall => (),
                InvokeCountinuationStyle::WithContinuation(v) => {
                    self.cont_stack.push(ContinuationOrHandler::Continuation(v))
                }
                InvokeCountinuationStyle::WithPerformHandler(v) => {
                    self.cont_stack.push(ContinuationOrHandler::PerformHandler(v));
                }
                InvokeCountinuationStyle::WithBoth(a, b) => {
                    self.cont_stack.push(ContinuationOrHandler::Continuation(a));
                    self.cont_stack.push(ContinuationOrHandler::PerformHandler(b));
                }
            };
            (invoke_result, true)
        } else {
            // 处理非 Invoke 类型
            let continuation = loop {
                match self.cont_stack.pop_and_auto_defork() {
                    Some(ContinuationOrHandler::Continuation(v)) => break Some(v),
                    Some(ContinuationOrHandler::PerformHandler(_)) => continue,
                    None => break None,
                }
            };

            match continuation {
                Some(cont) => {
                    let source_info = cont.source_info().cloned();
                    (
                        Invoke::new(
                            cont,
                            current_type,
                            None::<Type<T>>,
                            None::<Type<T>>,
                            source_info,
                        ),
                        true,
                    )
                }
                None => (current_type, false),
            }
        };

        self.current_type = Some(next_type);
        // println!(
        //     "-> Current type: {}",
        //     self.current_type
        //         .as_ref()
        //         .unwrap()
        //         .represent(&mut FastCycleDetector::new())
        // );
        // println!("Frames: {:?}", self.cont_stack.frames());

        // for ty in self.cont_stack.real_stack() {
        //     match ty {
        //         ContinuationOrHandler::Continuation(v) => {
        //             println!(
        //                 "  Continuation in stack: {}",
        //                 v.display(&mut FastCycleDetector::new())
        //             );
        //         }
        //         ContinuationOrHandler::PerformHandler(v) => {
        //             println!(
        //                 "  Perform Handler in stack: {}",
        //                 v.display(&mut FastCycleDetector::new())
        //             );
        //         }
        //     }
        // }
        // println!("\n");
        // println!("Stack length: {}", self.cont_stack.len());
        Ok(updated)
    }

    pub fn sweep_roots(&mut self) {
        self.roots.sweep();
        for ty in self.cont_stack.real_stack() {
            self.roots.attach(ty.as_type());
        }
        if let Some(current) = &self.current_type {
            self.roots.attach(current);
        }
    }

    pub fn stack(&self) -> &Stack<ContinuationOrHandler<T>> {
        &self.cont_stack
    }

    pub fn current(&self) -> &Type<T> {
        self.current_type.as_ref().expect("Current type is None")
    }

    pub fn roots(&self) -> &RootStack<Type<T>, T> {
        &self.roots
    }

    pub fn io_handler(&self) -> &Option<AsyncIoHandler<T>> {
        &self.outer_io_handler
    }

    pub fn io_handler_mut(&mut self) -> &mut Option<AsyncIoHandler<T>> {
        &mut self.outer_io_handler
    }

    pub fn set_state(&mut self, ty: Type<T>, stack: Stack<ContinuationOrHandler<T>>) {
        self.current_type = Some(ty);
        self.cont_stack = stack;
    }
}

pub fn find_last_perform_handler<'a, T: GcAllocObject<T, Inner = Type<T>>>(
    cont_stack: &'a StackView<'a, ContinuationOrHandler<T>>,
) -> Option<(&'a Type<T>, usize)> {
    for (index, cont) in cont_stack.iter().rev().enumerate() {
        match cont {
            ContinuationOrHandler::PerformHandler(v) => {
                return Some((v, cont_stack.len() - 1 - index));
            }
            _ => continue,
        }
    }
    None
}

pub fn find_last_continuation<'a, T: GcAllocObject<T, Inner = Type<T>>>(
    cont_stack: &'a StackView<'a, ContinuationOrHandler<T>>,
) -> Option<(&'a Type<T>, usize)> {
    for (index, cont) in cont_stack.iter().rev().enumerate() {
        match cont {
            ContinuationOrHandler::Continuation(v) => {
                return Some((v, cont_stack.len() - 1 - index));
            }
            _ => continue,
        }
    }
    None
}
