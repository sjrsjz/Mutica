pub mod stack;

use std::{io::Write, sync::Arc};

use arc_gc::gc::GC;

use crate::{
    scheduler::stack::{Stack, StackView},
    types::{
        CoinductiveType, GcAllocObject, InvokeContext, ReductionContext, Representable, Type,
        TypeError, TypeRef,
        character_value::CharacterValue,
        closure::{ClosureEnv, ParamEnv},
        invoke::{Invoke, InvokeCountinuationStyle},
        list::List,
        opcode::Opcode,
        tuple::Tuple,
    },
    util::{collector::Collector, cycle_detector::FastCycleDetector, rootstack::RootStack},
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

pub struct LinearScheduler<T: GcAllocObject<T, Inner = Type<T>>> {
    cont_stack: Stack<ContinuationOrHandler<T>>,
    current_type: Option<Type<T>>,
    roots: RootStack<Type<T>, T>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> LinearScheduler<T> {
    pub fn new(initial_type: Type<T>) -> Self {
        let mut roots = RootStack::new();
        roots.attach(&initial_type);
        Self {
            cont_stack: Stack::new(),
            current_type: Some(initial_type),
            roots,
        }
    }

    fn io(f: &Type<T>, arg: &Type<T>) -> Result<Option<Type<T>>, TypeError<Type<T>, T>> {
        f.map(&mut FastCycleDetector::new(), |_, f| {
            if !matches!(f, TypeRef::Opcode(_)) {
                return Ok(None);
            }
            let TypeRef::Opcode(op) = f else {
                unreachable!()
            };
            if !matches!(op, Opcode::IO(_)) {
                return Ok(None);
            }
            let Opcode::IO(io_name) = op else {
                unreachable!()
            };
            match io_name.as_ref().as_str() {
                "print" => {
                    let str = arg.display(&mut FastCycleDetector::new());
                    print!("{}", str);
                    Ok(Some(Tuple::new(Vec::<Type<T>>::new())))
                }
                "println" => {
                    let str = arg.display(&mut FastCycleDetector::new());
                    println!("{}", str);
                    Ok(Some(Tuple::new(Vec::<Type<T>>::new())))
                }
                "input" => {
                    let mut input = String::new();
                    std::io::stdin().read_line(&mut input).unwrap();
                    let chars = input
                        .chars()
                        .map(|c| CharacterValue::new(c))
                        .collect::<Vec<_>>();
                    Ok(Some(List::new(chars)))
                }
                "flush" => {
                    use std::io;
                    io::stdout().flush().unwrap();
                    Ok(Some(Tuple::new(Vec::<Type<T>>::new())))
                }
                "repr" => {
                    let repr = arg.represent(&mut FastCycleDetector::new());
                    let chars = repr
                        .chars()
                        .map(|c| CharacterValue::new(c))
                        .collect::<Vec<_>>();
                    Ok(Some(List::new(chars)))
                }
                "display" => {
                    let disp = arg.display(&mut FastCycleDetector::new());
                    let chars = disp
                        .chars()
                        .map(|c| CharacterValue::new(c))
                        .collect::<Vec<_>>();
                    Ok(Some(List::new(chars)))
                }
                "perform" => Err(TypeError::Perform(arg.clone().into())),
                "break" => Err(TypeError::Break(arg.clone().into())),
                "resume" => Err(TypeError::Resume(arg.clone().into())),
                _ => Ok(None),
            }
        })?
    }

    pub fn step(&mut self, gc: &mut GC<T>) -> Result<bool, TypeError<Type<T>, T>> {
        let empty_v = ClosureEnv::new(Vec::<Type<T>>::new());
        let empty_p = ParamEnv::from_collector(Collector::new()).unwrap().unwrap();
        let mut rec_assumptions = smallvec::SmallVec::new();
        let mut reduction_ctx = ReductionContext::new(
            &empty_v,
            &empty_p,
            &mut rec_assumptions,
            gc,
            &mut self.roots,
        );
        let current_type = self.current_type.take().ok_or_else(|| {
            TypeError::RuntimeError(Arc::new(std::io::Error::new(
                std::io::ErrorKind::Other,
                "No current type to step",
            )))
        })?;
        let reduced = current_type.reduce(&mut reduction_ctx)?;
        let (next_type, updated) =
            reduced.map(&mut FastCycleDetector::new(), |_, inner| match inner {
                TypeRef::Invoke(invoke) => {
                    let invoke_context = &mut InvokeContext::new(
                        invoke.arg(),
                        &empty_v,
                        &empty_p,
                        &mut rec_assumptions,
                        gc,
                        &mut self.roots,
                    );
                    let io_result = Self::io(invoke.func(), invoke.arg());
                    let io_result = match io_result {
                        Ok(v) => v,
                        Err(TypeError::Perform(v)) => {
                            let (perform_handler, index) =
                                match find_last_perform_handler(self.cont_stack.view()) {
                                    Some(handler) => handler,
                                    None => {
                                        return Err(TypeError::MissingPerformHandler(Box::new(
                                            invoke.arg().clone(),
                                        )));
                                    }
                                };
                            let perform_invoke =
                                Invoke::new(perform_handler, *v, None::<Type<T>>, None::<Type<T>>);
                            match invoke.continuation_style() {
                                InvokeCountinuationStyle::TailCall => (),
                                InvokeCountinuationStyle::CPS(v) => self
                                    .cont_stack
                                    .push(ContinuationOrHandler::Continuation(v.clone())),
                                InvokeCountinuationStyle::HPS(v) => {
                                    self.cont_stack
                                        .push(ContinuationOrHandler::PerformHandler(v.clone()));
                                }
                                InvokeCountinuationStyle::CHPS(a, b) => {
                                    self.cont_stack
                                        .push(ContinuationOrHandler::Continuation(a.clone()));
                                    self.cont_stack
                                        .push(ContinuationOrHandler::PerformHandler(b.clone()));
                                }
                            }
                            self.cont_stack.fork(index); // 踢掉perform handler及其上面的frame
                            return Ok((perform_invoke, true));
                        }
                        Err(TypeError::Break(v)) => {
                            // 找到最近的Perform Handler并删除它以及其上面的所有continuation
                            loop {
                                match self.cont_stack.pop_and_auto_defork() {
                                    Some(ContinuationOrHandler::Continuation(_)) => continue,
                                    Some(ContinuationOrHandler::PerformHandler(_)) => break,
                                    None => {
                                        return Err(TypeError::MissingPerformHandler(Box::new(
                                            invoke.arg().clone(),
                                        )));
                                    }
                                }
                            }
                            // 然后找到最近的Continuation
                            let continuation = loop {
                                match self.cont_stack.pop_and_auto_defork() {
                                    Some(ContinuationOrHandler::Continuation(v)) => break Some(v),
                                    Some(ContinuationOrHandler::PerformHandler(_)) => continue,
                                    None => break None,
                                }
                            };
                            match invoke.continuation_style() {
                                InvokeCountinuationStyle::TailCall => (),
                                InvokeCountinuationStyle::CPS(v) => self
                                    .cont_stack
                                    .push(ContinuationOrHandler::Continuation(v.clone())),
                                InvokeCountinuationStyle::HPS(v) => {
                                    self.cont_stack
                                        .push(ContinuationOrHandler::PerformHandler(v.clone()));
                                }
                                InvokeCountinuationStyle::CHPS(a, b) => {
                                    self.cont_stack
                                        .push(ContinuationOrHandler::Continuation(a.clone()));
                                    self.cont_stack
                                        .push(ContinuationOrHandler::PerformHandler(b.clone()));
                                }
                            }
                            let break_invoke = match continuation {
                                Some(continuation) => {
                                    Invoke::new(continuation, *v, None::<Type<T>>, None::<Type<T>>)
                                }
                                None => *v,
                            };
                            return Ok((break_invoke, true));
                        }
                        Err(TypeError::Resume(v)) => {
                            let continuation = match self.cont_stack.skip_frames(1) {
                                Some(stack_view) => {
                                    find_last_continuation(stack_view).map(|(v, _)| v.clone())
                                }
                                None => {
                                    return Err(TypeError::MissingContinuation(Box::new(
                                        invoke.arg().clone(),
                                    )));
                                }
                            };

                            match invoke.continuation_style() {
                                InvokeCountinuationStyle::TailCall => (),
                                InvokeCountinuationStyle::CPS(v) => self
                                    .cont_stack
                                    .push(ContinuationOrHandler::Continuation(v.clone())),
                                InvokeCountinuationStyle::HPS(v) => {
                                    self.cont_stack
                                        .push(ContinuationOrHandler::PerformHandler(v.clone()));
                                }
                                InvokeCountinuationStyle::CHPS(a, b) => {
                                    self.cont_stack
                                        .push(ContinuationOrHandler::Continuation(a.clone()));
                                    self.cont_stack
                                        .push(ContinuationOrHandler::PerformHandler(b.clone()));
                                }
                            }
                            let resume_invoke = match continuation {
                                Some(continuation) => {
                                    Invoke::new(continuation, *v, None::<Type<T>>, None::<Type<T>>)
                                }
                                None => *v,
                            };
                            return Ok((resume_invoke, true));
                        }
                        Err(e) => return Err(e),
                    };
                    let invoke_result = match io_result {
                        Some(io_result) => io_result,
                        None => invoke.func().invoke(invoke_context)?,
                    };

                    match invoke.continuation_style() {
                        InvokeCountinuationStyle::TailCall => (),
                        InvokeCountinuationStyle::CPS(v) => self
                            .cont_stack
                            .push(ContinuationOrHandler::Continuation(v.clone())),
                        InvokeCountinuationStyle::HPS(v) => {
                            self.cont_stack
                                .push(ContinuationOrHandler::PerformHandler(v.clone()));
                        }
                        InvokeCountinuationStyle::CHPS(a, b) => {
                            self.cont_stack
                                .push(ContinuationOrHandler::Continuation(a.clone()));
                            self.cont_stack
                                .push(ContinuationOrHandler::PerformHandler(b.clone()));
                        }
                    };
                    Ok((invoke_result, true))
                }
                _ => Ok(loop {
                    match self.cont_stack.pop_and_auto_defork() {
                        Some(ContinuationOrHandler::Continuation(v)) => break Some(v),
                        Some(ContinuationOrHandler::PerformHandler(_)) => continue,
                        None => break None,
                    }
                }
                .map(|cont| {
                    (
                        Invoke::new(cont, inner.clone_data(), None::<Type<T>>, None::<Type<T>>),
                        true,
                    )
                })
                .unwrap_or((reduced.clone(), false))),
            })??;
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
}

pub fn find_last_perform_handler<'a, T: GcAllocObject<T, Inner = Type<T>>>(
    cont_stack: StackView<'a, ContinuationOrHandler<T>>,
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
    cont_stack: StackView<'a, ContinuationOrHandler<T>>,
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
