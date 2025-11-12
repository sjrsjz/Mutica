pub mod stack;

use std::{future::Future, io::Write, pin::Pin, sync::Arc};

use arc_gc::gc::GC;

use crate::{
    scheduler::stack::{Stack, StackView},
    types::{
        AsDispatcher, CoinductiveType, GcAllocObject, InvokeContext, ReductionContext,
        Representable, Type, TypeError, TypeRef,
        character_value::CharacterValue,
        closure::{ClosureEnv, ParamEnv},
        integer_value::IntegerValue,
        invoke::{Invoke, InvokeCountinuationStyle},
        tuple::Tuple,
    },
    util::{
        allocator::{Id, IdAllocator},
        collector::Collector,
        cycle_detector::FastCycleDetector,
        rootstack::RootStack,
        source_info::SourceLocation,
    },
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
    allocated_types: IdAllocator<Type<T>>,
    roots: RootStack<Type<T>, T>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> LinearScheduler<T> {
    pub fn new(initial_type: Type<T>, outer_io_handler: Option<AsyncIoHandler<T>>) -> Self {
        let mut roots = RootStack::new();
        roots.attach(&initial_type);
        Self {
            outer_io_handler,
            cont_stack: Stack::new(),
            current_type: Some(initial_type),
            allocated_types: IdAllocator::new(),
            roots,
        }
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
            let TypeRef::Opcode(op) = f else {
                unreachable!()
            };
            if !matches!(&op.kind, crate::types::opcode::OpcodeKind::IO(_)) {
                return Ok(None);
            }
            let crate::types::opcode::OpcodeKind::IO(io_name) = &op.kind else {
                unreachable!()
            };
            match io_name.as_ref().as_str() {
                // 基本IO操作
                "print" => {
                    let str = arg.display(&mut FastCycleDetector::new(), 0, usize::MAX);
                    print!("{}", str);
                    Ok(Some(Tuple::new(
                        Vec::<Type<T>>::new(),
                        source_info.cloned(),
                    )))
                }
                "println" => {
                    let str = arg.display(&mut FastCycleDetector::new(), 0, usize::MAX);
                    println!("{}", str);
                    Ok(Some(Tuple::new(
                        Vec::<Type<T>>::new(),
                        source_info.cloned(),
                    )))
                }
                "input" => {
                    let mut input = String::new();
                    std::io::stdin().read_line(&mut input).unwrap();
                    let chars = input
                        .chars()
                        .map(|c| CharacterValue::new(c, source_info.cloned()))
                        .collect::<Vec<_>>();
                    Ok(Some(Tuple::new(chars, source_info.cloned())))
                }
                "flush" => {
                    use std::io;
                    io::stdout().flush().unwrap();
                    Ok(Some(Tuple::new(
                        Vec::<Type<T>>::new(),
                        source_info.cloned(),
                    )))
                }
                // 类型表示相关
                "repr" => {
                    let repr = arg.represent(&mut FastCycleDetector::new(), 0, usize::MAX);
                    let chars = repr
                        .chars()
                        .map(|c| CharacterValue::new(c, source_info.cloned()))
                        .collect::<Vec<_>>();
                    Ok(Some(Tuple::new(chars, source_info.cloned())))
                }
                "display" => {
                    let disp = arg.display(&mut FastCycleDetector::new(), 0, usize::MAX);
                    let chars = disp
                        .chars()
                        .map(|c| CharacterValue::new(c, source_info.cloned()))
                        .collect::<Vec<_>>();
                    Ok(Some(Tuple::new(chars, source_info.cloned())))
                }
                // 代数效应相关
                "perform" => Err(TypeError::Perform(arg.clone().into())),
                "break" => Err(TypeError::Break(arg.clone().into())),
                "resume" => Err(TypeError::Resume(arg.clone().into())),
                // 类型结构描述相关
                "tuple_len" => arg
                    .map(&mut FastCycleDetector::new(), |_, arg| match arg {
                        TypeRef::Tuple(v) => Ok(Some(IntegerValue::new(
                            v.len() as i64,
                            source_info.cloned(),
                        ))),
                        _ => Err(TypeError::TypeMismatch(
                            (arg.clone_data(), "Tuple | List".into()).into(),
                        )),
                    })?
                    .unwrap_or(Err(TypeError::UnresolvableType(
                        "Could not resolve tuple_len argument".into(),
                    ))),
                "as_tuple" => arg
                    .map(&mut FastCycleDetector::new(), |_, arg| match arg {
                        TypeRef::Tuple(_) => Ok(Some(arg.clone_data())),
                        TypeRef::Any(v) => {
                            let mut elements = Vec::new();
                            for ty in v.types() {
                                elements.push(ty.clone());
                            }
                            Ok(Some(Tuple::new(elements, source_info.cloned())))
                        }
                        TypeRef::All(v) => {
                            let mut elements = Vec::new();
                            for ty in v.types() {
                                elements.push(ty.clone());
                            }
                            Ok(Some(Tuple::new(elements, source_info.cloned())))
                        }
                        _ => Err(TypeError::TypeMismatch(
                            (
                                arg.clone_data(),
                                "Tuple | List | Generalize | Specialize".into(),
                            )
                                .into(),
                        )),
                    })?
                    .unwrap_or(Err(TypeError::UnresolvableType(
                        "Could not resolve as_tuple argument".into(),
                    ))),
                // 可变状态相关
                "alloc" => {
                    let id = self.allocated_types.alloc(arg.clone());
                    Ok(Some(Tuple::new(
                        vec![
                            IntegerValue::new(id.index() as i64, source_info.cloned()),
                            IntegerValue::new(id.generation() as i64, source_info.cloned()),
                        ],
                        source_info.cloned(),
                    )))
                }
                "dealloc" => arg
                    .map(&mut FastCycleDetector::new(), |_, arg| {
                        if let TypeRef::Tuple(tuple) = arg {
                            if tuple.len() != 2 {
                                return Err(TypeError::TypeMismatch(
                                    (arg.clone_data(), "Tuple of length 2".into()).into(),
                                ));
                            }
                            let index_ty = tuple.get(0).unwrap();
                            let generation_ty = tuple.get(1).unwrap();
                            if let (
                                TypeRef::IntegerValue(index_iv),
                                TypeRef::IntegerValue(gen_iv),
                            ) = (
                                index_ty.as_ref_dispatcher(),
                                generation_ty.as_ref_dispatcher(),
                            ) {
                                let index = index_iv.value() as usize;
                                let generation = gen_iv.value() as u32;
                                self.allocated_types
                                    .dealloc(Id::from_parts(index, generation));
                                Ok(Some(Tuple::new(
                                    Vec::<Type<T>>::new(),
                                    source_info.cloned(),
                                )))
                            } else {
                                Err(TypeError::TypeMismatch(
                                    (arg.clone_data(), "Tuple of two IntegerValues".into()).into(),
                                ))
                            }
                        } else {
                            Err(TypeError::TypeMismatch(
                                (arg.clone_data(), "Tuple".into()).into(),
                            ))
                        }
                    })?
                    .unwrap_or(Err(TypeError::UnresolvableType(
                        "Could not resolve dealloc argument".into(),
                    ))),
                "get" => arg
                    .map(&mut FastCycleDetector::new(), |_, arg| {
                        if let TypeRef::Tuple(tuple) = arg {
                            if tuple.len() != 2 {
                                return Err(TypeError::TypeMismatch(
                                    (arg.clone_data(), "Tuple of length 2".into()).into(),
                                ));
                            }
                            let index_ty = tuple.get(0).unwrap();
                            let generation_ty = tuple.get(1).unwrap();
                            if let (Type::IntegerValue(index_iv), Type::IntegerValue(gen_iv)) =
                                (index_ty, generation_ty)
                            {
                                let index = index_iv.value() as usize;
                                let generation = gen_iv.value() as u32;
                                let id = Id::from_parts(index, generation);
                                match self.allocated_types.get(id) {
                                    Some(v) => Ok(Some(v.clone())),
                                    None => {
                                        Err(TypeError::RuntimeError(Arc::new(std::io::Error::new(
                                            std::io::ErrorKind::NotFound,
                                            format!("No value found for allocated id {:?}", id),
                                        ))))
                                    }
                                }
                            } else {
                                Err(TypeError::TypeMismatch(
                                    (arg.clone_data(), "Tuple of two IntegerValues".into()).into(),
                                ))
                            }
                        } else {
                            Err(TypeError::TypeMismatch(
                                (arg.clone_data(), "Tuple".into()).into(),
                            ))
                        }
                    })?
                    .unwrap_or(Err(TypeError::UnresolvableType(
                        "Could not resolve get argument".into(),
                    ))),
                "set" => arg
                    .map(&mut FastCycleDetector::new(), |_, arg| {
                        if let TypeRef::Tuple(tuple) = arg {
                            if tuple.len() != 2 {
                                return Err(TypeError::TypeMismatch(
                                    (arg.clone_data(), "Tuple of length 2".into()).into(),
                                ));
                            }
                            let id_ty = tuple.get(0).unwrap();
                            let value_ty = tuple.get(1).unwrap();
                            id_ty
                                .map(&mut FastCycleDetector::new(), |_, id_ty| {
                                    if let TypeRef::Tuple(id_tup) = id_ty {
                                        if id_tup.len() != 2 {
                                            return Err(TypeError::TypeMismatch(
                                                (id_ty.clone_data(), "Tuple of length 2".into())
                                                    .into(),
                                            ));
                                        }
                                        let index_ty = id_tup.get(0).unwrap();
                                        let generation_ty = id_tup.get(1).unwrap();
                                        if let (
                                            Type::IntegerValue(index_iv),
                                            Type::IntegerValue(gen_iv),
                                        ) = (index_ty, generation_ty)
                                        {
                                            let index = index_iv.value() as usize;
                                            let generation = gen_iv.value() as u32;
                                            let id = Id::from_parts(index, generation);
                                            match self.allocated_types.get_mut(id) {
                                                Some(v) => {
                                                    *v = value_ty.clone();
                                                    Ok(Some(Tuple::new(
                                                        Vec::<Type<T>>::new(),
                                                        source_info.cloned(),
                                                    )))
                                                }
                                                None => Err(TypeError::RuntimeError(Arc::new(
                                                    std::io::Error::new(
                                                        std::io::ErrorKind::NotFound,
                                                        format!(
                                                            "No value found for allocated id {:?}",
                                                            id
                                                        ),
                                                    ),
                                                ))),
                                            }
                                        } else {
                                            Err(TypeError::TypeMismatch(
                                                (
                                                    id_ty.clone_data(),
                                                    "Tuple of two IntegerValues".into(),
                                                )
                                                    .into(),
                                            ))
                                        }
                                    } else {
                                        Err(TypeError::TypeMismatch(
                                            (id_ty.clone_data(), "Tuple".into()).into(),
                                        ))
                                    }
                                })?
                                .unwrap_or(Err(TypeError::UnresolvableType(
                                    "Could not resolve id in set argument".into(),
                                )))
                        } else {
                            Err(TypeError::TypeMismatch(
                                (arg.clone_data(), "Tuple".into()).into(),
                            ))
                        }
                    })?
                    .unwrap_or(Err(TypeError::UnresolvableType(
                        "Could not resolve set argument".into(),
                    ))),

                _ => Ok(None),
            }
        })?
        .unwrap_or(Err(TypeError::UnresolvableType(
            "Could not resolve set argument".into(),
        )))
    }

    pub async fn step(&mut self, gc: &mut GC<T>) -> Result<bool, TypeError<Type<T>, T>> {
        let empty_v = ClosureEnv::new(Vec::<Type<T>>::new());
        let empty_p = ParamEnv::from_collector(&mut Collector::new(), 0)
            .unwrap()
            .unwrap();

        // 在 await 之前完成所有需要 rec_assumptions 的工作
        let reduced = {
            let mut rec_assumptions = smallvec::SmallVec::new();
            let mut reduction_ctx = ReductionContext::new(
                &empty_v,
                &empty_p,
                &mut rec_assumptions,
                gc,
                &mut self.roots,
            );
            let current_type = self.current_type.take().ok_or_else(|| {
                TypeError::RuntimeError(Arc::new(std::io::Error::other("No current type to step")))
            })?;
            current_type.reduce(&mut reduction_ctx)?
        };

        // 检查是否是 Invoke 类型
        let is_invoke = matches!(reduced.as_ref_dispatcher(), TypeRef::Invoke(_));

        let (next_type, updated) = if is_invoke {
            // 处理 Invoke 类型
            let (func, arg, continuation_style, source_info) = if let Type::Invoke(invoke) = reduced
            {
                invoke.take()
            } else {
                unreachable!()
            };

            let io_result = self.io(&func, &arg, source_info.as_ref()).await;

            let io_result = match io_result {
                Ok(v) => v,
                Err(TypeError::Perform(v)) => {
                    let view = self.cont_stack.view();
                    let (perform_handler, index) = match find_last_perform_handler(&view) {
                        Some(handler) => handler,
                        None => {
                            return Err(TypeError::MissingPerformHandler(Box::new(func)));
                        }
                    };
                    let perform_invoke = Invoke::new(
                        perform_handler.clone(),
                        *v,
                        None::<Type<T>>,
                        None::<Type<T>>,
                        source_info.clone(),
                    );
                    match continuation_style {
                        InvokeCountinuationStyle::TailCall => (),
                        InvokeCountinuationStyle::WithContinuation(v) => {
                            self.cont_stack.push(ContinuationOrHandler::Continuation(v))
                        }
                        InvokeCountinuationStyle::WithPerformHandler(v) => {
                            self.cont_stack
                                .push(ContinuationOrHandler::PerformHandler(v));
                        }
                        InvokeCountinuationStyle::WithBoth(a, b) => {
                            self.cont_stack.push(ContinuationOrHandler::Continuation(a));
                            self.cont_stack
                                .push(ContinuationOrHandler::PerformHandler(b));
                        }
                    }
                    self.cont_stack.fork(index); // 踢掉perform handler及其上面的frame
                    self.current_type = Some(perform_invoke);
                    return Ok(true);
                }
                Err(TypeError::Break(v)) => {
                    // 找到最近的Perform Handler并删除它以及其上面的所有continuation
                    loop {
                        match self.cont_stack.pop_and_auto_defork() {
                            Some(ContinuationOrHandler::Continuation(_)) => continue,
                            Some(ContinuationOrHandler::PerformHandler(_)) => break,
                            None => {
                                return Err(TypeError::MissingPerformHandler(Box::new(func)));
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
                    match continuation_style {
                        InvokeCountinuationStyle::TailCall => (),
                        InvokeCountinuationStyle::WithContinuation(v) => {
                            self.cont_stack.push(ContinuationOrHandler::Continuation(v))
                        }
                        InvokeCountinuationStyle::WithPerformHandler(v) => {
                            self.cont_stack
                                .push(ContinuationOrHandler::PerformHandler(v));
                        }
                        InvokeCountinuationStyle::WithBoth(a, b) => {
                            self.cont_stack.push(ContinuationOrHandler::Continuation(a));
                            self.cont_stack
                                .push(ContinuationOrHandler::PerformHandler(b));
                        }
                    }
                    let break_invoke = match continuation {
                        Some(continuation) => Invoke::new(
                            continuation,
                            *v,
                            None::<Type<T>>,
                            None::<Type<T>>,
                            source_info.clone(),
                        ),
                        None => *v,
                    };
                    self.current_type = Some(break_invoke);
                    return Ok(true);
                }
                Err(TypeError::Resume(v)) => {
                    match continuation_style {
                        InvokeCountinuationStyle::TailCall => (),
                        InvokeCountinuationStyle::WithContinuation(v) => {
                            self.cont_stack.push(ContinuationOrHandler::Continuation(v))
                        }
                        InvokeCountinuationStyle::WithPerformHandler(v) => {
                            self.cont_stack
                                .push(ContinuationOrHandler::PerformHandler(v));
                        }
                        InvokeCountinuationStyle::WithBoth(a, b) => {
                            self.cont_stack.push(ContinuationOrHandler::Continuation(a));
                            self.cont_stack
                                .push(ContinuationOrHandler::PerformHandler(b));
                        }
                    }
                    let view = match self.cont_stack.skip_frames(1) {
                        Some(view) => view,
                        None => {
                            return Err(TypeError::RuntimeError(Arc::new(std::io::Error::other(
                                "No continuation to resume",
                            ))));
                        }
                    };
                    let cont = find_last_continuation(&view).map(|(v, _)| v.clone());
                    self.cont_stack.fork_frame(view.len(), view.frame_index());
                    if let Some(v) = cont {
                        self.cont_stack.push(ContinuationOrHandler::Continuation(v));
                    }

                    self.current_type = Some(*v);
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
                        arg.clone(),
                        &empty_v,
                        &empty_p,
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
                    self.cont_stack
                        .push(ContinuationOrHandler::PerformHandler(v));
                }
                InvokeCountinuationStyle::WithBoth(a, b) => {
                    self.cont_stack.push(ContinuationOrHandler::Continuation(a));
                    self.cont_stack
                        .push(ContinuationOrHandler::PerformHandler(b));
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
                        Invoke::new(cont, reduced, None::<Type<T>>, None::<Type<T>>, source_info),
                        true,
                    )
                }
                None => (reduced, false),
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
        for ty in self.allocated_types.iter().flatten() {
            self.roots.attach(ty);
        }
    }

    pub fn stack(&self) -> &Stack<ContinuationOrHandler<T>> {
        &self.cont_stack
    }

    pub fn current(&self) -> &Type<T> {
        self.current_type.as_ref().expect("Current type is None")
    }

    pub fn allocated_types(&self) -> &IdAllocator<Type<T>> {
        &self.allocated_types
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
