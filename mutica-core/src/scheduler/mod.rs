use std::{io::Write, sync::Arc};

use arc_gc::gc::GC;

use crate::{
    types::{
        CoinductiveType, InvokeContext, ReductionContext, Representable, Type, TypeError,
        character_value::CharacterValue,
        closure::{ClosureEnv, ParamEnv},
        fixpoint::FixPointInner,
        list::List,
        opcode::Opcode,
        tuple::Tuple,
    },
    util::{collector::Collector, cycle_detector::FastCycleDetector, rootstack::RootStack},
};

pub struct LinearScheduler {
    cont_stack: Vec<Type>, // Continuation stack
    current_type: Option<Type>,
    roots: RootStack,
}

impl LinearScheduler {
    pub fn new(initial_type: Type) -> Self {
        let mut roots = RootStack::new();
        roots.attach(&initial_type);
        Self {
            cont_stack: vec![],
            current_type: Some(initial_type),
            roots,
        }
    }

    fn io(f: &Type, arg: &Type) -> Result<Option<Type>, TypeError> {
        f.map(&mut FastCycleDetector::new(), |_, f| {
            if !matches!(f, Type::Opcode(_)) {
                return Ok(None);
            }
            let Type::Opcode(op) = f else { unreachable!() };
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
                    Ok(Some(Tuple::new(Vec::<Type>::new())))
                }
                "println" => {
                    let str = arg.display(&mut FastCycleDetector::new());
                    println!("{}", str);
                    Ok(Some(Tuple::new(Vec::<Type>::new())))
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
                    Ok(Some(Tuple::new(Vec::<Type>::new())))
                }
                _ => Ok(None),
            }
        })?
    }

    pub fn step(&mut self, gc: &mut GC<FixPointInner>) -> Result<bool, TypeError> {
        let empty_v = ClosureEnv::new(Vec::<Type>::new());
        let empty_p = ParamEnv::from_collector(Collector::new()).unwrap().unwrap();
        let mut rec_assumptions = smallvec::SmallVec::new();
        let mut reduction_ctx = ReductionContext::new(
            &empty_v,
            &empty_p,
            None,
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
            reduced.map(&mut FastCycleDetector::new(), |_, reduced| match reduced {
                Type::Invoke(invoke) => {
                    let mut rec_assumptions2 = smallvec::SmallVec::new();
                    let mut invoke_ctx = InvokeContext::new(
                        invoke.arg(),
                        &empty_v,
                        &empty_p,
                        invoke.continuation(),
                        &mut rec_assumptions2,
                        gc,
                        &mut self.roots,
                    );

                    let result = match Self::io(invoke.func(), invoke.arg())? {
                        Some(io_result) => io_result,
                        None => invoke.func().invoke(&mut invoke_ctx)?,
                    };
                    let result = invoke.flat_compose_stack(
                        result,
                        gc,
                        &mut self.roots,
                        &mut self.cont_stack,
                    )?;
                    Ok((result, true))
                }
                _ => {
                    if self.cont_stack.is_empty() {
                        // No more continuation, we are done
                        return Ok((reduced.clone(), false));
                    }
                    // Now take a continuation from the stack and invoke it
                    let k = self.cont_stack.pop().unwrap();
                    let mut rec_assumptions2 = smallvec::SmallVec::new();
                    let mut invoke_ctx = InvokeContext::new(
                        reduced,
                        &empty_v,
                        &empty_p,
                        None,
                        &mut rec_assumptions2,
                        gc,
                        &mut self.roots,
                    );
                    let result = match Self::io(&k, &reduced)? {
                        Some(io_result) => io_result,
                        None => k.invoke(&mut invoke_ctx)?,
                    };
                    Ok((result, true))
                }
            })??;
        self.current_type = Some(next_type);
        // println!(
        //     "-> Current type: {}\n",
        //     self.current_type.represent(&mut FastCycleDetector::new())
        // );
        Ok(updated)
    }

    pub fn sweep_roots(&mut self) {
        self.roots.sweep();
        for ty in &self.cont_stack {
            self.roots.attach(ty);
        }
        if let Some(current) = &self.current_type {
            self.roots.attach(current);
        }
    }

    pub fn stack(&self) -> &[Type] {
        &self.cont_stack
    }

    pub fn current(&self) -> &Type {
        self.current_type.as_ref().expect("Current type is None")
    }
}
