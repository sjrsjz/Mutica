use std::{io::Write, sync::Arc};

use arc_gc::gc::GC;

use crate::{
    types::{
        CoinductiveType, InvokeContext, ReductionContext, Representable, Type, TypeError,
        TypeEnum,
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
    current_type: Option<Type>,
    roots: RootStack,
}

impl LinearScheduler {
    pub fn new(initial_type: Type) -> Self {
        let mut roots = RootStack::new();
        roots.sweep(&initial_type);
        Self {
            current_type: Some(initial_type),
            roots,
        }
    }

    fn io(f: &Type, arg: &Type) -> Result<Option<Type>, TypeError> {
        f.map(&mut FastCycleDetector::new(), |_, f| {
            if !matches!(f.ty(), TypeEnum::Opcode(_)) {
                return Ok(None);
            }
            let TypeEnum::Opcode(op) = f.ty() else {
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

        let reduced = match self.current_type.take() {
            Some(t) => t.reduce(&mut reduction_ctx)?,
            None => {
                return Err(TypeError::RuntimeError(Arc::new(std::io::Error::new(
                    std::io::ErrorKind::Other,
                    "No current type",
                ))));
            }
        };
        let (next_type, updated) =
            reduced.map(&mut FastCycleDetector::new(), |_, reduced| {
                match reduced.ty() {
                    TypeEnum::Invoke(invoke) => {
                        let mut rec_assumptions2 = smallvec::SmallVec::new();
                        let mut invoke_ctx = InvokeContext::new(
                            invoke.arg(),
                            &empty_v,
                            &empty_p,
                            Some(invoke.continuation()),
                            &mut rec_assumptions2,
                            gc,
                            &mut self.roots,
                        );

                        let result = match Self::io(invoke.func(), invoke.arg())? {
                            Some(io_result) => io_result,
                            None => invoke.func().invoke(&mut invoke_ctx)?,
                        };
                        let result = invoke.flat_compose(result, gc, &mut self.roots)?;
                        Ok((result, true))
                    }
                    _ => Ok((reduced.clone(), false)),
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
        if let Some(current_type) = &self.current_type {
            self.roots.sweep(current_type);
        }
    }

    pub fn current_type(&self) -> Option<&Type> {
        self.current_type.as_ref()
    }
}
