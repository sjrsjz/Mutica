use std::io::Write;

use arc_gc::gc::GC;

use crate::{
    types::{
        CoinductiveType, InvokeContext, ReductionContext, Representable, Stabilized,
        StabilizedType, Type, TypeError,
        character_value::CharacterValue,
        closure::{ClosureEnv, ParamEnv},
        fixpoint::FixPointInner,
        list::List,
        opcode::Opcode,
        tuple::Tuple,
    },
    util::{collector::Collector, cycle_detector::FastCycleDetector},
};

pub struct LinearScheduler {
    current_type: StabilizedType,
}

impl LinearScheduler {
    pub fn new(initial_type: StabilizedType) -> Self {
        Self {
            current_type: initial_type,
        }
    }

    fn io(&self, f: &Type, invoke_ctx: &mut InvokeContext) -> Result<StabilizedType, TypeError> {
        f.map(&mut FastCycleDetector::new(), |_, f| {
            if !matches!(f, Type::Opcode(_)) {
                return f.invoke(invoke_ctx);
            }
            let Type::Opcode(op) = f else { unreachable!() };
            if !matches!(op, Opcode::IO(_)) {
                return f.invoke(invoke_ctx);
            }
            let Opcode::IO(io_name) = op else {
                unreachable!()
            };
            match io_name.as_str() {
                "print" => {
                    let str = invoke_ctx.arg.display(&mut FastCycleDetector::new());
                    print!("{}", str);
                    Ok(Tuple::new(Vec::<Type>::new()))
                }
                "println" => {
                    let str = invoke_ctx.arg.display(&mut FastCycleDetector::new());
                    println!("{}", str);
                    Ok(Tuple::new(Vec::<Type>::new()))
                }
                "input" => {
                    let mut input = String::new();
                    std::io::stdin().read_line(&mut input).unwrap();
                    let chars = input
                        .chars()
                        .map(|c| CharacterValue::new(c))
                        .collect::<Vec<_>>();
                    Ok(List::new(chars))
                }
                "flush" => {
                    use std::io;
                    io::stdout().flush().unwrap();
                    Ok(Tuple::new(Vec::<Type>::new()))
                }
                _ => f.invoke(invoke_ctx),
            }
        })?
    }

    pub fn step(&mut self, gc: &mut GC<FixPointInner>) -> Result<bool, TypeError> {
        let empty_v = ClosureEnv::new(Vec::<Type>::new());
        let empty_p = ParamEnv::from_collector(Collector::new()).unwrap().unwrap();
        let mut rec_assumptions = smallvec::SmallVec::new();
        let mut reduction_ctx =
            ReductionContext::new(&empty_v, &empty_p, None, &mut rec_assumptions, gc);

        let reduced = self.current_type.reduce(&mut reduction_ctx)?;
        let (next_type, updated) =
            reduced.map(&mut FastCycleDetector::new(), |_, reduced| match reduced {
                Type::Invoke(invoke) => {
                    let mut rec_assumptions2 = smallvec::SmallVec::new();
                    let mut invoke_ctx = InvokeContext::new(
                        invoke.arg(),
                        &empty_v,
                        &empty_p,
                        Some(invoke.continuation()),
                        &mut rec_assumptions2,
                        gc,
                    );

                    let result = self.io(invoke.func(), &mut invoke_ctx)?;
                    let result = invoke.flat_compose(result.weak(), gc)?;
                    Ok((result, true))
                }
                _ => Ok((reduced.clone().stabilize(), false)),
            })??;
        self.current_type = next_type;
        // println!(
        //     "-> Current type: {}\n",
        //     self.current_type.represent(&mut FastCycleDetector::new())
        // );
        Ok(updated)
    }

    pub fn current_type(&self) -> &StabilizedType {
        &self.current_type
    }
}
