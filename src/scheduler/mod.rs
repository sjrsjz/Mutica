use arc_gc::gc::GC;

use crate::{
    types::{
        CoinductiveType, InvokeContext, ReductionContext, Stabilized, StabilizedType, Type,
        TypeError,
        closure::{ClosureEnv, ParamEnv},
        fixpoint::FixPointInner,
    },
    util::collector::Collector,
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

    pub fn step(&mut self, gc: &mut GC<FixPointInner>) -> Result<bool, TypeError> {
        let v = ClosureEnv::new(Vec::<Type>::new());
        let p = ParamEnv::from_collector(Collector::new());
        let mut rec_assumptions = smallvec::SmallVec::new();
        let mut reduction_ctx = ReductionContext::new(&v, &p, None, &mut rec_assumptions, gc);

        let reduced = self.current_type.reduce(&mut reduction_ctx)?;
        let (next_type, updated) = match reduced.weak() {
            Type::Invoke(invoke) => {
                let v2 = ClosureEnv::new(Vec::<Type>::new());
                let p2 = ParamEnv::from_collector(Collector::new());
                let mut rec_assumptions2 = smallvec::SmallVec::new();
                let mut invoke_ctx = InvokeContext::new(
                    invoke.arg(),
                    &v2,
                    &p2,
                    Some(invoke.continuation()),
                    &mut rec_assumptions2,
                    gc,
                );

                let result = invoke.func().invoke(&mut invoke_ctx)?;
                let result = invoke.flat_compose(result.weak(), gc)?;
                (result, true)
            }
            _ => (reduced.clone(), false),
        };
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
