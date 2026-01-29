use std::sync::Arc;

use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        GcAllocObject, InvokeContext, Representable, Type, TypeError,
        natural_number::NaturalNumber,
        opaque::{OpaqueObject, OpaqueValue},
    },
    util::{rootstack::Rootable, source_info::SourceLocation},
};

pub struct Stopwatch<T: GcAllocObject<T, Inner = Type<T>>> {
    pub start_time: std::time::Instant,
    pub _phantom: std::marker::PhantomData<T>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Stopwatch<T> {
    fn collect(&self, _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {}
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Stopwatch<T> {}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Stopwatch<T> {
    fn represent(
        &self,
        _path: &mut crate::util::cycle_detector::FastCycleDetector<crate::types::TaggedPtr<()>>,
        _depth: usize,
        _max_depth: usize,
    ) -> String {
        format!("Stopwatch<elapsed={:?}>", self.start_time.elapsed())
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Stopwatch<T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(source_info: Option<Arc<SourceLocation>>) -> Type<T> {
        OpaqueObject::new(
            Stopwatch { start_time: std::time::Instant::now(), _phantom: std::marker::PhantomData },
            source_info,
        )
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> OpaqueValue<T> for Stopwatch<T> {
    fn invoke(&self, _ctx: InvokeContext<Type<T>, T>) -> Result<Type<T>, TypeError<Type<T>, T>> {
        // 返回从创建时间到现在的纳秒数
        let elapsed_nanos = self.start_time.elapsed().as_nanos();
        Ok(NaturalNumber::new(elapsed_nanos as usize, None))
    }
}
