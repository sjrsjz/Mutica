use std::{num::NonZero, sync::Arc};

use arena_arc::Allocator;

use crate::types::{
    CoinductiveType, GcAllocObject, closure::ClosureBranch, constraint::Constraint,
    invoke::InvokeCountinuationStyle,
};

pub struct Allocators<U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    pub v: Allocator<U, usize, 1024>,
    pub kv: Allocator<(Arc<str>, U), usize, 256>,
    pub match_branch: Allocator<ClosureBranch<U, V>, usize, 64>,
    pub invoke: Allocator<(U, U, InvokeCountinuationStyle<U, V>), usize, 256>,
    pub constraint: Allocator<Constraint<U, V>, usize, 64>,
    pub rle: Allocator<(U, NonZero<usize>), usize, 256>,
    #[doc(hidden)]
    _phandom: std::marker::PhantomData<V>,
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Allocators<U, V> {
    pub fn new() -> Self {
        Self {
            v: Allocator::new(),
            kv: Allocator::new(),
            match_branch: Allocator::new(),
            invoke: Allocator::new(),
            constraint: Allocator::new(),
            rle: Allocator::new(),
            _phandom: std::marker::PhantomData,
        }
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Default for Allocators<U, V> {
    fn default() -> Self {
        Self::new()
    }
}
