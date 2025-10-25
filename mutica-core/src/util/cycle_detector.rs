use smallvec::{SmallVec, smallvec};

pub struct FastCycleDetector<T: PartialEq> {
    visited: SmallVec<[T; 8]>,
}
impl<T: PartialEq> Default for FastCycleDetector<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: PartialEq> FastCycleDetector<T> {
    pub fn with_guard<R, F>(&mut self, item: T, f: F) -> Option<R>
    where
        F: FnOnce(&mut Self) -> R,
    {
        if self.visited.contains(&item) {
            return None;
        }
        self.visited.push(item);
        let result = f(self); // 将自身的重新借用传入闭包
        self.visited.pop();
        Some(result)
    }

    pub fn new() -> Self {
        FastCycleDetector {
            visited: smallvec![],
        }
    }

    pub fn last(&self) -> Option<&T> {
        self.visited.last()
    }
}
