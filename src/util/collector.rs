pub struct Collector<T> {
    pub items: smallvec::SmallVec<[T; 8]>,
}

impl<T> Collector<T> {
    pub fn new() -> Self {
        Self {
            items: smallvec::SmallVec::new(),
        }
    }

    pub fn collect<F, R, E>(&mut self, f: F) -> Result<Option<R>, E>
    where
        F: FnOnce(&mut Self) -> Result<Option<R>, E>,
    {
        let len = self.items.len();
        let result = f(self);
        match result {
            Ok(Some(res)) => Ok(Some(res)),
            Err(e) => Err(e),
            Ok(None) => {
                self.items.truncate(len);
                Ok(None)
            }
        }
    }

    pub fn push(&mut self, item: T) {
        self.items.push(item);
    }

    pub fn into_vec(self) -> Vec<T> {
        self.items.into_iter().collect()
    }

    pub fn items(&self) -> &smallvec::SmallVec<[T; 8]> {
        &self.items
    }
}
