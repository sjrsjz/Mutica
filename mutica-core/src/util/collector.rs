pub struct Collector<T> {
    items: Option<smallvec::SmallVec<[T; 8]>>,
}

impl<T> Collector<T> {
    pub fn new() -> Self {
        Self {
            items: Some(smallvec::SmallVec::new()),
        }
    }

    pub fn new_disabled() -> Self {
        Self { items: None }
    }

    pub fn is_enabled(&self) -> bool {
        self.items.is_some()
    }

    pub fn is_empty(&self) -> bool {
        match &self.items {
            Some(items) => items.is_empty(),
            None => true,
        }
    }

    pub fn collect<F, R, E>(&mut self, f: F) -> Result<Option<R>, E>
    where
        F: FnOnce(&mut Self) -> Result<Option<R>, E>,
    {
        if self.items.is_some() {
            let len = self.items.as_ref().unwrap().len();
            let result = f(self);
            match result {
                Ok(Some(res)) => Ok(Some(res)),
                Err(e) => Err(e),
                Ok(None) => {
                    self.items.as_mut().unwrap().truncate(len);
                    Ok(None)
                }
            }
        } else {
            f(self)
        }
    }

    pub fn push(&mut self, item: T) {
        match &mut self.items {
            Some(items) => items.push(item),
            None => {}
        }
    }

    pub fn into_vec(self) -> Vec<T> {
        match self.items {
            Some(items) => items.into_vec(),
            None => Vec::new(),
        }
    }

    pub fn items(&self) -> Option<&smallvec::SmallVec<[T; 8]>> {
        self.items.as_ref()
    }

    pub fn take_items(&mut self) -> Option<smallvec::SmallVec<[T; 8]>> {
        self.items.take()
    }
}
