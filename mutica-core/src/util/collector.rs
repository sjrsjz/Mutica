use crate::util::three_valued_logic::ThreeValuedLogic;

pub struct Collector<T> {
    items: Option<smallvec::SmallVec<[T; 8]>>,
}

impl<T> Default for Collector<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Collector<T> {
    pub fn new() -> Self {
        Self {
            items: Some(smallvec::SmallVec::new()),
        }
    }

    /// 声明一个禁用的 Collector，所有操作都将直接生效且不进行收集
    /// 含义为禁用模式匹配环境
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

    pub fn collect<F, E>(&mut self, f: F) -> Result<ThreeValuedLogic, E>
    where
        F: FnOnce(&mut Self) -> Result<ThreeValuedLogic, E>,
    {
        if self.items.is_some() {
            let len = self.items.as_ref().unwrap().len();
            let result = f(self);
            match result {
                Ok(ThreeValuedLogic::True) => Ok(ThreeValuedLogic::True),
                Ok(ThreeValuedLogic::Unknown) => {
                    self.items.as_mut().unwrap().truncate(len);
                    Ok(ThreeValuedLogic::Unknown)
                }
                Ok(ThreeValuedLogic::False) => {
                    self.items.as_mut().unwrap().truncate(len);
                    Ok(ThreeValuedLogic::False)
                }
                Err(e) => Err(e),
            }
        } else {
            f(self)
        }
    }

    pub fn push(&mut self, item: T) {
        if let Some(items) = &mut self.items {
            items.push(item)
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

    pub fn clear(&mut self) {
        if let Some(items) = &mut self.items {
            items.clear();
        } else {
            self.items = Some(smallvec::SmallVec::new());
        }
    }
}
