use std::sync::Arc;

pub struct ArcOpt<T> {
    value: Arc<Option<T>>,
}

impl<T> Clone for ArcOpt<T> {
    fn clone(&self) -> Self {
        Self {
            value: Arc::clone(&self.value),
        }
    }
}

impl<T> AsRef<T> for ArcOpt<T> {
    fn as_ref(&self) -> &T {
        self.value.as_ref().as_ref().expect("ArcOpt contains None")
    }
}

impl<T> ArcOpt<T> {
    pub fn new(value: T) -> Self {
        Self {
            value: Arc::new(Some(value)),
        }
    }

    pub fn modify<F, E>(&mut self, f: F) -> Result<Option<()>, E>
    where
        F: FnOnce(T) -> Result<T, E>,
    {
        match Arc::get_mut(&mut self.value) {
            Some(opt) => match opt.take() {
                Some(v) => match f(v) {
                    Ok(new_value) => {
                        *opt = Some(new_value);
                        Ok(Some(()))
                    }
                    Err(e) => Err(e),
                },
                None => panic!("ArcOpt contains None"),
            },
            None => Ok(None),
        }
    }
}
