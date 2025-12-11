use std::sync::Arc;

use smallvec::SmallVec;

use crate::{
    types::{AsDispatcher, CoinductiveType, GcAllocObject, TypeError},
    util::three_valued_logic::ThreeValuedLogic,
};

pub struct Environment<U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    type_vars: Vec<(Arc<str>, Option<U>)>,
    _phantom: std::marker::PhantomData<V>,
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Clone for Environment<U, V> {
    fn clone(&self) -> Self {
        Self {
            type_vars: self.type_vars.iter().map(|(name, ty)| (name.clone(), ty.clone())).collect(),
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Environment<U, V> {
    pub fn new<I: IntoIterator<Item = S>, S: Into<Arc<str>>>(type_vars: I) -> Self {
        Self {
            type_vars: type_vars.into_iter().map(|s| (s.into(), None)).collect(),
            _phantom: std::marker::PhantomData,
        }
    }

    pub fn new_exact<I: IntoIterator<Item = (S, U)>, S: Into<Arc<str>>>(type_vars: I) -> Self {
        Self {
            type_vars: type_vars.into_iter().map(|(s, ty)| (s.into(), Some(ty))).collect(),
            _phantom: std::marker::PhantomData,
        }
    }

    pub fn bind<'a, X: AsDispatcher<U, V>, S: AsRef<str>>(
        &mut self,
        name: S,
        ty: X,
        lhs_env: EnvironmentView<'a, U, V>,
        rhs_env: EnvironmentView<'a, U, V>,
    ) -> Result<(), TypeError<U, V>> {
        let ty = ty.into_dispatcher();
        for (var_name, var_ty) in self.type_vars.iter_mut() {
            if var_name.as_ref() == name.as_ref() {
                match var_ty {
                    Some(v) => {
                        if let ThreeValuedLogic::True =
                            ty.equals(v.as_ref_dispatcher(), lhs_env, rhs_env)?
                        {
                            return Ok(());
                        } else {
                            return Err(TypeError::AssertFailed((v.clone(), ty).into()));
                        }
                    }
                    None => {
                        *var_ty = Some(ty.into_dispatcher());
                        return Ok(());
                    }
                }
            }
        }
        Err(TypeError::UnboundEnvironmentVariable(name.as_ref().into()))
    }

    pub fn bind_no_except<'a, X: AsDispatcher<U, V>, S: AsRef<str>>(
        &mut self,
        name: S,
        ty: X,
        lhs_env: EnvironmentView<'a, U, V>,
        rhs_env: EnvironmentView<'a, U, V>,
    ) -> Result<bool, TypeError<U, V>> {
        let ty = ty.into_dispatcher();
        for (var_name, var_ty) in self.type_vars.iter_mut() {
            if var_name.as_ref() == name.as_ref() {
                return Ok(match var_ty {
                    Some(v) => {
                        matches!(
                            ty.equals(v.as_ref_dispatcher(), lhs_env, rhs_env)?,
                            ThreeValuedLogic::True
                        )
                    }
                    None => {
                        *var_ty = Some(ty.into_dispatcher());
                        true
                    }
                });
            }
        }
        Ok(false)
    }

    pub fn capture_from(mut self, other: EnvironmentView<U, V>) -> Result<Self, TypeError<U, V>> {
        for (var_name, var_ty) in self.type_vars.iter_mut() {
            if let Some(other_ty) = other.lookup(var_name.as_ref()) {
                *var_ty = Some(other_ty.clone());
            } else {
                return Err(TypeError::UnboundEnvironmentVariable(var_name.as_ref().into()));
            }
        }
        Ok(self)
    }

    pub fn lookup<S: AsRef<str>>(&self, name: S) -> Option<&U> {
        for (var_name, var_ty) in self.type_vars.iter() {
            if var_name.as_ref() == name.as_ref() {
                return var_ty.as_ref();
            }
        }
        None
    }

    pub fn view(&self) -> EnvironmentView<'_, U, V> {
        EnvironmentView::new(&self.type_vars)
    }

    pub fn type_vars(&self) -> &[(Arc<str>, Option<U>)] {
        &self.type_vars
    }
}

pub struct EnvironmentView<'a, U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    type_vars: &'a [(Arc<str>, Option<U>)],
    _phantom: std::marker::PhantomData<V>,
}

impl<'a, U: CoinductiveType<U, V>, V: GcAllocObject<V>> Clone for EnvironmentView<'a, U, V> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, U: CoinductiveType<U, V>, V: GcAllocObject<V>> Copy for EnvironmentView<'a, U, V> {}

impl<'a, U: CoinductiveType<U, V>, V: GcAllocObject<V>> EnvironmentView<'a, U, V> {
    pub fn new(type_vars: &'a [(Arc<str>, Option<U>)]) -> Self {
        Self { type_vars, _phantom: std::marker::PhantomData }
    }

    pub fn lookup<S: AsRef<str>>(&self, name: S) -> Option<&U> {
        for (var_name, var_ty) in self.type_vars.iter() {
            if var_name.as_ref() == name.as_ref() {
                return var_ty.as_ref();
            }
        }
        None
    }
}

pub struct EnvironmentStack<U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    stack: SmallVec<[Environment<U, V>; 4]>,
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> EnvironmentStack<U, V> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, env: Environment<U, V>) {
        self.stack.push(env);
    }

    pub fn pop(&mut self) -> Option<Environment<U, V>> {
        self.stack.pop()
    }

    pub fn lookup<S: AsRef<str>>(&self, name: S) -> Option<&U> {
        for env in self.stack.iter().rev() {
            if let Some(ty) = env.lookup(name.as_ref()) {
                return Some(ty);
            }
        }
        None
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Default for EnvironmentStack<U, V> {
    fn default() -> Self {
        Self { stack: SmallVec::new() }
    }
}
