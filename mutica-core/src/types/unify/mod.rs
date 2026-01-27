pub mod path_collector;
pub mod collector;
use std::sync::Arc;

use smallvec::{SmallVec, smallvec};

use crate::types::{AsDispatcher, CoinductiveType, GcAllocObject, Type, TypeError, anyof::AnyOf};

pub enum EnvironmentVarState<U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    FromPattern,
    FromCapture,
    Bound(U),
    BoundList(SmallVec<[U; 4]>),
    #[doc(hidden)]
    Phantom(std::marker::PhantomData<V>),
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Clone for EnvironmentVarState<U, V> {
    fn clone(&self) -> Self {
        match self {
            EnvironmentVarState::FromPattern => EnvironmentVarState::FromPattern,
            EnvironmentVarState::FromCapture => EnvironmentVarState::FromCapture,
            EnvironmentVarState::Bound(ty) => EnvironmentVarState::Bound(ty.clone()),
            EnvironmentVarState::BoundList(tys) => EnvironmentVarState::BoundList(tys.clone()),
            EnvironmentVarState::Phantom(_) => {
                EnvironmentVarState::Phantom(std::marker::PhantomData)
            }
        }
    }
}

pub struct Environment<U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    type_vars: Vec<(Arc<str>, EnvironmentVarState<U, V>)>,
    _phantom: std::marker::PhantomData<V>,
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Clone for Environment<U, V> {
    fn clone(&self) -> Self {
        Self { type_vars: self.type_vars.clone(), _phantom: std::marker::PhantomData }
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Environment<U, V> {
    pub fn new<I: IntoIterator<Item = (S, EnvironmentVarState<U, V>)>, S: Into<Arc<str>>>(
        type_vars: I,
    ) -> Self {
        Self {
            type_vars: type_vars.into_iter().map(|(s, state)| (s.into(), state)).collect(),
            _phantom: std::marker::PhantomData,
        }
    }

    pub fn new_bound<I: IntoIterator<Item = (S, U)>, S: Into<Arc<str>>>(type_vars: I) -> Self {
        Self {
            type_vars: type_vars
                .into_iter()
                .map(|(s, ty)| (s.into(), EnvironmentVarState::Bound(ty)))
                .collect(),
            _phantom: std::marker::PhantomData,
        }
    }

    pub fn bind<X: AsDispatcher<U, V>, S: AsRef<str>>(
        &mut self,
        name: S,
        ty: X,
    ) -> Result<(), TypeError<U, V>> {
        let ty = ty.into_dispatcher();
        for (var_name, var_ty) in self.type_vars.iter_mut() {
            if var_name.as_ref() == name.as_ref() {
                match var_ty {
                    EnvironmentVarState::Bound(_) => {
                        panic!(
                            "CRITICAL: Trying to bind already bound variable '{}'",
                            name.as_ref(),
                        )
                    }
                    EnvironmentVarState::BoundList(v) => {
                        v.push(ty.into_dispatcher());
                        return Ok(());
                    }
                    EnvironmentVarState::FromPattern | EnvironmentVarState::FromCapture => {
                        *var_ty = EnvironmentVarState::BoundList(smallvec![ty.into_dispatcher()]);
                        return Ok(());
                    }
                    EnvironmentVarState::Phantom(_) => unreachable!(),
                }
            }
        }
        Err(TypeError::UnboundEnvironmentVariable(name.as_ref().into()))
    }

    pub fn capture_from(
        mut self,
        pattern_env: EnvironmentView<U, V>,
        capture_env: EnvironmentView<U, V>,
    ) -> Result<Self, TypeError<U, V>> {
        for (var_name, var_ty) in self.type_vars.iter_mut() {
            match var_ty {
                EnvironmentVarState::FromPattern => {
                    if let Some(other_ty) = pattern_env.lookup(var_name.as_ref()) {
                        *var_ty = EnvironmentVarState::Bound(other_ty.clone());
                    } else {
                        return Err(TypeError::UnboundEnvironmentVariable(
                            var_name.as_ref().into(),
                        ));
                    }
                }
                EnvironmentVarState::FromCapture => {
                    if let Some(other_ty) = capture_env.lookup(var_name.as_ref()) {
                        *var_ty = EnvironmentVarState::Bound(other_ty.clone());
                    } else {
                        return Err(TypeError::UnboundEnvironmentVariable(
                            var_name.as_ref().into(),
                        ));
                    }
                }
                EnvironmentVarState::Bound(_) => {}
                EnvironmentVarState::BoundList(_) => panic!(
                    "CRITICAL: Trying to capture a BoundList variable from an environment which didn't finalize it."
                ),
                EnvironmentVarState::Phantom(_) => unreachable!(),
            }
        }
        Ok(self)
    }

    pub fn lookup<S: AsRef<str>>(&self, name: S) -> Option<&U> {
        for (var_name, var_ty) in self.type_vars.iter() {
            if var_name.as_ref() == name.as_ref() {
                if let EnvironmentVarState::Bound(ty) = var_ty {
                    return Some(ty);
                } else {
                    return None;
                }
            }
        }
        None
    }

    pub fn view(&self) -> EnvironmentView<'_, U, V> {
        EnvironmentView::new(&self.type_vars)
    }

    pub fn type_vars(&self) -> &[(Arc<str>, EnvironmentVarState<U, V>)] {
        &self.type_vars
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Environment<Type<T>, T> {
    pub fn finalize<'a>(
        &mut self,
        env: EnvironmentView<'a, Type<T>, T>,
    ) -> Result<(), TypeError<Type<T>, T>> {
        // 把所有BoundList变量转换为Bound
        for (_, var_ty) in self.type_vars.iter_mut() {
            if let EnvironmentVarState::BoundList(tys) = var_ty {
                *var_ty = EnvironmentVarState::Bound(AnyOf::new(tys.iter(), None, env)?)
            }
        }
        Ok(())
    }
}
pub struct EnvironmentView<'a, U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    type_vars: &'a [(Arc<str>, EnvironmentVarState<U, V>)],
}

impl<'a, U: CoinductiveType<U, V>, V: GcAllocObject<V>> Clone for EnvironmentView<'a, U, V> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, U: CoinductiveType<U, V>, V: GcAllocObject<V>> Copy for EnvironmentView<'a, U, V> {}

impl<'a, U: CoinductiveType<U, V>, V: GcAllocObject<V>> EnvironmentView<'a, U, V> {
    pub fn new(type_vars: &'a [(Arc<str>, EnvironmentVarState<U, V>)]) -> Self {
        Self { type_vars }
    }

    pub fn lookup<S: AsRef<str>>(&self, name: S) -> Option<&U> {
        for (var_name, var_ty) in self.type_vars.iter() {
            if var_name.as_ref() == name.as_ref() {
                if let EnvironmentVarState::Bound(ty) = var_ty {
                    return Some(ty);
                } else {
                    return None;
                }
            }
        }
        None
    }

    pub fn type_vars(&self) -> &'a [(Arc<str>, EnvironmentVarState<U, V>)] {
        self.type_vars
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Default for Environment<U, V> {
    fn default() -> Self {
        Self { type_vars: Vec::new(), _phantom: std::marker::PhantomData }
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Default for EnvironmentView<'_, U, V> {
    fn default() -> Self {
        Self { type_vars: &[] }
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

    // pub fn lookup<S: AsRef<str>>(&self, name: S) -> Option<&U> {
    //     for env in self.stack.iter().rev() {
    //         if let Some(ty) = env.lookup(name.as_ref()) {
    //             return Some(ty);
    //         }
    //     }
    //     None
    // }

    pub fn lookup_at_last_layer<S: AsRef<str>>(&self, name: S) -> Option<&U> {
        if let Some(env) = self.stack.last() { env.lookup(name.as_ref()) } else { None }
    }

    pub fn layers(&self) -> usize {
        self.stack.len()
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Default for EnvironmentStack<U, V> {
    fn default() -> Self {
        Self { stack: SmallVec::new() }
    }
}
