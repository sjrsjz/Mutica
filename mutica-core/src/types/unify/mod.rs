pub mod capture_env;
pub mod collector;
pub mod path_collector;
use std::sync::Arc;

use smallvec::SmallVec;

use crate::types::{
    AsDispatcher, CoinductiveType, GcAllocObject, Type, TypeError, anyof::AnyOf,
    unify::capture_env::CaptureEnvList,
};

pub enum ArgumentBinding<U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    Bound(U),
    Collect(SmallVec<[U; 4]>),
    #[doc(hidden)]
    Phantom(std::marker::PhantomData<V>),
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> ArgumentBinding<U, V> {
    pub fn get_bound(&self) -> Option<&U> {
        match self {
            ArgumentBinding::Bound(ty) => Some(ty),
            _ => None,
        }
    }
}

#[derive(Default)]
pub enum Environment<U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    #[default]
    Placeholder,
    PatternBinding {
        type_vars: Vec<(Arc<str>, ArgumentBinding<U, V>)>,
    },
    SubtypeAssumption {
        // (sub, sup, layer_sub, layer_sup)
        subtype_assumptions: Vec<(Arc<str>, Arc<str>, usize, usize)>,
    },
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Environment<U, V> {
    pub fn placeholder() -> Self {
        Environment::Placeholder
    }

    pub fn pattern_binding<
        I: IntoIterator<Item = (S, ArgumentBinding<U, V>)>,
        S: Into<Arc<str>>,
    >(
        type_vars: I,
    ) -> Self {
        Environment::PatternBinding {
            type_vars: type_vars.into_iter().map(|(s, state)| (s.into(), state)).collect(),
        }
    }

    pub fn subtype_assumption<
        J: IntoIterator<Item = (P, Q, usize, usize)>,
        P: Into<Arc<str>>,
        Q: Into<Arc<str>>,
    >(
        subtype_assumptions: J,
    ) -> Self {
        Environment::SubtypeAssumption {
            subtype_assumptions: subtype_assumptions
                .into_iter()
                .map(|(p, q, layer_sub, layer_sup)| (p.into(), q.into(), layer_sub, layer_sup))
                .collect(),
        }
    }

    pub fn new_bound<I: IntoIterator<Item = (S, U)>, S: Into<Arc<str>>>(type_vars: I) -> Self {
        Environment::PatternBinding {
            type_vars: type_vars
                .into_iter()
                .map(|(s, ty)| (s.into(), ArgumentBinding::Bound(ty)))
                .collect(),
        }
    }

    pub fn is_reduced(&self) -> bool {
        let Environment::PatternBinding { type_vars } = self else {
            return true;
        };
        for (_, var_ty) in type_vars.iter() {
            match var_ty {
                ArgumentBinding::Collect(_) => {
                    return false;
                }
                ArgumentBinding::Bound(_) => {}
                ArgumentBinding::Phantom(_) => unreachable!(),
            }
        }
        true
    }

    pub fn bind<X: AsDispatcher<U, V>, S: AsRef<str>>(
        &mut self,
        name: S,
        ty: X,
    ) -> Result<(), TypeError<U, V>> {
        let Environment::PatternBinding { type_vars } = self else {
            return Err(TypeError::UnboundArgument(name.as_ref().into()));
        };

        let ty = ty.into_dispatcher();
        for (var_name, var_ty) in type_vars.iter_mut() {
            if var_name.as_ref() == name.as_ref() {
                match var_ty {
                    ArgumentBinding::Bound(_) => {
                        panic!(
                            "CRITICAL: Trying to bind already bound variable '{}'",
                            name.as_ref(),
                        )
                    }
                    ArgumentBinding::Collect(v) => {
                        v.push(ty.into_dispatcher());
                        return Ok(());
                    }
                    ArgumentBinding::Phantom(_) => unreachable!(),
                }
            }
        }
        Err(TypeError::UnboundArgument(name.as_ref().into()))
    }

    pub fn lookup<S: AsRef<str>>(&self, name: S) -> Option<&U> {
        let Environment::PatternBinding { type_vars } = self else {
            return None;
        };
        for (var_name, var_ty) in type_vars.iter() {
            if var_name.as_ref() == name.as_ref() {
                if let ArgumentBinding::Bound(ty) = var_ty {
                    return Some(ty);
                } else {
                    return None;
                }
            }
        }
        None
    }

    pub fn view(&self) -> EnvironmentView<'_, U, V> {
        match self {
            Environment::PatternBinding { type_vars } => EnvironmentView::new(type_vars),
            Environment::Placeholder | Environment::SubtypeAssumption { .. } => {
                EnvironmentView::default()
            }
        }
    }

    pub fn type_vars(&self) -> &[(Arc<str>, ArgumentBinding<U, V>)] {
        match self {
            Environment::PatternBinding { type_vars } => type_vars,
            Environment::Placeholder | Environment::SubtypeAssumption { .. } => &[],
        }
    }

    pub fn subtype_assumptions(&self) -> &[(Arc<str>, Arc<str>, usize, usize)] {
        match self {
            Environment::SubtypeAssumption { subtype_assumptions } => subtype_assumptions,
            Environment::Placeholder | Environment::PatternBinding { .. } => &[],
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Environment<Type<T>, T> {
    pub fn finalize<'a>(
        &mut self,
        capture_env: CaptureEnvList<'a, Type<T>, T>,
    ) -> Result<(), TypeError<Type<T>, T>> {
        let Environment::PatternBinding { type_vars } = self else {
            return Ok(());
        };
        // 把所有BoundList变量转换为Bound
        for (_, var_ty) in type_vars.iter_mut() {
            if let ArgumentBinding::Collect(tys) = var_ty {
                *var_ty = ArgumentBinding::Bound(AnyOf::new(tys.iter(), None, capture_env)?)
            }
        }
        Ok(())
    }
}
pub struct EnvironmentView<'a, U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    type_vars: &'a [(Arc<str>, ArgumentBinding<U, V>)],
}

impl<'a, U: CoinductiveType<U, V>, V: GcAllocObject<V>> Clone for EnvironmentView<'a, U, V> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, U: CoinductiveType<U, V>, V: GcAllocObject<V>> Copy for EnvironmentView<'a, U, V> {}

impl<'a, U: CoinductiveType<U, V>, V: GcAllocObject<V>> EnvironmentView<'a, U, V> {
    pub fn new(type_vars: &'a [(Arc<str>, ArgumentBinding<U, V>)]) -> Self {
        Self { type_vars }
    }

    pub fn lookup<S: AsRef<str>>(&self, name: S) -> Option<&U> {
        for (var_name, var_ty) in self.type_vars.iter() {
            if var_name.as_ref() == name.as_ref() {
                if let ArgumentBinding::Bound(ty) = var_ty {
                    return Some(ty);
                } else {
                    return None;
                }
            }
        }
        None
    }

    pub fn type_vars(&self) -> &'a [(Arc<str>, ArgumentBinding<U, V>)] {
        self.type_vars
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

    pub fn lookup_at_layer<S: AsRef<str>>(&self, name: S, layer: usize) -> Option<Option<&U>> {
        match self.stack.get(layer) {
            Some(env) => match env.lookup(name.as_ref()) {
                Some(ty) => Some(Some(ty)),
                None => Some(None),
            },
            None => None,
        }
    }

    pub fn lookup_subtype_assumption<M: AsRef<str>, N: AsRef<str>>(
        &self,
        sub: M,
        sup: N,
        layer_sub: usize,
        layer_sup: usize,
    ) -> Option<bool> {
        if layer_sub >= self.stack.len() || layer_sup >= self.stack.len() {
            return None;
        }
        for env in self.stack.iter().rev() {
            for (p, q, l_sub, l_sup) in env.subtype_assumptions().iter() {
                if p.as_ref() == sub.as_ref()
                    && q.as_ref() == sup.as_ref()
                    && *l_sub == layer_sub
                    && *l_sup == layer_sup
                {
                    return Some(true);
                }
            }
        }
        Some(false)
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
