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

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Clone for ArgumentBinding<U, V> {
    fn clone(&self) -> Self {
        match self {
            ArgumentBinding::Bound(v) => ArgumentBinding::Bound(v.clone()),
            ArgumentBinding::Collect(v) => ArgumentBinding::Collect(v.clone()),
            ArgumentBinding::Phantom(_) => ArgumentBinding::Phantom(std::marker::PhantomData),
        }
    }
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
pub enum GenericBinding<'a, U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    #[default]
    WaitForBind, // 等待绑定
    Pattern {
        type_vars: &'a [(Arc<str>, ArgumentBinding<U, V>)],
        type_vars_rev: &'a [(Arc<str>, ArgumentBinding<U, V>)],
    },
    SubtypeAssumption {
        // (sub, sup)
        subtype_assumptions: &'a [(Arc<str>, Arc<str>)],
        subtype_assumptions_rev: &'a [(Arc<str>, Arc<str>)],
        is_params: bool,
    },
}

impl<'a, U: CoinductiveType<U, V>, V: GcAllocObject<V>> GenericBinding<'a, U, V> {
    pub fn wait_for_bind() -> Self {
        GenericBinding::WaitForBind
    }

    pub fn pattern(
        type_vars: &'a mut [(Arc<str>, ArgumentBinding<U, V>)],
        type_vars_rev: &'a mut [(Arc<str>, ArgumentBinding<U, V>)],
    ) -> Self {
        GenericBinding::Pattern { type_vars, type_vars_rev }
    }

    pub fn subtype_assumption(
        assumptions: &'a [(Arc<str>, Arc<str>)],
        assumptions_rev: &'a [(Arc<str>, Arc<str>)],
        is_params: bool,
    ) -> Self {
        GenericBinding::SubtypeAssumption {
            subtype_assumptions: assumptions,
            subtype_assumptions_rev: assumptions_rev,
            is_params,
        }
    }

    pub fn is_reduced(&self) -> bool {
        let GenericBinding::Pattern { type_vars, .. } = self else {
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
        type_vars: &mut [(Arc<str>, ArgumentBinding<U, V>)],
        name: S,
        ty: X,
    ) -> Result<(), TypeError<U, V>> {
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
        let GenericBinding::Pattern { type_vars, .. } = self else {
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

    pub fn type_vars(&self) -> &[(Arc<str>, ArgumentBinding<U, V>)] {
        match self {
            GenericBinding::Pattern { type_vars, .. } => type_vars,
            GenericBinding::WaitForBind | GenericBinding::SubtypeAssumption { .. } => &[],
        }
    }

    pub fn subtype_assumptions(&self) -> &[(Arc<str>, Arc<str>)] {
        match self {
            GenericBinding::SubtypeAssumption { subtype_assumptions, .. } => subtype_assumptions,
            GenericBinding::WaitForBind | GenericBinding::Pattern { .. } => &[],
        }
    }

    pub fn flip(&self) -> Self {
        match self {
            GenericBinding::SubtypeAssumption {
                subtype_assumptions,
                subtype_assumptions_rev,
                is_params,
            } => GenericBinding::SubtypeAssumption {
                subtype_assumptions: subtype_assumptions_rev,
                subtype_assumptions_rev: subtype_assumptions,
                is_params: *is_params,
            },
            GenericBinding::Pattern { type_vars, type_vars_rev } => {
                GenericBinding::Pattern { type_vars: type_vars_rev, type_vars_rev: type_vars }
            }
            GenericBinding::WaitForBind => GenericBinding::WaitForBind,
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GenericBinding<'_, Type<T>, T> {
    #[allow(clippy::type_complexity)]
    pub fn finalize<'a>(
        type_vars: &mut [(Arc<str>, ArgumentBinding<Type<T>, T>)],
        capture_env: CaptureEnvList<'a, Type<T>, T>,
    ) -> Result<(), TypeError<Type<T>, T>> {
        // 把所有BoundList变量转换为Bound
        for (_, var_ty) in type_vars.iter_mut() {
            if let ArgumentBinding::Collect(tys) = var_ty {
                *var_ty = ArgumentBinding::Bound(AnyOf::new(tys.iter(), None, capture_env)?)
            }
        }
        Ok(())
    }
}
