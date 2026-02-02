pub mod capture_env;
pub mod collector;
pub mod path_collector;
use std::{fmt::Debug, sync::Arc};

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

pub enum GenericBinding<'a, U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    WaitForBind {
        parent: Option<&'a GenericBinding<'a, U, V>>,
        is_lhs: bool,
    }, // 等待绑定
    Pattern {
        lhs_type_vars: &'a [(Arc<str>, ArgumentBinding<U, V>)],
        rhs_type_vars: &'a [(Arc<str>, ArgumentBinding<U, V>)],
        parent: Option<&'a GenericBinding<'a, U, V>>,
        is_lhs: bool,
    },
    SubtypeAssumption {
        // (sub, sup)
        lhs_subtype_assumptions: &'a [(Arc<str>, Arc<str>)],
        rhs_subtype_assumptions: &'a [(Arc<str>, Arc<str>)],
        parent: Option<&'a GenericBinding<'a, U, V>>,
        is_lhs: bool,
    },
    ParamSubtypeAssumption {
        // (sub, sup)
        lhs_subtype_assumptions: &'a [(Arc<str>, Arc<str>)],
        rhs_subtype_assumptions: &'a [(Arc<str>, Arc<str>)],
        parent: Option<&'a GenericBinding<'a, U, V>>,
        is_lhs: bool,
    },
}

impl<'a, U: CoinductiveType<U, V>, V: GcAllocObject<V>> Debug for GenericBinding<'a, U, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GenericBinding::WaitForBind { is_lhs, .. } => {
                write!(f, "WaitForBind(is_lhs: {}, parent: {:?})", is_lhs, self.parent())
            }
            GenericBinding::Pattern { is_lhs, .. } => {
                write!(f, "Pattern(is_lhs: {}, parent: {:?})", is_lhs, self.parent())
            }
            GenericBinding::SubtypeAssumption {
                is_lhs,
                lhs_subtype_assumptions,
                rhs_subtype_assumptions,
                ..
            } => {
                write!(
                    f,
                    "SubtypeAssumption(is_lhs: {}, lhs: {:?}, rhs: {:?}, parent: {:?})",
                    is_lhs,
                    lhs_subtype_assumptions,
                    rhs_subtype_assumptions,
                    self.parent()
                )
            }
            GenericBinding::ParamSubtypeAssumption {
                is_lhs,
                lhs_subtype_assumptions,
                rhs_subtype_assumptions,
                ..
            } => {
                write!(
                    f,
                    "ParamSubtypeAssumption(is_lhs: {}, lhs: {:?}, rhs: {:?}, parent: {:?})",
                    is_lhs,
                    lhs_subtype_assumptions,
                    rhs_subtype_assumptions,
                    self.parent()
                )
            }
        }
    }
}

impl<'a, U: CoinductiveType<U, V>, V: GcAllocObject<V>> GenericBinding<'a, U, V> {
    pub fn wait_for_bind(parent: Option<&'a GenericBinding<'a, U, V>>) -> Self {
        if let Some(binding) = parent {
            GenericBinding::WaitForBind { parent, is_lhs: binding.is_lhs() }
        } else {
            GenericBinding::WaitForBind { parent, is_lhs: true }
        }
    }

    pub fn pattern(
        lhs_type_vars: &'a [(Arc<str>, ArgumentBinding<U, V>)], // 相对于调用者而言是LHS，但是实际上受父环境 `is_lhs` 控制
        rhs_type_vars: &'a [(Arc<str>, ArgumentBinding<U, V>)],
        parent: Option<&'a GenericBinding<'a, U, V>>,
    ) -> Self {
        if let Some(binding) = parent {
            GenericBinding::Pattern {
                lhs_type_vars,
                rhs_type_vars,
                parent,
                is_lhs: binding.is_lhs(),
            }
        } else {
            GenericBinding::Pattern { lhs_type_vars, rhs_type_vars, parent, is_lhs: true }
        }
    }

    pub fn subtype_assumption(
        lhs_assumptions: &'a [(Arc<str>, Arc<str>)],
        rhs_assumptions: &'a [(Arc<str>, Arc<str>)],
        parent: Option<&'a GenericBinding<'a, U, V>>,
    ) -> Self {
        if let Some(binding) = parent {
            GenericBinding::SubtypeAssumption {
                lhs_subtype_assumptions: lhs_assumptions,
                rhs_subtype_assumptions: rhs_assumptions,
                parent,
                is_lhs: binding.is_lhs(),
            }
        } else {
            GenericBinding::SubtypeAssumption {
                lhs_subtype_assumptions: lhs_assumptions,
                rhs_subtype_assumptions: rhs_assumptions,
                parent,
                is_lhs: true,
            }
        }
    }

    pub fn param_subtype_assumption(
        lhs_assumptions: &'a [(Arc<str>, Arc<str>)],
        rhs_assumptions: &'a [(Arc<str>, Arc<str>)],
        parent: Option<&'a GenericBinding<'a, U, V>>,
    ) -> Self {
        if let Some(binding) = parent {
            GenericBinding::ParamSubtypeAssumption {
                lhs_subtype_assumptions: lhs_assumptions,
                rhs_subtype_assumptions: rhs_assumptions,
                parent,
                is_lhs: binding.is_lhs(),
            }
        } else {
            GenericBinding::ParamSubtypeAssumption {
                lhs_subtype_assumptions: lhs_assumptions,
                rhs_subtype_assumptions: rhs_assumptions,
                parent,
                is_lhs: true,
            }
        }
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

    pub fn lookup<S: AsRef<str>>(&self, name: S, layer: usize, lhs: bool) -> Option<Option<&U>> {
        let binding = self.find_generic_layer(layer)?;
        let GenericBinding::Pattern { lhs_type_vars, rhs_type_vars, is_lhs: layer_is_lhs, .. } =
            binding
        else {
            return Some(None);
        };
        if *layer_is_lhs == lhs {
            for (var_name, var_ty) in lhs_type_vars.iter() {
                if var_name.as_ref() == name.as_ref() {
                    if let ArgumentBinding::Bound(ty) = var_ty {
                        return Some(Some(ty));
                    } else {
                        return Some(None);
                    }
                }
            }
            Some(None)
        } else {
            for (var_name, var_ty) in rhs_type_vars.iter() {
                if var_name.as_ref() == name.as_ref() {
                    if let ArgumentBinding::Bound(ty) = var_ty {
                        return Some(Some(ty));
                    } else {
                        return Some(None);
                    }
                }
            }
            Some(None)
        }
    }

    pub fn type_vars(&self, lhs: bool) -> &[(Arc<str>, ArgumentBinding<U, V>)] {
        match self {
            GenericBinding::Pattern { lhs_type_vars, rhs_type_vars, .. } => {
                if self.is_lhs() == lhs { lhs_type_vars } else { rhs_type_vars }
            }
            GenericBinding::WaitForBind { .. }
            | GenericBinding::SubtypeAssumption { .. }
            | GenericBinding::ParamSubtypeAssumption { .. } => &[],
        }
    }
    pub fn check_subtype_assumption<S: AsRef<str>>(&self, sub: S, sup: S, lhs: bool) -> bool {
        match self {
            GenericBinding::SubtypeAssumption {
                lhs_subtype_assumptions,
                rhs_subtype_assumptions,
                ..
            } => {
                let (sub, sup) = if self.is_lhs() != lhs {
                    (sup.as_ref(), sub.as_ref())
                } else {
                    (sub.as_ref(), sup.as_ref())
                };
                lhs_subtype_assumptions
                    .iter()
                    .chain(rhs_subtype_assumptions.iter())
                    .any(|(lhs, rhs)| lhs.as_ref() == sub && rhs.as_ref() == sup)
            }
            GenericBinding::ParamSubtypeAssumption {
                lhs_subtype_assumptions,
                rhs_subtype_assumptions,
                ..
            } => {
                let (sub, sup) = if self.is_lhs() != lhs {
                    (sup.as_ref(), sub.as_ref())
                } else {
                    (sub.as_ref(), sup.as_ref())
                };
                lhs_subtype_assumptions
                    .iter()
                    .chain(rhs_subtype_assumptions.iter())
                    .any(|(lhs, rhs)| lhs.as_ref() == sub && rhs.as_ref() == sup)
            }
            GenericBinding::WaitForBind { .. } | GenericBinding::Pattern { .. } => false,
        }
    }

    pub fn flip(&self) -> Self {
        match self {
            GenericBinding::SubtypeAssumption {
                lhs_subtype_assumptions,
                rhs_subtype_assumptions,
                parent,
                is_lhs,
            } => GenericBinding::SubtypeAssumption {
                lhs_subtype_assumptions,
                rhs_subtype_assumptions,
                parent: *parent,
                is_lhs: !is_lhs,
            },
            GenericBinding::ParamSubtypeAssumption {
                lhs_subtype_assumptions,
                rhs_subtype_assumptions,
                parent,
                is_lhs,
            } => GenericBinding::ParamSubtypeAssumption {
                lhs_subtype_assumptions,
                rhs_subtype_assumptions,
                parent: *parent,
                is_lhs: !is_lhs,
            },
            GenericBinding::Pattern { lhs_type_vars, rhs_type_vars, parent, is_lhs } => {
                GenericBinding::Pattern {
                    lhs_type_vars,
                    rhs_type_vars,
                    parent: *parent,
                    is_lhs: !is_lhs,
                }
            }
            GenericBinding::WaitForBind { parent, is_lhs } => {
                GenericBinding::WaitForBind { parent: *parent, is_lhs: !is_lhs }
            }
        }
    }

    pub fn parent(&self) -> Option<&'a GenericBinding<'a, U, V>> {
        match self {
            GenericBinding::WaitForBind { parent, .. } => *parent,
            GenericBinding::Pattern { parent, .. } => *parent,
            GenericBinding::SubtypeAssumption { parent, .. } => *parent,
            GenericBinding::ParamSubtypeAssumption { parent, .. } => *parent,
        }
    }

    pub fn is_lhs(&self) -> bool {
        match self {
            GenericBinding::WaitForBind { is_lhs, .. } => *is_lhs,
            GenericBinding::Pattern { is_lhs, .. } => *is_lhs,
            GenericBinding::SubtypeAssumption { is_lhs, .. } => *is_lhs,
            GenericBinding::ParamSubtypeAssumption { is_lhs, .. } => *is_lhs,
        }
    }

    pub fn find_param_layer(&'a self, layer: usize) -> Option<&'a GenericBinding<'a, U, V>> {
        let mut current = Some(self);
        let mut param_layer_count = 0;
        while let Some(binding) = current {
            match binding {
                GenericBinding::ParamSubtypeAssumption { parent, .. } => {
                    if param_layer_count == layer {
                        return Some(binding);
                    }
                    param_layer_count += 1;
                    current = *parent;
                }
                _ => {
                    current = binding.parent();
                }
            }
        }
        None
    }

    pub fn find_generic_layer(&'a self, layer: usize) -> Option<&'a GenericBinding<'a, U, V>> {
        let mut current = Some(self);
        let mut generic_layer_count = 0;
        while let Some(binding) = current {
            match binding {
                GenericBinding::Pattern { parent, .. }
                | GenericBinding::SubtypeAssumption { parent, .. }
                | GenericBinding::WaitForBind { parent, .. } => {
                    if generic_layer_count == layer {
                        return Some(binding);
                    }
                    generic_layer_count += 1;
                    current = *parent;
                }
                // GenericBinding::WaitForBind { parent, .. } => {
                //     // 跳过 WaitForBind 层，这是因为 WaitForBind 表示处于约束收集阶段，并不实际存在泛型层级（和前端解析器的行为一致）
                //     current = *parent;
                // }
                GenericBinding::ParamSubtypeAssumption { .. } => {
                    // 遇到 ParamSubtypeAssumption 时说明已经超出 Generic 层级
                    return None;
                }
            }
        }
        None
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
