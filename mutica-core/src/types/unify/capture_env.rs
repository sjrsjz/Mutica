use std::sync::Arc;

use arc_gc::traceable::GCTraceable;
use smallvec::SmallVec;

use crate::{
    types::{CoinductiveType, GcAllocObject, unify::ArgumentBinding},
    util::rootstack::Rootable,
};

/// 表示捕获变量的来源
#[derive(Clone, Debug)]
pub enum CaptureOrigin {
    FromParentArgument, // 来自父函数的参数
    FromParentEnv,      // 来自父环境(即父闭包的捕获变量)
}

/// 捕获变量环境
pub enum CaptureEnv<U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    Unsolved(SmallVec<[(Arc<str>, CaptureOrigin); 4]>), // 未解决的捕获变量列表
    Solved(SmallVec<[(Arc<str>, U); 4]>),               // 已解决的捕获变量，包含变量名和对应类型
    #[doc(hidden)]
    Pandom(std::marker::PhantomData<V>), // 占位符，表示无效状态
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> CaptureEnv<U, V> {
    /// 从已解决的参数环境和父捕获环境中捕获变量，生成新的捕获环境
    pub fn capture_from(
        mut self,
        solved_argument: &[(Arc<str>, ArgumentBinding<U, V>)],
        parent_env: CaptureEnvList<U, V>,
    ) -> Result<Self, Arc<str>> {
        match &mut self {
            CaptureEnv::Unsolved(vars) => {
                let mut solved_vars: SmallVec<[(Arc<str>, U); 4]> = SmallVec::new();
                for (var_name, origin) in vars.iter() {
                    match origin {
                        CaptureOrigin::FromParentArgument => {
                            // 从已解决的参数环境中查找变量类型
                            let found = solved_argument
                                .iter()
                                .find(|(arg_name, _)| arg_name.as_ref() == var_name.as_ref());
                            match found {
                                Some((_, var_type)) => {
                                    // solved_vars.push((var_name.clone(), var_type.clone()));
                                    match var_type.get_bound() {
                                        Some(ty) => {
                                            solved_vars.push((var_name.clone(), ty.clone()));
                                        }
                                        None => {
                                            return Err(var_name.clone());
                                        }
                                    }
                                }
                                None => {
                                    return Err(var_name.clone());
                                }
                            }
                        }
                        CaptureOrigin::FromParentEnv => {
                            // 从父捕获环境中查找变量类型
                            match parent_env
                                .lookup(var_name.as_ref())
                                .map_err(|_| var_name.clone())?
                            {
                                Some(var_type) => {
                                    solved_vars.push((var_name.clone(), var_type.clone()));
                                }
                                None => {
                                    return Err(var_name.clone());
                                }
                            }
                        }
                    }
                }
                Ok(CaptureEnv::Solved(solved_vars))
            }
            CaptureEnv::Solved(_) => Ok(self),
            CaptureEnv::Pandom(_) => unreachable!("Invalid CaptureEnv state"),
        }
    }

    pub fn new_unsolved(vars: SmallVec<[(Arc<str>, CaptureOrigin); 4]>) -> CaptureEnv<U, V> {
        if vars.is_empty() {
            CaptureEnv::Solved(SmallVec::new())
        } else {
            CaptureEnv::Unsolved(vars)
        }
    }

    pub fn is_solved(&self) -> bool {
        matches!(self, CaptureEnv::Solved(_))
    }

    #[allow(clippy::type_complexity)]
    pub fn solved_vars(&self) -> Option<&SmallVec<[(Arc<str>, U); 4]>> {
        match self {
            CaptureEnv::Solved(vars) => Some(vars),
            _ => None,
        }
    }

    #[allow(clippy::type_complexity)]
    pub fn unsolved_vars(&self) -> Option<&SmallVec<[(Arc<str>, CaptureOrigin); 4]>> {
        match self {
            CaptureEnv::Unsolved(vars) => Some(vars),
            _ => None,
        }
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> GCTraceable<V> for CaptureEnv<U, V> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<V>>) {
        if self.rootless() {
            return;
        }
        match self {
            CaptureEnv::Unsolved(_) => {}
            CaptureEnv::Solved(vars) => {
                for (_, var_type) in vars.iter() {
                    var_type.collect(queue);
                }
            }
            CaptureEnv::Pandom(_) => {}
        }
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Rootable<V> for CaptureEnv<U, V> {
    fn upgrade(&self, collected: &mut Vec<arc_gc::arc::GCArc<V>>) {
        if self.rootless() {
            return;
        }
        match self {
            CaptureEnv::Unsolved(_) => {}
            CaptureEnv::Solved(vars) => {
                for (_, var_type) in vars.iter() {
                    var_type.upgrade(collected);
                }
            }
            CaptureEnv::Pandom(_) => {}
        }
    }

    fn rootless(&self) -> bool {
        match self {
            CaptureEnv::Unsolved(_) => true,
            CaptureEnv::Solved(v) => {
                for (_, var_type) in v.iter() {
                    if !var_type.rootless() {
                        return false;
                    }
                }
                true
            }
            CaptureEnv::Pandom(_) => true,
        }
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Clone for CaptureEnv<U, V> {
    fn clone(&self) -> Self {
        match self {
            CaptureEnv::Unsolved(vars) => CaptureEnv::Unsolved(vars.clone()),
            CaptureEnv::Solved(vars) => CaptureEnv::Solved(vars.clone()),
            CaptureEnv::Pandom(_) => CaptureEnv::Pandom(std::marker::PhantomData),
        }
    }
}

#[allow(clippy::type_complexity)]
pub struct CaptureEnvList<'a, U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    pub parent_env: Option<&'a CaptureEnvList<'a, U, V>>,
    pub parent_argument: Option<&'a [(Arc<str>, ArgumentBinding<U, V>)]>,
    pub local: &'a CaptureEnv<U, V>,
}

impl<'a, U: CoinductiveType<U, V>, V: GcAllocObject<V>> Clone for CaptureEnvList<'a, U, V> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, U: CoinductiveType<U, V>, V: GcAllocObject<V>> Copy for CaptureEnvList<'a, U, V> {}

pub enum CaptureEnvLookupError<'a, U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    Argument(usize), // 变量在第几层环境中找到了定义但是它是未绑定值的参数（正向计数，0表示第一层）
    NotCaptured(&'a CaptureEnv<U, V>), // 变量未被捕获
    NotFound,        // 变量在所有环境中均未找到
}

impl<'a, U: CoinductiveType<U, V>, V: GcAllocObject<V>> CaptureEnvList<'a, U, V> {
    pub fn new(local: &'a CaptureEnv<U, V>) -> Self {
        Self { parent_env: None, parent_argument: None, local }
    }

    #[allow(clippy::type_complexity)]
    pub fn with_parent(
        local: &'a CaptureEnv<U, V>,
        parent: &'a CaptureEnvList<'a, U, V>,
        parent_argument: Option<&'a [(Arc<str>, ArgumentBinding<U, V>)]>,
    ) -> Self {
        Self { parent_env: Some(parent), parent_argument, local }
    }

    #[allow(clippy::type_complexity)]
    pub fn attach(
        &'a self,
        local: &'a CaptureEnv<U, V>,
        parent_argument: Option<&'a [(Arc<str>, ArgumentBinding<U, V>)]>,
    ) -> Self {
        Self { parent_env: Some(self), parent_argument, local }
    }

    pub fn lookup(&self, name: &str) -> Result<Option<&U>, CaptureEnvLookupError<'a, U, V>> {
        self.lookup_recursive(name, 0)
    }

    #[stacksafe::stacksafe]
    fn lookup_recursive(
        &self,
        name: &str,
        layer: usize,
    ) -> Result<Option<&U>, CaptureEnvLookupError<'a, U, V>> {
        match self.local {
            CaptureEnv::Solved(vars) => {
                for (var_name, var_type) in vars.iter() {
                    if var_name.as_ref() == name {
                        return Ok(Some(var_type));
                    }
                }
                return Err(CaptureEnvLookupError::NotCaptured(self.local));
            }
            CaptureEnv::Unsolved(vars) => {
                let found = vars.iter().find(|(var_name, _)| var_name.as_ref() == name);
                match found {
                    Some((_, CaptureOrigin::FromParentEnv)) => {
                        // 继续向上查找
                        if let Some(parent) = self.parent_env {
                            parent.lookup_recursive(name, layer + 1)
                        } else {
                            Ok(None)
                        }
                    }
                    Some((_, CaptureOrigin::FromParentArgument)) => {
                        if let Some(parent_argument) = self.parent_argument {
                            match parent_argument
                                .iter()
                                .find(|(arg_name, _)| arg_name.as_ref() == name)
                            {
                                Some((_, arg_binding)) => match arg_binding.get_bound() {
                                    Some(ty) => Ok(Some(ty)),
                                    None => Err(CaptureEnvLookupError::Argument(layer)),
                                },
                                None => Err(CaptureEnvLookupError::Argument(layer)),
                            }
                        } else {
                            Err(CaptureEnvLookupError::Argument(layer))
                        }
                    }
                    None => {
                        return Err(CaptureEnvLookupError::NotCaptured(self.local));
                    }
                }
            }
            CaptureEnv::Pandom(_) => unreachable!("Invalid CaptureEnv state"),
        }
    }
}
