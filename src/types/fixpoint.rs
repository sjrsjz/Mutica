use std::sync::OnceLock;

use arc_gc::{
    arc::{GCArc, GCArcWeak},
    gc::GC,
    traceable::GCTraceable,
};

use crate::{
    as_type,
    types::{
        AsTypeRef, CoinductiveType, CoinductiveTypeWithAny, GCArcStorage, Representable, Rootable,
        Stabilized, StabilizedType, TaggedPtr, Type, TypeError,
        closure::{ClosureEnv, ParamEnv},
        type_bound::TypeBound,
    },
    util::{collector::Collector, cycle_detector::FastCycleDetector},
};

/// # 不动点算子内部实现 (Fixed-Point Operator Inner Implementation)
///
/// `FixPointInner` 是不动点类型的内部存储结构，使用 `OnceLock`
/// 实现**延迟初始化**，支持递归类型的前向引用。
///
/// ## 设计原理
///
/// 在类型理论中，不动点算子 **μX.T(X)** 表示满足方程 **X = T(X)** 的类型。
/// 例如：
/// - 自然数：`Nat = μX. () | X`  
/// - 列表：`List<A> = μX. () | (A, X)`
/// - 树：`Tree<A> = μX. A | (X, X)`
///
/// ## 延迟初始化机制
///
/// 由于递归类型的定义需要引用自身，我们必须：
/// 1. **先创建占位符**：分配类型引用但不指定内容
/// 2. **后填充定义**：通过 `set` 方法设置具体的递归结构
pub struct FixPointInner {
    inner: OnceLock<Type>,
}

impl GCTraceable<FixPointInner> for FixPointInner {
    fn collect(&self, queue: &mut std::collections::VecDeque<GCArcWeak<FixPointInner>>) {
        if let Some(t) = self.inner.get() {
            t.collect(queue);
        }
    }
}

impl FixPointInner {
    /// 安全的类型遍历，带循环检测
    ///
    /// 使用 [`CycleDetector`] 防止在遍历递归类型时陷入无限循环。
    #[inline(always)]
    fn map<F, R>(&self, path: &mut FastCycleDetector<*const ()>, f: F) -> Result<R, TypeError>
    where
        F: FnOnce(&mut FastCycleDetector<*const ()>, &Type) -> R,
    {
        match self.inner.get() {
            Some(t) => path
                .with_guard(t as *const _ as *const (), |path| t.map(path, f))
                .ok_or(TypeError::InfiniteRecursion)?,
            None => Err(TypeError::UnresolvableType),
        }
    }

    /// 创建未初始化的不动点占位符
    ///
    /// 这是递归类型定义的第一步：创建一个"洞"，稍后填充。
    fn new_placeholder() -> Self {
        FixPointInner {
            inner: OnceLock::new(),
        }
    }

    /// 获取已初始化的类型内容
    ///
    /// 如果类型尚未通过 `set` 方法初始化，返回 `None`。
    fn get(&self) -> Option<&Type> {
        self.inner.get()
    }
}


#[derive(Clone)]
pub struct FixPoint {
    reference: GCArcWeak<FixPointInner>,
}

impl GCTraceable<FixPointInner> for FixPoint {
    fn collect(&self, queue: &mut std::collections::VecDeque<GCArcWeak<FixPointInner>>) {
        queue.push_back(self.reference.clone());
    }
}

impl Rootable for FixPoint {
    fn upgrade(&self, collected: &mut smallvec::SmallVec<[GCArc<FixPointInner>; 8]>) {
        if let Some(inner) = self.reference.upgrade() {
            collected.push(inner);
        }
    }
}

impl FixPoint {
    pub fn map<F, R>(&self, path: &mut FastCycleDetector<*const ()>, f: F) -> Result<R, TypeError>
    where
        F: FnOnce(&mut FastCycleDetector<*const ()>, &Type) -> R,
    {
        self.reference
            .upgrade()
            .ok_or(TypeError::UnresolvableType)
            .and_then(|inner| inner.as_ref().map(path, f))
    }
}

impl FixPoint {
    /// 创建递归类型占位符
    ///
    /// ## 返回值
    ///
    /// 返回一个 [`StabilizedType`]，包含：
    /// - 未初始化的 `FixPoint`
    /// - 对应的强引用以保证 GC 安全

    pub fn new_placeholder(gc: &mut GC<FixPointInner>) -> StabilizedType {
        let pointer = gc.create(FixPointInner::new_placeholder());
        let fix_point = FixPoint {
            reference: pointer.as_weak(),
        };
        StabilizedType::new_with_ref(Type::FixPoint(fix_point), GCArcStorage::Single(pointer))
    }

    /// 设置递归类型的具体定义
    ///
    /// ## 参数
    /// - `t`: 递归类型的展开形式，可以引用自身
    ///
    /// ## 错误
    /// - `RedeclaredType`: 类型已经被设置过
    /// - `UnresolvableType`: 不动点引用已失效
    pub fn set<V: AsTypeRef>(&self, t: V) -> Result<(), TypeError> {
        if let Some(inner) = self.reference.upgrade() {
            inner
                .as_ref()
                .inner
                .set(t.as_type_ref().clone())
                .map_err(|_| TypeError::RedeclaredType) // already initialized
        } else {
            Err(TypeError::UnresolvableType) // reference is dead
        }
    }
}

impl CoinductiveType<Type, StabilizedType> for FixPoint {
    fn dispatch(self) -> Type {
        Type::FixPoint(self)
    }

    /// 递归类型的子类型检查
    ///
    /// ## 算法原理
    ///
    /// 递归类型的子类型检查必须处理**无限展开**的问题。
    /// 我们使用**假设集合** (assumption set) 来记录"正在检查的关系"，
    /// 当遇到重复的检查时，假设关系成立（**协归纳假设**）。
    ///
    /// ## 协归纳子类型规则
    ///
    /// ```text
    /// Γ, μX.S <: μY.T ⊢ S[μX.S/X] <: T[μY.T/Y]
    /// ────────────────────────────────────────────
    ///              μX.S <: μY.T
    /// ```
    ///
    /// 即：在假设 μX.S <: μY.T 的前提下，检查展开后的类型关系。
    fn is(
        &self,
        other: &Type,
        assumptions: &mut smallvec::SmallVec<[(TaggedPtr<()>, TaggedPtr<()>); 8]>,
        closure_env: (&ClosureEnv, &ClosureEnv),
        pattern_env: &mut Collector<(usize, Type)>,
        pattern_mode: bool,
    ) -> Result<Option<()>, TypeError> {
        pattern_env.collect(|pattern_env| match other {
            Type::Bound(TypeBound::Top) => Ok(Some(())), // 快速路径
            Type::Pattern(v) => v.has(self, assumptions, closure_env, pattern_env, pattern_mode),
            Type::Variable(v) => v.has(self, assumptions, closure_env, pattern_env, pattern_mode),
            _ => match self.reference.upgrade() {
                Some(inner) => {
                    let inner = inner.as_ref().get().ok_or(TypeError::UnresolvableType)?;
                    let self_ptr = inner.tagged_ptr();
                    let other_ptr = other.tagged_ptr();
                    if assumptions.contains(&(self_ptr.clone(), other_ptr.clone())) {
                        return Ok(Some(())); // already assumed
                    }
                    assumptions.push((self_ptr, other_ptr));
                    let result =
                        inner.is(other, assumptions, closure_env, pattern_env, pattern_mode);
                    assumptions.pop();
                    result
                }
                None => Err(TypeError::UnresolvableType), // reference is dead
            },
        })
    }

    /// 递归类型的应用
    ///
    /// `(μX.T)[V] = T[μX.T/X][V]`
    ///
    /// 即：将递归类型应用到输入上，等价于将展开的类型应用到输入上。
    fn reduce(
        &self,
        v: &ClosureEnv,
        p: &ParamEnv,
        rec_assumptions: &mut smallvec::SmallVec<[(TaggedPtr<()>, Type, bool); 8]>,
        gc: &mut GC<FixPointInner>,
    ) -> Result<StabilizedType, TypeError> {
        match self.reference.upgrade() {
            Some(inner) => {
                let inner_type = inner.as_ref().get().ok_or(TypeError::UnresolvableType)?;
                for r in rec_assumptions.iter_mut().rev() {
                    if r.0 == inner_type.tagged_ptr() {
                        //已经假设递归的归约结果，直接返回
                        r.2 = true; // mark as used
                        return Ok(r.1.clone().stabilize());
                    }
                }
                let temp_fixpoint = Self::new_placeholder(gc);
                // 假设递归类型的归约结果为 temp_fixpoint
                rec_assumptions.push((
                    inner_type.tagged_ptr(),
                    temp_fixpoint.weak().clone(),
                    false,
                ));
                let result = inner_type.reduce(v, p, rec_assumptions, gc);
                let (_, _, used) = rec_assumptions.pop().unwrap();
                if used {
                    // 递归类型在展开中被使用，返回新的递归类型
                    as_type!(temp_fixpoint.weak(), Type::FixPoint).set(result?)?;
                    Ok(temp_fixpoint)
                } else {
                    // 递归类型未被使用，直接返回展开结果
                    result
                }
            }
            None => Err(TypeError::UnresolvableType), // reference is dead
        }
    }

    fn apply(
        &self,
        v: &Type,
        context: &ClosureEnv,
        p: &ParamEnv,
        rec_assumptions: &mut smallvec::SmallVec<[(TaggedPtr<()>, Type, bool); 8]>,
        gc: &mut GC<FixPointInner>,
    ) -> Result<StabilizedType, TypeError> {
        match self.reference.upgrade() {
            Some(inner) => inner
                .as_ref()
                .get()
                .ok_or(TypeError::UnresolvableType)
                .and_then(|t| t.apply(v, context, p, rec_assumptions, gc)),
            None => Err(TypeError::UnresolvableType), // reference is dead
        }
    }
}

impl CoinductiveTypeWithAny<Type, StabilizedType> for FixPoint {
    fn has<V: CoinductiveType<Type, StabilizedType> + Clone>(
        &self,
        other: &V,
        assumptions: &mut smallvec::SmallVec<[(TaggedPtr<()>, TaggedPtr<()>); 8]>,
        closure_env: (&ClosureEnv, &ClosureEnv),
        pattern_env: &mut Collector<(usize, Type)>,
        pattern_mode: bool,
    ) -> Result<Option<()>, TypeError> {
        pattern_env.collect(|pattern_env| match self.reference.upgrade() {
            Some(inner) => other.is(
                inner.as_ref().get().ok_or(TypeError::UnresolvableType)?,
                assumptions,
                closure_env,
                pattern_env,
                pattern_mode,
            ),
            None => Err(TypeError::UnresolvableType), // reference is dead
        })
    }
}

impl Representable for FixPoint {
    /// 递归类型的字符串表示
    ///
    /// 使用数学记号 `μ.地址 内容` 表示不动点类型，其中：
    /// - `μ` 表示不动点算子
    /// - `地址` 是类型对象的内存地址（用于区分不同的递归类型）
    /// - `内容` 是类型的展开形式（如果没有循环）
    ///
    /// 对于循环引用，只显示地址以避免无限递归打印。
    fn represent(&self, path: &mut FastCycleDetector<*const ()>) -> String {
        match self.reference.upgrade() {
            Some(inner) => match inner.as_ref().get() {
                Some(t) => {
                    match path.with_guard(t as *const _ as *const (), |path| t.represent(path)) {
                        Some(s) => format!("μ.{:?} {}", t as *const _ as *const (), s),
                        None => format!("{:?}", t as *const _ as *const ()),
                    }
                }
                None => "!UninitializedFixPoint".to_string(), // 未初始化
            },
            None => "!InvalidFixPoint".to_string(), // reference is dead
        }
    }
}
