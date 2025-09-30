use std::{ops::Deref, sync::Arc};

use arc_gc::{gc::GC, traceable::GCTraceable};

use crate::{
    types::{
        AsTypeRef, CoinductiveType, CoinductiveTypeWithAny, Representable, Rootable,
        StabilizedType, TaggedPtr, Type, TypeError, fixpoint::FixPointInner,
        generalize::Generalize, type_bound::TypeBound,
    },
    util::{collector::Collector, cycle_detector::FastCycleDetector},
};

#[derive(Clone)]
pub struct ClosureEnv(Vec<Type>);

impl Deref for ClosureEnv {
    type Target = Vec<Type>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl GCTraceable<FixPointInner> for ClosureEnv {
    fn collect(
        &self,
        queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<FixPointInner>>,
    ) {
        for v in self.0.iter() {
            v.collect(queue);
        }
    }
}

impl Rootable for ClosureEnv {
    fn upgrade(&self, collected: &mut smallvec::SmallVec<[arc_gc::arc::GCArc<FixPointInner>; 8]>) {
        for v in self.0.iter() {
            v.upgrade(collected);
        }
    }
}

impl Default for ClosureEnv {
    fn default() -> Self {
        ClosureEnv(Vec::new())
    }
}

impl Representable for ClosureEnv {
    fn represent(&self, path: &mut FastCycleDetector<*const ()>) -> String {
        let mut repr = String::from("(");
        for (i, v) in self.0.iter().enumerate().rev() {
            if i != self.0.len() - 1 {
                repr.push_str(", ");
            }
            repr.push_str("λ.");
            repr.push_str(&(-1 - i as isize).to_string());
            repr.push_str(" => ");
            repr.push_str(&v.represent(path));
        }
        repr.push(')');
        repr
    }
}

impl ClosureEnv {
    pub fn new<T: AsTypeRef>(v: impl IntoIterator<Item = T>) -> Self {
        ClosureEnv(v.into_iter().map(|t| t.as_type_ref().clone()).collect())
    }

    pub fn get(&self, index: usize) -> Result<&Type, TypeError> {
        self.0
            .get(index)
            .map(|v| v)
            .ok_or_else(|| TypeError::UnboundVariable(-1 - index as isize))
    }
}

pub struct ParamEnv(Vec<StabilizedType>);
impl Deref for ParamEnv {
    type Target = Vec<StabilizedType>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl ParamEnv {
    pub fn from_collector(collector: Collector<(usize, Type)>) -> Self {
        // 计算出最大的索引
        let max_index = collector
            .items()
            .iter()
            .map(|(index, _)| *index)
            .max()
            .unwrap_or(0);
        let mut vec = vec![smallvec::SmallVec::<[Type; 8]>::new(); max_index + 1];
        for (index, ty) in collector.items().iter() {
            vec[*index].push(ty.clone());
        }
        let vec = vec
            .into_iter()
            .map(|types| Generalize::new(types, &ClosureEnv::default()).unwrap())
            .collect::<Vec<_>>();
        ParamEnv(vec)
    }

    pub fn get(&self, index: usize) -> Result<&StabilizedType, TypeError> {
        self.0
            .get(index)
            .ok_or_else(|| TypeError::UnboundVariable(index as isize))
    }
}

pub struct ClosureInner {
    env: ClosureEnv,
    pattern: Type,
    pattern_param_size: usize,
    expr: Type,
    fail_branch: Option<Type>,
}

impl GCTraceable<FixPointInner> for ClosureInner {
    fn collect(
        &self,
        queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<FixPointInner>>,
    ) {
        self.env.collect(queue);
        self.expr.collect(queue);
    }
}

impl Rootable for ClosureInner {
    fn upgrade(&self, collected: &mut smallvec::SmallVec<[arc_gc::arc::GCArc<FixPointInner>; 8]>) {
        self.env.upgrade(collected);
        self.expr.upgrade(collected);
    }
}

impl ClosureInner {
    pub fn env(&self) -> &ClosureEnv {
        &self.env
    }

    pub fn expr(&self) -> &Type {
        &self.expr
    }
}

#[derive(Clone)]
pub struct Closure {
    inner: Arc<ClosureInner>,
}

impl GCTraceable<FixPointInner> for Closure {
    fn collect(
        &self,
        queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<FixPointInner>>,
    ) {
        self.inner.collect(queue);
    }
}

impl Rootable for Closure {
    fn upgrade(&self, collected: &mut smallvec::SmallVec<[arc_gc::arc::GCArc<FixPointInner>; 8]>) {
        self.inner.upgrade(collected);
    }
}

impl CoinductiveType<Type, StabilizedType> for Closure {
    fn dispatch(self) -> Type {
        Type::Closure(self)
    }

    fn is(
        &self,
        other: &Type,
        assumptions: &mut smallvec::SmallVec<[(TaggedPtr<()>, TaggedPtr<()>); 8]>,
        closure_env: (&ClosureEnv, &ClosureEnv),
        pattern_env: &mut Collector<(usize, Type)>,
        pattern_mode: bool,
    ) -> Result<Option<()>, TypeError> {
        pattern_env.collect(|pattern_env| match other {
            Type::Closure(v) => {
                // 我们不考虑比较时捕获对象是Variable的情况，因为自由变量不应当存在被检查的闭包的环境中
                // 由于闭包的模式不应当被泄漏，对闭包的解构是不适用的
                let pattern_match = v
                    .inner
                    .pattern
                    .is(
                        &self.inner.pattern,
                        assumptions,
                        closure_env,
                        pattern_env,
                        false,
                    )?
                    .is_some();

                if !pattern_match {
                    return Ok(None);
                }

                let expr_match = self
                    .inner
                    .expr
                    .is(
                        &v.inner.expr,
                        assumptions,
                        (&self.inner.env, &v.inner.env),
                        pattern_env,
                        false,
                    )?
                    .is_some();

                if !expr_match {
                    return Ok(None);
                }

                let fail_branch_match = match (&self.inner.fail_branch, &v.inner.fail_branch) {
                    (Some(a), Some(b)) => a
                        .is(b, assumptions, closure_env, pattern_env, pattern_mode)?
                        .is_some(),
                    (None, None) => true,
                    _ => false,
                };

                if !fail_branch_match {
                    return Ok(None);
                }

                Ok(Some(()))
            }
            Type::Bound(TypeBound::Top) => Ok(Some(())), // 快速路径
            Type::Generalize(v) => v.has(self, assumptions, closure_env, pattern_env, pattern_mode),
            Type::Specialize(v) => v.has(self, assumptions, closure_env, pattern_env, pattern_mode),
            Type::FixPoint(v) => v.has(self, assumptions, closure_env, pattern_env, pattern_mode),
            Type::Pattern(v) => v.has(self, assumptions, closure_env, pattern_env, pattern_mode),
            Type::Variable(v) => v.has(self, assumptions, closure_env, pattern_env, pattern_mode),
            _ => Ok(None),
        })
    }

    fn reduce(
        &self,
        v: &ClosureEnv,
        p: &ParamEnv,
        rec_assumptions: &mut smallvec::SmallVec<[(TaggedPtr<()>, Type, bool); 8]>,
        gc: &mut GC<FixPointInner>,
    ) -> Result<StabilizedType, TypeError> {
        // 先把闭包环境中的类型都化简
        let env = self
            .inner
            .env
            .0
            .iter()
            .map(|ty| ty.reduce(v, p, rec_assumptions, gc))
            .collect::<Result<Vec<_>, _>>()?;
        let new_env = ClosureEnv::new(env);
        Ok(Self {
            inner: Arc::new(ClosureInner {
                env: new_env,
                pattern: self.inner.pattern.clone(),
                pattern_param_size: self.inner.pattern_param_size,
                expr: self.inner.expr.clone(),
                fail_branch: self.inner.fail_branch.clone(),
            }),
        }
        .dispatch()
        .stabilize())
    }

    fn apply(
        &self,
        v: &Type,
        context: &ClosureEnv,
        p: &ParamEnv,
        rec_assumptions: &mut smallvec::SmallVec<[(TaggedPtr<()>, Type, bool); 8]>,
        gc: &mut GC<FixPointInner>,
    ) -> Result<StabilizedType, TypeError> {
        let mut matched_pattern = Collector::new();
        if v.is(
            &self.inner.pattern,
            &mut smallvec::smallvec![],
            (context, context),
            &mut matched_pattern,
            true,
        )?
        .is_some()
        {
            // 一旦模式匹配成功，就可以用 matched_pattern 来替换 expr 中的模式变量
            self.inner.expr.reduce(
                self.inner.env(),
                &ParamEnv::from_collector(matched_pattern),
                rec_assumptions,
                gc,
            )
        } else {
            if self.inner.fail_branch.is_none() {
                return Err(TypeError::AssertFailed(
                    (
                        v.clone().stabilize(),
                        self.inner.pattern.clone().stabilize(),
                    )
                        .into(),
                ));
            }
            // 模式匹配失败，返回 fail_branch
            self.inner
                .fail_branch
                .as_ref()
                .unwrap()
                .reduce(context, p, rec_assumptions, gc)
        }
    }
}

impl Representable for Closure {
    fn represent(&self, path: &mut FastCycleDetector<*const ()>) -> String {
        let mut repr = format!(
            "<{}> with {}",
            self.inner.pattern.represent(path),
            self.inner.env.represent(path)
        );
        repr.push_str(" -> ");
        repr.push_str(&self.inner.expr.represent(path));
        if let Some(fail_branch) = &self.inner.fail_branch {
            repr.push_str(" \\ ");
            repr.push_str(&fail_branch.represent(path));
        }
        repr
    }
}

impl Closure {
    pub fn new<U, V, W>(
        pattern_param_size: usize,
        pattern: U,
        expr: V,
        fail_branch: Option<W>,
        env: ClosureEnv,
    ) -> StabilizedType
    where
        U: AsTypeRef,
        V: AsTypeRef,
        W: AsTypeRef,
    {
        Self {
            inner: Arc::new(ClosureInner {
                env,
                pattern: pattern.as_type_ref().clone(),
                pattern_param_size,
                expr: expr.as_type_ref().clone(),
                fail_branch: fail_branch.map(|fb| fb.as_type_ref().clone()),
            }),
        }
        .dispatch()
        .stabilize()
    }

    pub fn env(&self) -> &ClosureEnv {
        &self.inner.env
    }

    pub fn expr(&self) -> &Type {
        &self.inner.expr
    }
}
