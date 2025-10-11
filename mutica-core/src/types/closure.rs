use std::{ops::Deref, sync::Arc};

use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        AsTypeRef, CoinductiveType, CoinductiveTypeWithAny, InvokeContext, ReductionContext,
        Representable, Rootable, StabilizedType, Type, TypeCheckContext, TypeError,
        fixpoint::FixPointInner, generalize::Generalize, type_bound::TypeBound,
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
        ClosureEnv(v.into_iter().map(|t| t.into_type()).collect())
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
        self.pattern.collect(queue);
        self.expr.collect(queue);
        if let Some(fb) = &self.fail_branch {
            fb.collect(queue);
        }
    }
}

impl Rootable for ClosureInner {
    fn upgrade(&self, collected: &mut smallvec::SmallVec<[arc_gc::arc::GCArc<FixPointInner>; 8]>) {
        self.env.upgrade(collected);
        self.pattern.upgrade(collected);
        self.expr.upgrade(collected);
        if let Some(fb) = &self.fail_branch {
            fb.upgrade(collected);
        }
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

    fn is(&self, other: &Type, ctx: &mut TypeCheckContext) -> Result<Option<()>, TypeError> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.assumptions,
                ctx.closure_env,
                pattern_env,
                ctx.pattern_mode,
            );
            match other {
                Type::Closure(v) => {
                    // 我们不考虑比较时捕获对象是Variable的情况,因为自由变量不应当存在被检查的闭包的环境中
                    // 由于闭包的模式不应当被泄漏,对闭包的解构是不适用的

                    // 创建用于模式比较的上下文
                    let mut pattern_ctx =
                        TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, false);

                    let pattern_match = v
                        .inner
                        .pattern
                        .is(&self.inner.pattern, &mut pattern_ctx)?
                        .is_some();

                    if !pattern_match {
                        return Ok(None);
                    }

                    // 创建用于表达式比较的上下文(不同的闭包环境)
                    let mut expr_ctx = TypeCheckContext::new(
                        ctx.assumptions,
                        (&self.inner.env, &v.inner.env),
                        pattern_env,
                        false,
                    );

                    let expr_match = self.inner.expr.is(&v.inner.expr, &mut expr_ctx)?.is_some();

                    if !expr_match {
                        return Ok(None);
                    }

                    let fail_branch_match = match (&self.inner.fail_branch, &v.inner.fail_branch) {
                        (Some(a), Some(b)) => {
                            let mut fb_ctx = TypeCheckContext::new(
                                ctx.assumptions,
                                ctx.closure_env,
                                pattern_env,
                                false,
                            );
                            a.is(b, &mut fb_ctx)?.is_some()
                        }
                        (None, None) => true,
                        _ => false,
                    };

                    if !fail_branch_match {
                        return Ok(None);
                    }

                    Ok(Some(()))
                }
                Type::Bound(TypeBound::Top) => Ok(Some(())), // 快速路径
                Type::Generalize(v) => v.has(self, &mut inner_ctx),
                Type::Specialize(v) => v.has(self, &mut inner_ctx),
                Type::FixPoint(v) => v.has(self, &mut inner_ctx),
                Type::Pattern(v) => v.has(self, &mut inner_ctx),
                Type::Variable(v) => v.has(self, &mut inner_ctx),
                _ => Ok(None),
            }
        })
    }

    fn reduce(&self, ctx: &mut ReductionContext) -> Result<StabilizedType, TypeError> {
        // 先把闭包环境中的类型都化简
        let env = self
            .inner
            .env
            .0
            .iter()
            .map(|ty| ty.reduce(ctx))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Self::new(
            self.inner.pattern_param_size,
            self.inner.pattern.reduce(ctx)?,
            &self.inner.expr,
            match &self.inner.fail_branch {
                Some(fb) => Some(fb.reduce(ctx)?),
                None => None,
            },
            ClosureEnv::new(env), // 不能拆开成中间变量否则会导致临时变量被drop后fixpoint失效（极其罕见）
        ))
    }

    fn invoke(&self, ctx: &mut InvokeContext) -> Result<StabilizedType, TypeError> {
        let mut matched_pattern = Collector::new();

        // 创建用于模式匹配的类型检查上下文
        let mut assumptions_temp = smallvec::smallvec![];
        let mut pattern_check_ctx = TypeCheckContext::new(
            &mut assumptions_temp,
            (ctx.closure_env, ctx.closure_env),
            &mut matched_pattern,
            true,
        );

        if ctx
            .arg
            .is(&self.inner.pattern, &mut pattern_check_ctx)?
            .is_some()
        {
            // 一旦模式匹配成功,就可以用 matched_pattern 来替换 expr 中的模式变量
            let param_env = ParamEnv::from_collector(matched_pattern);
            let mut reduce_ctx = ReductionContext::new(
                self.inner.env(),
                &param_env,
                ctx.continuation,
                ctx.rec_assumptions,
                ctx.gc,
            );
            self.inner.expr.reduce(&mut reduce_ctx)
        } else {
            if self.inner.fail_branch.is_none() {
                return Err(TypeError::AssertFailed(
                    (
                        ctx.arg.clone().stabilize(),
                        self.inner.pattern.clone().stabilize(),
                    )
                        .into(),
                ));
            }
            // 模式匹配失败,返回 fail_branch
            let mut reduce_ctx = ReductionContext::new(
                ctx.closure_env,
                ctx.param_env,
                ctx.continuation,
                ctx.rec_assumptions,
                ctx.gc,
            );
            self.inner
                .fail_branch
                .as_ref()
                .unwrap()
                .reduce(&mut reduce_ctx)
        }
    }
}

impl Representable for Closure {
    fn represent(&self, path: &mut FastCycleDetector<*const ()>) -> String {
        let mut repr = if self.inner.env.0.is_empty() {
            format!("<{}>", self.inner.pattern.represent(path))
        } else {
            format!(
                "<{}> carry {}",
                self.inner.pattern.represent(path),
                self.inner.env.represent(path)
            )
        };
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
                pattern: pattern.into_type(),
                pattern_param_size,
                expr: expr.into_type(),
                fail_branch: fail_branch.map(|fb| fb.into_type()),
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
