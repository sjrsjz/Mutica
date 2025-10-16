use std::{marker::PhantomData, ops::Deref, sync::Arc};

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, Type, TypeCheckContext, TypeError, TypeRef,
        type_bound::TypeBound,
    },
    util::{collector::Collector, cycle_detector::FastCycleDetector},
};

pub struct ClosureEnv<U: CoinductiveType<U, V>, V: GcAllocObject<V>>(Vec<U>, PhantomData<V>);

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Clone for ClosureEnv<U, V> {
    fn clone(&self) -> Self {
        ClosureEnv(self.0.clone(), PhantomData)
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Deref for ClosureEnv<U, V> {
    type Target = Vec<U>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> GCTraceable<V> for ClosureEnv<U, V> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<V>>) {
        for v in self.0.iter() {
            v.collect(queue);
        }
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Rootable<V> for ClosureEnv<U, V> {
    fn upgrade(&self, collected: &mut Vec<GCArc<V>>) {
        for v in self.0.iter() {
            v.upgrade(collected);
        }
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Default for ClosureEnv<U, V> {
    fn default() -> Self {
        ClosureEnv::<U, V>(Vec::new(), PhantomData)
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Representable for ClosureEnv<U, V> {
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

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> ClosureEnv<U, V> {
    pub fn new<T: AsDispatcher<U, V>>(v: impl IntoIterator<Item = T>) -> Self {
        ClosureEnv::<U, V>(
            v.into_iter().map(|t| t.into_dispatcher()).collect(),
            PhantomData,
        )
    }

    pub fn get(&self, index: usize) -> Result<&U, TypeError<U, V>> {
        self.0
            .get(index)
            .map(|v| v)
            .ok_or_else(|| TypeError::UnboundVariable(-1 - index as isize))
    }

    pub fn all_nf(&self) -> bool {
        for ty in self.0.iter() {
            if !ty.is_normal_form() {
                return false;
            }
        }
        true
    }
}

pub struct ParamEnv<U: CoinductiveType<U, V>, V: GcAllocObject<V>>(
    Vec<U>,
    std::marker::PhantomData<V>,
);
impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Deref for ParamEnv<U, V> {
    type Target = Vec<U>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> ParamEnv<U, V> {
    /// 尝试从 Collector 构造 ParamEnv，如果同一索引下的类型不等价则返回 None
    /// 这个构造器拒绝“空洞的真理”，即保证每个索引下至少有一个类型
    pub fn from_collector(
        mut collector: Collector<(usize, U)>,
    ) -> Result<Option<Self>, TypeError<U, V>> {
        if collector.is_empty() {
            return Ok(Some(ParamEnv(Vec::new(), PhantomData)));
        }
        // 计算出最大的索引
        let max_index = collector
            .items()
            .unwrap()
            .iter()
            .map(|(index, _)| *index)
            .max()
            .unwrap();
        let mut vec = vec![smallvec::SmallVec::<[U; 8]>::new(); max_index + 1];
        for (index, ty) in collector.take_items().unwrap().into_iter() {
            vec[index].push(ty);
        }
        let mut stabilized_vec = Vec::with_capacity(vec.len());
        for types in vec.into_iter() {
            if Self::check_equivalent(&types)? {
                stabilized_vec.push(types.into_iter().next().unwrap());
            } else {
                return Ok(None);
            }
        }
        Ok(Some(ParamEnv(stabilized_vec, PhantomData)))
    }

    fn check_equivalent(types: &smallvec::SmallVec<[U; 8]>) -> Result<bool, TypeError<U, V>> {
        if types.is_empty() {
            // 我们不承认“空洞的真理”，因为“空洞的真理”会导致空匹配无法被严格处理，如果仅仅只是处理成 Bottom 那么会导致类型黑洞引发错误传播
            // 这在构造主义逻辑中是不可接受的
            return Ok(false);
        }
        let base_type = &types[0];
        for ty in types.iter().skip(1) {
            if !ty.equivalent(base_type)? {
                return Ok(false);
            }
        }
        Ok(true)
    }

    pub fn get(&self, index: usize) -> Result<&U, TypeError<U, V>> {
        self.0
            .get(index)
            .ok_or_else(|| TypeError::UnboundVariable(index as isize))
    }
}

#[derive(Clone)]
pub struct ClosureInner<U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    env: ClosureEnv<U, V>,
    pattern: U,
    pattern_param_size: usize,
    expr: U,
    fail_branch: Option<U>,
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> GCTraceable<V> for ClosureInner<U, V> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<V>>) {
        self.env.collect(queue);
        self.pattern.collect(queue);
        self.expr.collect(queue);
        if let Some(fb) = &self.fail_branch {
            fb.collect(queue);
        }
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Rootable<V> for ClosureInner<U, V> {
    fn upgrade(&self, collected: &mut Vec<GCArc<V>>) {
        self.env.upgrade(collected);
        self.pattern.upgrade(collected);
        self.expr.upgrade(collected);
        if let Some(fb) = &self.fail_branch {
            fb.upgrade(collected);
        }
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> ClosureInner<U, V> {
    pub fn env(&self) -> &ClosureEnv<U, V> {
        &self.env
    }

    pub fn expr(&self) -> &U {
        &self.expr
    }

    pub fn pattern(&self) -> &U {
        &self.pattern
    }

    pub fn fail_branch(&self) -> Option<&U> {
        self.fail_branch.as_ref()
    }

    pub fn pattern_param_size(&self) -> usize {
        self.pattern_param_size
    }
}

pub struct Closure<T: GcAllocObject<T, Inner = Type<T>>> {
    inner: Arc<ClosureInner<Type<T>, T>>,
    is_nf: bool,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Closure<T> {
    fn clone(&self) -> Self {
        Self {
            inner: Arc::clone(&self.inner),
            is_nf: self.is_nf,
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GcAllocObject<T> for Closure<T> {
    type Inner = Type<T>;
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Closure<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        self.inner.collect(queue);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Closure<T> {
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        self.inner.upgrade(collected);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for Closure<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;

    fn into_dispatcher(self) -> Type<T> {
        Type::Closure(self)
    }

    fn as_ref_dispatcher<'a>(&'a self) -> Self::RefDispatcher<'a> {
        TypeRef::Closure(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for Closure<T> {
    fn is(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<Option<()>, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env);
            match other {
                TypeRef::Closure(v) => {
                    // 我们不考虑比较时捕获对象是Variable的情况,因为自由变量不应当存在被检查的闭包的环境中
                    // 由于闭包的模式不应当被泄漏,对闭包的解构是不适用的
                    // 因此所有的pattern_env都应当被禁用

                    // 创建用于模式比较的上下文
                    let mut pattern_env_disabled = Collector::new_disabled();
                    let mut pattern_ctx = TypeCheckContext::new(
                        ctx.assumptions,
                        (ctx.closure_env.1, ctx.closure_env.0), // 反转，因为模式在类型检查时是逆向的
                        &mut pattern_env_disabled,
                    );

                    let pattern_match = v
                        .inner
                        .pattern
                        .is(self.inner.pattern.as_ref_dispatcher(), &mut pattern_ctx)?
                        .is_some();

                    if !pattern_match {
                        return Ok(None);
                    }

                    // 创建用于表达式比较的上下文(不同的闭包环境)
                    let mut expr_ctx = TypeCheckContext::new(
                        ctx.assumptions,
                        (&self.inner.env, &v.inner.env),
                        &mut pattern_env_disabled,
                    );

                    let expr_match = self
                        .inner
                        .expr
                        .is(v.inner.expr.as_ref_dispatcher(), &mut expr_ctx)?
                        .is_some();

                    if !expr_match {
                        return Ok(None);
                    }

                    let fail_branch_match = match (&self.inner.fail_branch, &v.inner.fail_branch) {
                        (Some(a), Some(b)) => {
                            let mut fb_ctx = TypeCheckContext::new(
                                ctx.assumptions,
                                ctx.closure_env,
                                &mut pattern_env_disabled,
                            );
                            a.is(b.as_ref_dispatcher(), &mut fb_ctx)?.is_some()
                        }
                        (None, None) => true,
                        _ => false,
                    };

                    if !fail_branch_match {
                        return Ok(None);
                    }

                    Ok(Some(()))
                }
                TypeRef::Bound(TypeBound::Top) => Ok(Some(())), // 快速路径
                TypeRef::Generalize(v) => v.has(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Specialize(v) => v.has(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.has(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.has(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.has(self.as_ref_dispatcher(), &mut inner_ctx),
                _ => Ok(None),
            }
        })
    }

    fn reduce(
        self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        // 先把闭包环境中的类型都化简
        let env = self
            .inner
            .env
            .0
            .iter()
            .map(|ty| ty.clone().reduce(ctx))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Self::new(
            self.inner.pattern_param_size,
            self.inner.pattern.clone().reduce(ctx)?,
            self.inner.expr.clone(), // expr 是惰性求值的,不应当在这里化简
            match &self.inner.fail_branch {
                Some(fb) => Some(fb.clone().reduce(ctx)?),
                None => None,
            },
            ClosureEnv::<Type<T>, T>::new(env), // 不能拆开成中间变量否则会导致临时变量被drop后fixpoint失效（极其罕见）
        ))
    }

    fn invoke(
        &self,
        ctx: &mut InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        let mut matched_pattern = Collector::new();

        // 创建用于模式匹配的类型检查上下文
        let mut assumptions_temp = smallvec::smallvec![];
        let mut pattern_check_ctx = TypeCheckContext::new(
            &mut assumptions_temp,
            (ctx.closure_env, ctx.closure_env),
            &mut matched_pattern,
        );

        if ctx
            .arg
            .is(
                self.inner.pattern.as_ref_dispatcher(),
                &mut pattern_check_ctx,
            )?
            .is_some()
            && let Some(param_env) = ParamEnv::from_collector(matched_pattern)?
        {
            // 一旦模式匹配成功,就可以用 matched_pattern 来替换 expr 中的模式变量
            let mut reduce_ctx = ReductionContext::new(
                self.inner.env(),
                &param_env,
                ctx.continuation,
                ctx.rec_assumptions,
                ctx.gc,
                ctx.roots,
            );
            self.inner.expr.clone().reduce(&mut reduce_ctx)
        } else {
            if self.inner.fail_branch.is_none() {
                return Err(TypeError::AssertFailed(
                    (ctx.arg.clone(), self.inner.pattern.clone()).into(),
                ));
            }
            // 模式匹配失败,返回 fail_branch
            let mut reduce_ctx = ReductionContext::new(
                ctx.closure_env,
                ctx.param_env,
                ctx.continuation,
                ctx.rec_assumptions,
                ctx.gc,
                ctx.roots,
            );
            self.inner
                .fail_branch
                .as_ref()
                .unwrap()
                .clone()
                .reduce(&mut reduce_ctx)
        }
    }

    fn is_normal_form(&self) -> bool {
        self.is_nf
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Closure<T> {
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

impl<T: GcAllocObject<T, Inner = Type<T>>> Closure<T> {
    pub fn new<U, V, W>(
        pattern_param_size: usize,
        pattern: U,
        expr: V,
        fail_branch: Option<W>,
        env: ClosureEnv<Type<T>, T>,
    ) -> Type<T>
    where
        U: AsDispatcher<Type<T>, T>,
        V: AsDispatcher<Type<T>, T>,
        W: AsDispatcher<Type<T>, T>,
    {
        let pattern = pattern.into_dispatcher();
        let expr = expr.into_dispatcher();
        let fail_branch = fail_branch.map(|fb| fb.into_dispatcher());

        // 由于闭包的reduce不会去尝试reduce expr,因此is_nf的计算无需考虑expr（它是惰性求值的）
        let all_nf = env.all_nf()
            && pattern.is_normal_form()
            && fail_branch.as_ref().map_or(true, |fb| fb.is_normal_form());

        Self {
            inner: Arc::new(ClosureInner {
                env,
                pattern,
                pattern_param_size,
                expr,
                fail_branch: fail_branch.map(|fb| fb.into_dispatcher()),
            }),
            is_nf: all_nf,
        }
        .dispatch()
    }

    pub fn env(&self) -> &ClosureEnv<Type<T>, T> {
        &self.inner.env
    }

    pub fn expr(&self) -> &Type<T> {
        &self.inner.expr
    }
}
