use std::{marker::PhantomData, ops::Deref, sync::Arc};

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, Type, TypeCheckContext, TypeError, TypeRef,
        type_bound::TypeBound,
    },
    util::{
        collector::Collector, cycle_detector::FastCycleDetector,
        three_valued_logic::ThreeValuedLogic,
    },
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

    pub fn all_nf(&self) -> ThreeValuedLogic {
        let mut result = ThreeValuedLogic::True;
        for ty in self.0.iter() {
            result &= ty.is_normal_form();
        }
        result
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
        let empty_closure_env = ClosureEnv::<U, V>::new(Vec::<U>::new());
        if types.is_empty() {
            // 我们不承认“空洞的真理”，因为“空洞的真理”会导致空匹配无法被严格处理，如果仅仅只是处理成 Bottom 那么会导致类型黑洞引发错误传播
            // 这在构造主义逻辑中是不可接受的
            return Ok(false);
        }
        let base_type = &types[0];
        for ty in types.iter().skip(1) {
            if !ty.equals(&empty_closure_env, &empty_closure_env, base_type)? {
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

pub struct ClosureBranch<U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    pattern: U,
    expr: U,
    _pantom: PhantomData<V>,
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Clone for ClosureBranch<U, V> {
    fn clone(&self) -> Self {
        Self {
            pattern: self.pattern.clone(),
            expr: self.expr.clone(),
            _pantom: PhantomData,
        }
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> GCTraceable<V> for ClosureBranch<U, V> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<V>>) {
        self.pattern.collect(queue);
        self.expr.collect(queue);
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Rootable<V> for ClosureBranch<U, V> {
    fn upgrade(&self, collected: &mut Vec<GCArc<V>>) {
        self.pattern.upgrade(collected);
        self.expr.upgrade(collected);
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> ClosureBranch<U, V> {
    pub fn expr(&self) -> &U {
        &self.expr
    }

    pub fn pattern(&self) -> &U {
        &self.pattern
    }
}

pub struct Closure<T: GcAllocObject<T, Inner = Type<T>>> {
    inner: Arc<(
        Vec<(ClosureBranch<Type<T>, T>, usize)>, // usize 用于记录分支指向的环境索引
        Vec<ClosureEnv<Type<T>, T>>,             // 环境列表
    )>,
    is_nf: ThreeValuedLogic,
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
        for (inner, _) in self.inner.0.iter() {
            inner.collect(queue);
        }
        for env in self.inner.1.iter() {
            env.collect(queue);
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Closure<T> {
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        for (inner, _) in self.inner.0.iter() {
            inner.upgrade(collected);
        }
        for env in self.inner.1.iter() {
            env.upgrade(collected);
        }
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
    fn fulfill(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<Option<()>, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env);
            match other {
                TypeRef::Generalize(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Specialize(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Bound(TypeBound::Top) => Ok(Some(())),
                TypeRef::Closure(v) => {
                    if self.inner.0.len() != v.inner.0.len() {
                        return Ok(None);
                    }

                    for ((self_inner, self_idx), (other_inner, other_idx)) in
                        self.inner.0.iter().zip(v.inner.0.iter())
                    {
                        // 我们不考虑比较时捕获对象是Variable的情况,因为自由变量不应当存在被检查的闭包的环境中
                        // 由于闭包的模式不应当被泄漏,对闭包的解构是不适用的
                        // 因此所有的pattern_env都应当被禁用

                        // 创建用于表达式比较的上下文
                        if *self_idx >= self.inner.1.len() || *other_idx >= v.inner.1.len() {
                            panic!("CRITICAL: Closure branch environment index out of bounds");
                        }
                        let mut pattern_env_disabled = Collector::new_disabled();
                        let mut pattern_ctx = TypeCheckContext::new(
                            ctx.assumptions,
                            (&self.inner.1[*self_idx], &v.inner.1[*other_idx]),
                            &mut pattern_env_disabled,
                        );

                        if self_inner
                            .expr
                            .fulfill(other_inner.expr.as_ref_dispatcher(), &mut pattern_ctx)?
                            .is_none()
                        {
                            return Ok(None);
                        }

                        // 创建用于模式比较的上下文
                        let mut pattern_env_disabled = Collector::new_disabled();
                        let mut pattern_ctx = TypeCheckContext::new(
                            ctx.assumptions,
                            (ctx.closure_env.1, ctx.closure_env.0), // 逆变
                            &mut pattern_env_disabled,
                        );

                        if other_inner
                            .pattern
                            .fulfill(self_inner.pattern.as_ref_dispatcher(), &mut pattern_ctx)?
                            .is_none()
                        {
                            return Ok(None);
                        }
                    }
                    Ok(Some(()))
                }
                _ => Ok(None),
            }
        })
    }

    fn reduce(
        self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        // 化简env
        let reduced_env = self
            .env()
            .iter()
            .map(|env| {
                env.iter()
                    .map(|ty| ty.clone().reduce(ctx))
                    .collect::<Result<Vec<_>, _>>()
                    .map(ClosureEnv::new)
            })
            .collect::<Result<Vec<_>, _>>()?;
        let mut reduced_branches = Vec::with_capacity(self.inner.0.len());

        for (inner, closure_idx) in self.inner.0.iter() {
            let reduced_pattern = inner.pattern.clone().reduce(ctx)?;
            reduced_branches.push((reduced_pattern, inner.expr.clone(), *closure_idx)); // expr 是惰性的,不需要立即化简
        }
        Ok(Closure::new::<Type<T>, Type<T>, Type<T>>(
            reduced_branches,
            reduced_env,
        ))
    }

    fn invoke(
        &self,
        ctx: &mut InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        let empty_closure_env = ClosureEnv::new(Vec::<Type<T>>::new());
        for (inner, closure_idx) in self.inner.0.iter() {
            let mut matched_pattern = Collector::new();

            // 创建用于模式匹配的类型检查上下文
            let mut assumptions_temp = smallvec::smallvec![];
            let mut pattern_check_ctx = TypeCheckContext::new(
                &mut assumptions_temp,
                (ctx.closure_env, &empty_closure_env), // 模式自身不应当访问闭包环境
                &mut matched_pattern,
            );

            if ctx
                .arg
                .fulfill(inner.pattern.as_ref_dispatcher(), &mut pattern_check_ctx)?
                .is_some()
                && let Some(param_env) = ParamEnv::from_collector(matched_pattern)?
            {
                // 模式匹配成功，构造用于表达式求值的上下文
                if *closure_idx >= self.inner.1.len() {
                    panic!("CRITICAL: Closure branch environment index out of bounds");
                }
                let mut reduce_ctx = ReductionContext::new(
                    &self.env()[*closure_idx],
                    &param_env,
                    ctx.rec_assumptions,
                    ctx.gc,
                    ctx.roots,
                );
                return inner.expr.clone().reduce(&mut reduce_ctx);
            }
        }
        Err(TypeError::AssertFailed(
            (self.clone().dispatch(), ctx.arg.clone()).into(),
        ))
    }

    fn is_normal_form(&self) -> ThreeValuedLogic {
        self.is_nf
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Closure<T> {
    fn represent(&self, path: &mut FastCycleDetector<*const ()>) -> String {
        let mut repr = String::from("match");
        if !self.inner.1.is_empty() {
            repr.push_str(" capture<");
            repr.push_str(&self.inner.1.represent(path));
            repr.push_str(">");
        }
        for (inner, closure_idx) in self.inner.0.iter() {
            repr.push_str(&format!(" | c.{} ", closure_idx));
            repr.push_str(&inner.pattern.represent(path));
            repr.push_str(" => ");
            repr.push_str(&inner.expr.represent(path));
        }
        repr.push_str(" | panic");
        repr
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Closure<T> {
    pub fn new<U, V, W>(
        branches: Vec<(U, V, usize)>,
        closure_env: Vec<ClosureEnv<Type<T>, T>>,
    ) -> Type<T>
    where
        U: AsDispatcher<Type<T>, T>,
        V: AsDispatcher<Type<T>, T>,
        W: AsDispatcher<Type<T>, T>,
    {
        let mut is_nf = ThreeValuedLogic::True;
        let inner = Arc::from((
            branches
                .into_iter()
                .map(|(pattern, expr, closure_idx)| {
                    let pattern_ty = pattern.into_dispatcher();
                    let expr_ty = expr.into_dispatcher();
                    is_nf &= pattern_ty.is_normal_form();
                    // expr_ty 是惰性的, 不影响 is_nf
                    (
                        ClosureBranch {
                            pattern: pattern_ty,
                            expr: expr_ty,
                            _pantom: PhantomData,
                        },
                        closure_idx,
                    )
                })
                .collect::<Vec<_>>(),
            {
                for env in closure_env.iter() {
                    is_nf &= env.all_nf();
                }
                closure_env
            },
        ));
        Type::Closure(Closure { inner, is_nf })
    }

    pub fn env(&self) -> &[ClosureEnv<Type<T>, T>] {
        &self.inner.1
    }

    pub fn branches(&self) -> &[(ClosureBranch<Type<T>, T>, usize)] {
        &self.inner.0
    }

    pub fn impls(&self, other: &Self) -> Type<T> {
        let mut new_closure_env = self.env().to_vec();
        new_closure_env.extend_from_slice(other.env());
        let mut new_branches = self.branches().to_vec();
        new_branches.extend_from_slice(other.branches());
        // 修正环境索引
        let offset = self.env().len();
        for (_, closure_idx) in new_branches.iter_mut().skip(self.branches().len()) {
            *closure_idx += offset;
        }
        Closure {
            inner: Arc::new((new_branches, new_closure_env)),
            is_nf: self.is_normal_form() & other.is_normal_form(),
        }
        .into_dispatcher()
    }
}
