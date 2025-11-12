use std::{
    marker::PhantomData,
    ops::Deref,
    sync::{Arc, RwLock},
};

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::{
    test_true,
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, TaggedPtr, Type, TypeCheckContext, TypeError,
        TypeRef, anyof::AnyOf,
    },
    util::{
        arc_opt::ArcOpt, collector::Collector, cycle_detector::FastCycleDetector,
        source_info::SourceLocation, three_valued_logic::ThreeValuedLogic,
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
    fn represent(
        &self,
        path: &mut FastCycleDetector<TaggedPtr<()>>,
        depth: usize,
        max_depth: usize,
    ) -> String {
        let mut repr = String::from("(");
        for (i, v) in self.0.iter().enumerate().rev() {
            if i != self.0.len() - 1 {
                repr.push_str(", ");
            }
            repr.push_str("λ.");
            repr.push_str(&(-1 - i as isize).to_string());
            repr.push_str(" => ");
            repr.push_str(&v.represent(path, depth, max_depth));
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

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> IntoIterator for ClosureEnv<U, V> {
    type Item = U;
    type IntoIter = std::vec::IntoIter<U>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
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
        collector: &mut Collector<(usize, U)>,
        pattern_count: usize,
    ) -> Result<Option<Self>, TypeError<U, V>> {
        if collector.is_empty() {
            return Ok(Some(ParamEnv(Vec::new(), PhantomData)));
        }
        let mut vec = vec![smallvec::SmallVec::<[U; 8]>::new(); pattern_count];
        for (index, ty) in collector.take_items().unwrap().into_iter() {
            if index >= pattern_count {
                return Err(TypeError::UndefinedPatternVariable(index as isize));
            }
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
        let mut assumptions = smallvec::smallvec![];
        let mut pattern_env_disabled = Collector::new_disabled();
        let mut ctx = TypeCheckContext::new(
            &mut assumptions,
            (&empty_closure_env, &empty_closure_env),
            &mut pattern_env_disabled,
            false,
        );
        for ty in types.iter().skip(1) {
            let ThreeValuedLogic::True = ty.equals(base_type.as_ref_dispatcher(), &mut ctx)? else {
                return Ok(false);
            };
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
    #[allow(clippy::type_complexity)]
    inner: ArcOpt<(
        Vec<(ClosureBranch<Type<T>, T>, usize, usize)>, // 第一个 usize 用于记录分支指向的环境索引， 第二个 usize 用于记录分支的模式共有多少个待匹配变量
        Vec<ClosureEnv<Type<T>, T>>,                    // 环境列表
        RwLock<ThreeValuedLogic>,
        Option<Arc<SourceLocation>>,
    )>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Closure<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Closure<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        let (branches, env, _, _) = self.inner.as_ref();
        for (inner, _, _) in branches.iter() {
            inner.collect(queue);
        }
        for e in env.iter() {
            e.collect(queue);
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Closure<T> {
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        let (branches, env, _, _) = self.inner.as_ref();
        for (inner, _, _) in branches.iter() {
            inner.upgrade(collected);
        }
        for e in env.iter() {
            e.upgrade(collected);
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
    fn check(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.rhs);
            match other {
                TypeRef::Any(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::All(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::EqOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::SubOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Bound(v)
                    if matches!(&v.kind, crate::types::type_bound::TypeBoundKind::Top) =>
                {
                    Ok(ThreeValuedLogic::True)
                }
                TypeRef::Closure(v) => {
                    let (self_branches, self_env, _, _) = self.inner.as_ref();
                    let (v_branches, v_env, _, _) = v.inner.as_ref();

                    if self_branches.len() != v_branches.len() {
                        return Ok(ThreeValuedLogic::False);
                    }

                    let mut all = ThreeValuedLogic::True;

                    for ((self_inner, self_idx, _), (other_inner, other_idx, _)) in
                        self_branches.iter().zip(v_branches.iter())
                    {
                        // 我们不考虑比较时捕获对象是Variable的情况,因为自由变量不应当存在被检查的闭包的环境中
                        // 由于闭包的模式不应当被泄漏,对闭包的解构是不适用的
                        // 因此所有的pattern_env都应当被禁用

                        // 创建用于表达式比较的上下文
                        if *self_idx >= self_env.len() || *other_idx >= v_env.len() {
                            panic!("CRITICAL: Closure branch environment index out of bounds");
                        }
                        let mut pattern_env_disabled = Collector::new_disabled();
                        let mut pattern_ctx = TypeCheckContext::new(
                            ctx.assumptions,
                            (&self_env[*self_idx], &v_env[*other_idx]),
                            &mut pattern_env_disabled,
                            ctx.rhs,
                        );

                        all &= test_true!(
                            self_inner
                                .expr
                                .check(other_inner.expr.as_ref_dispatcher(), &mut pattern_ctx)?
                        );

                        // 创建用于模式比较的上下文
                        let mut pattern_env_disabled = Collector::new_disabled();
                        let mut pattern_ctx = TypeCheckContext::new(
                            ctx.assumptions,
                            (ctx.closure_env.1, ctx.closure_env.0), // 逆变
                            &mut pattern_env_disabled,
                            !ctx.rhs,
                        );
                        all &= test_true!(
                            other_inner
                                .pattern
                                .check(self_inner.pattern.as_ref_dispatcher(), &mut pattern_ctx)?
                        )
                    }
                    Ok(all)
                }
                _ => Ok(ThreeValuedLogic::False),
            }
        })
    }

    fn subof(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut super::TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.rhs);
            match other {
                TypeRef::Any(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::All(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Bound(v)
                    if matches!(&v.kind, crate::types::type_bound::TypeBoundKind::Top) =>
                {
                    Ok(ThreeValuedLogic::True)
                }
                TypeRef::Closure(v) => {
                    let (self_branches, self_env, _, _) = self.inner.as_ref();
                    let (v_branches, v_env, _, _) = v.inner.as_ref();

                    if self_branches.len() != v_branches.len() {
                        return Ok(ThreeValuedLogic::False);
                    }

                    let mut all = ThreeValuedLogic::True;

                    for ((self_inner, self_idx, _), (other_inner, other_idx, _)) in
                        self_branches.iter().zip(v_branches.iter())
                    {
                        // 我们不考虑比较时捕获对象是Variable的情况,因为自由变量不应当存在被检查的闭包的环境中
                        // 由于闭包的模式不应当被泄漏,对闭包的解构是不适用的
                        // 因此所有的pattern_env都应当被禁用

                        // 创建用于表达式比较的上下文
                        if *self_idx >= self_env.len() || *other_idx >= v_env.len() {
                            panic!("CRITICAL: Closure branch environment index out of bounds");
                        }
                        let mut pattern_env_disabled = Collector::new_disabled();
                        let mut pattern_ctx = TypeCheckContext::new(
                            ctx.assumptions,
                            (&self_env[*self_idx], &v_env[*other_idx]),
                            &mut pattern_env_disabled,
                            ctx.rhs,
                        );

                        all &= test_true!(
                            self_inner
                                .expr
                                .subof(other_inner.expr.as_ref_dispatcher(), &mut pattern_ctx)?
                        );

                        // 创建用于模式比较的上下文
                        let mut pattern_env_disabled = Collector::new_disabled();
                        let mut pattern_ctx = TypeCheckContext::new(
                            ctx.assumptions,
                            (ctx.closure_env.1, ctx.closure_env.0), // 逆变
                            &mut pattern_env_disabled,
                            !ctx.rhs,
                        );
                        all &= test_true!(
                            other_inner
                                .pattern
                                .subof(self_inner.pattern.as_ref_dispatcher(), &mut pattern_ctx)?
                        )
                    }
                    Ok(all)
                }
                _ => Ok(ThreeValuedLogic::False),
            }
        })
    }

    fn reduce(
        mut self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        match self.inner.modify(|(branches, env, is_nf, source_info)| {
            let mut is_env_nf = env
                .iter()
                .map(|e| e.all_nf())
                .fold(ThreeValuedLogic::True, |a, b| a & b);
            let mut is_branches_nf = branches
                .iter()
                .map(|(inner, _, _)| inner.pattern.is_normal_form())
                .fold(ThreeValuedLogic::True, |a, b| a & b);
            // 化简env
            let reduced_env = if let ThreeValuedLogic::True = is_env_nf {
                env
            } else {
                let result = env
                    .into_iter()
                    .map(|e: ClosureEnv<Type<T>, T>| {
                        e.into_iter()
                            .map(|ty| ty.reduce(ctx))
                            .collect::<Result<Vec<_>, _>>()
                            .map(ClosureEnv::new)
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                is_env_nf = ThreeValuedLogic::True;
                for e in result.iter() {
                    is_env_nf &= e.all_nf();
                }
                result
            };

            let reduced_branches = if let ThreeValuedLogic::True = is_branches_nf {
                branches
            } else {
                let result = branches
                    .into_iter()
                    .map(|(inner, closure_idx, pattern_count)| {
                        inner.pattern.reduce(ctx).map(|reduced_pattern| {
                            (
                                ClosureBranch {
                                    pattern: reduced_pattern,
                                    expr: inner.expr,
                                    _pantom: PhantomData,
                                },
                                closure_idx,
                                pattern_count,
                            )
                        })
                    })
                    .collect::<Result<Vec<_>, TypeError<Type<T>, T>>>()?;
                is_branches_nf = ThreeValuedLogic::True;
                for (inner, _, _) in result.iter() {
                    is_branches_nf &= inner.pattern.is_normal_form();
                }
                result
            };

            // 重新计算 is_nf
            if let Ok(mut nf_lock) = is_nf.write() {
                *nf_lock = is_env_nf & is_branches_nf;
            }

            Ok((reduced_branches, reduced_env, is_nf, source_info))
        })? {
            Some(()) => Ok(self.dispatch()),
            None => {
                let (branches, env, _, source_info) = self.inner.as_ref();
                // 化简env
                let reduced_env = env
                    .iter()
                    .map(|e| {
                        e.iter()
                            .map(|ty| ty.clone().reduce(ctx))
                            .collect::<Result<Vec<_>, _>>()
                            .map(ClosureEnv::new)
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                let mut reduced_branches = Vec::with_capacity(branches.len());
                for (inner, closure_idx, pattern_count) in branches.iter() {
                    let reduced_pattern = inner.pattern.clone().reduce(ctx)?;
                    reduced_branches.push((
                        reduced_pattern,
                        inner.expr.clone(),
                        *closure_idx,
                        *pattern_count,
                    ));
                }
                Ok(Closure::new::<Type<T>, Type<T>>(
                    reduced_branches,
                    reduced_env,
                    source_info.clone(),
                ))
            }
        }
    }

    fn invoke(self, ctx: InvokeContext<Type<T>, T>) -> Result<Type<T>, TypeError<Type<T>, T>> {
        let (branches, env, _, _) = self.inner.as_ref();
        let empty_closure_env = ClosureEnv::new(Vec::<Type<T>>::new());
        let mut matched_pattern = Collector::new();
        let mut assumptions_temp = smallvec::smallvec![];
        for (inner, closure_idx, pattern_count) in branches.iter() {
            matched_pattern.clear();
            assumptions_temp.clear();
            // 创建用于模式匹配的类型检查上下文
            let mut pattern_check_ctx = TypeCheckContext::new(
                &mut assumptions_temp,
                (ctx.closure_env, &empty_closure_env), // 模式自身不应当访问闭包环境
                &mut matched_pattern,
                false,
            );

            if let ThreeValuedLogic::True = ctx
                .arg
                .check(inner.pattern.as_ref_dispatcher(), &mut pattern_check_ctx)?
                && let Some(param_env) =
                    ParamEnv::from_collector(&mut matched_pattern, *pattern_count)?
            {
                // 模式匹配成功，构造用于表达式求值的上下文
                if *closure_idx >= env.len() {
                    panic!("CRITICAL: Closure branch environment index out of bounds");
                }
                let mut reduce_ctx = ReductionContext::new(
                    &env[*closure_idx],
                    &param_env,
                    ctx.rec_assumptions,
                    ctx.gc,
                    ctx.roots,
                );
                return inner.expr.clone().reduce(&mut reduce_ctx);
            }
        }
        let expect_arg = self
            .branches()
            .iter()
            .map(|(b, _, _)| b.pattern())
            .collect::<Vec<_>>();
        Err(TypeError::AssertFailed(
            (
                AnyOf::new(expect_arg, self.source_info().cloned()),
                ctx.arg.clone(),
            )
                .into(),
        ))
    }

    fn is_normal_form(&self) -> ThreeValuedLogic {
        let (_, _, is_nf, _) = self.inner.as_ref();
        match is_nf.read() {
            Ok(v) => *v,
            Err(_) => ThreeValuedLogic::False,
        }
    }

    fn recalculate_normal_form(&self, cycle_detector: &mut FastCycleDetector<TaggedPtr<()>>) {
        let (branches, env, is_nf, _) = self.inner.as_ref();
        for (inner, _, _) in branches.iter() {
            inner.pattern.recalculate_normal_form(cycle_detector);
        }
        for e in env.iter() {
            for ty in e.iter() {
                ty.recalculate_normal_form(cycle_detector);
            }
        }
        let mut new_nf = ThreeValuedLogic::True;
        for (inner, _, _) in branches.iter() {
            new_nf &= inner.pattern.is_normal_form();
            // expr_ty 是惰性的, 不影响 is_nf
        }
        for e in env.iter() {
            new_nf &= e.all_nf();
        }
        if let Ok(mut nf_lock) = is_nf.write() {
            *nf_lock = new_nf;
        }
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        let (_, _, _, source_info) = self.inner.as_ref();
        source_info.as_ref()
    }

    fn report_source_info(&self) -> crate::types::TypeReport {
        let (_, _, _, source_info) = self.inner.as_ref();
        if let Some(loc) = source_info {
            let span = loc.span().clone();
            let filepath = loc.source().filepath().to_string();
            ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                .with_message(format!("Closure type at {}", filepath))
                .with_label(
                    ariadne::Label::new((filepath, span)).with_message("Closure defined here"),
                )
                .finish()
        } else {
            ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                .with_message("Closure type has no source location")
                .with_label(
                    ariadne::Label::new(("<unknown>".to_string(), 0..0))
                        .with_message("Location unknown"),
                )
                .finish()
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Closure<T> {
    fn represent(
        &self,
        path: &mut FastCycleDetector<TaggedPtr<()>>,
        depth: usize,
        max_depth: usize,
    ) -> String {
        if depth > max_depth {
            return "...".to_string();
        }
        let (branches, env, _, _) = self.inner.as_ref();
        let mut repr = String::from("match");
        if !env.is_empty() {
            repr.push_str(" capture ");
            repr.push_str(&env.represent(path, depth + 1, max_depth));
        }
        for (inner, closure_idx, _) in branches.iter() {
            repr.push_str(&format!(" | c.{} ", closure_idx));
            repr.push_str(&inner.pattern.represent(path, depth + 1, max_depth));
            repr.push_str(" => ");
            repr.push_str(&inner.expr.represent(path, depth + 1, max_depth));
        }
        repr.push_str(" | panic");
        repr
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Closure<T> {
    #[allow(clippy::new_ret_no_self)]
    /// 构造闭包类型
    /// `branches` 参数格式: (pattern, expr, closure_env_index, pattern_variable_count)
    /// `closure_env` 参数格式: 捕获环境列表
    pub fn new<U, V>(
        branches: Vec<(U, V, usize, usize)>,
        closure_env: Vec<ClosureEnv<Type<T>, T>>,
        source_info: Option<Arc<SourceLocation>>,
    ) -> Type<T>
    where
        U: AsDispatcher<Type<T>, T>,
        V: AsDispatcher<Type<T>, T>,
    {
        let mut is_nf = ThreeValuedLogic::True;
        let branches_vec = branches
            .into_iter()
            .map(|(pattern, expr, closure_idx, pattern_count)| {
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
                    pattern_count,
                )
            })
            .collect::<Vec<_>>();

        for env in closure_env.iter() {
            is_nf &= env.all_nf();
        }

        Type::Closure(Closure {
            inner: ArcOpt::new((branches_vec, closure_env, RwLock::new(is_nf), source_info)),
        })
    }

    pub fn env(&self) -> &[ClosureEnv<Type<T>, T>] {
        &self.inner.as_ref().1
    }

    #[allow(clippy::type_complexity)]
    pub fn branches(&self) -> &[(ClosureBranch<Type<T>, T>, usize, usize)] {
        &self.inner.as_ref().0
    }

    pub fn impls(self, other: Self, source_info: Option<Arc<SourceLocation>>) -> Type<T> {
        let mut new_closure_env = self.env().to_vec();
        new_closure_env.extend_from_slice(other.env());
        let mut new_branches = self.branches().to_vec();
        new_branches.extend_from_slice(other.branches());
        // 修正环境索引
        let offset = self.env().len();
        for (_, closure_idx, _) in new_branches.iter_mut().skip(self.branches().len()) {
            *closure_idx += offset;
        }
        let is_nf = self.is_normal_form() & other.is_normal_form();
        Closure {
            inner: ArcOpt::new((
                new_branches,
                new_closure_env,
                RwLock::new(is_nf),
                source_info,
            )),
        }
        .into_dispatcher()
    }
}
