use core::panic;
use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};
use smallvec::SmallVec;

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, CollectorExt, GcAllocObject,
        GenericBinding, InvokeContext, PatternCollector, ReductionContext, Representable, Rootable,
        TaggedPtr, Type, TypeCheckContext, TypeError, TypeOfContext, TypeRef,
        allof::AllOf,
        anyof::AnyOf,
        constraint::Constraint,
        lambda::Lambda,
        pattern::Pattern,
        unify::{
            ArgumentBinding,
            capture_env::{CaptureEnv, CaptureEnvList, CaptureOrigin},
            collector::Collector,
        },
        variable::Variable,
    },
    util::{
        cycle_detector::FastCycleDetector, source_info::SourceLocation,
        three_valued_logic::ThreeValuedLogic,
    },
};

pub struct ClosureBranch<U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    capture_env: CaptureEnv<U, V>,
    pattern: Constraint<U, V>,
    expr: U,
    rootless: bool,
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Clone for ClosureBranch<U, V> {
    fn clone(&self) -> Self {
        Self {
            capture_env: self.capture_env.clone(),
            pattern: self.pattern.clone(),
            expr: self.expr.clone(),
            rootless: self.rootless,
        }
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> GCTraceable<V> for ClosureBranch<U, V> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<V>>) {
        if self.rootless {
            return;
        }
        self.capture_env.collect(queue);
        self.pattern.collect(queue);
        self.expr.collect(queue);
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Rootable<V> for ClosureBranch<U, V> {
    fn upgrade(&self, collected: &mut Vec<GCArc<V>>) {
        if self.rootless {
            return;
        }
        self.capture_env.upgrade(collected);
        self.pattern.upgrade(collected);
        self.expr.upgrade(collected);
    }

    fn rootless(&self) -> bool {
        self.rootless
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> ClosureBranch<U, V> {
    #[allow(clippy::type_complexity)]
    pub fn capture(
        mut self,
        solved_argument: &[(Arc<str>, ArgumentBinding<U, V>)],
        parent_env: CaptureEnvList<U, V>,
    ) -> Result<Self, TypeError<U, V>> {
        self.capture_env =
            self.capture_env.capture_from(solved_argument, parent_env).map_err(|var_name| {
                TypeError::MissingVariable(var_name.to_string().into_boxed_str())
            })?;
        // 重置 rootless 状态
        self.rootless =
            self.capture_env.rootless() && self.pattern.rootless() && self.expr.rootless();
        Ok(self)
    }

    pub fn new(capture_env: CaptureEnv<U, V>, pattern: Constraint<U, V>, expr: U) -> Self {
        let rootless = capture_env.rootless() && pattern.rootless() && expr.rootless();
        Self { capture_env, pattern, expr, rootless }
    }
}

pub struct Closure<U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    branches: Arc<[ClosureBranch<U, V>]>,
    rootless: bool,
    source_info: Option<Arc<SourceLocation>>,
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Clone for Closure<U, V> {
    fn clone(&self) -> Self {
        Self {
            branches: self.branches.clone(),
            rootless: self.rootless,
            source_info: self.source_info.clone(),
        }
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> GCTraceable<V> for Closure<U, V> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<V>>) {
        if self.rootless {
            return;
        }
        for branch in self.branches.iter() {
            branch.collect(queue);
        }
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Rootable<V> for Closure<U, V> {
    fn upgrade(&self, collected: &mut Vec<GCArc<V>>) {
        if self.rootless {
            return;
        }
        for branch in self.branches.iter() {
            branch.upgrade(collected);
        }
    }

    fn rootless(&self) -> bool {
        self.rootless
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for Closure<Type<T>, T> {
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

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for Closure<Type<T>, T> {
    fn check(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        ctx.pattern_collector.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.coinductive_assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.bound_generic_variables,
            );
            match other {
                TypeRef::Any(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::All(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Constraint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::SubOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Lambda(other) => {
                    // Closure 对 Lambda 的 check：只比较模式，忽略分支的 expr
                    // 规则与 Lambda::subof 类似，但 LHS 使用 closure 的 branch.pattern
                    let lhs_branches = self.branches.as_ref();
                    let rhs_patterns = other.patterns();
                    let flipped = ctx.bound_generic_variables.flip();
                    let mut inner_ctx = TypeCheckContext::new(
                        ctx.coinductive_assumptions,
                        PatternCollector::None, // 交换方向会导致收集器不可用
                        ctx.rhs_env,
                        ctx.lhs_env,
                        &flipped,
                    );
                    let mut i = 0usize;
                    let mut j = 0usize;
                    let mut result = ThreeValuedLogic::True;

                    while i < lhs_branches.len() && j < rhs_patterns.len() {
                        let lhs = &lhs_branches[i].pattern;
                        let rhs = &rhs_patterns[j];
                        match rhs.subof_constraint(
                            lhs,
                            &mut inner_ctx,
                            None::<
                                fn(
                                    &mut TypeCheckContext<Type<T>, T>,
                                )
                                    -> Result<ThreeValuedLogic, TypeError<Type<T>, T>>,
                            >,
                        )? {
                            ThreeValuedLogic::True => {
                                j += 1;
                            }
                            ThreeValuedLogic::False => {
                                i += 1;
                            }
                            ThreeValuedLogic::Unknown => {
                                result &= ThreeValuedLogic::Unknown;
                                i += 1;
                            }
                        }
                    }

                    if j >= rhs_patterns.len() { Ok(result) } else { Ok(ThreeValuedLogic::False) }
                }
                _ => Ok(ThreeValuedLogic::False),
            }
        })
    }

    fn subof(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        ctx.pattern_collector.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.coinductive_assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.bound_generic_variables,
            );
            match other {
                TypeRef::Any(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::All(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Closure(other) => {
                    // println!("LHS: {:?}", self.represent(&mut FastCycleDetector::new(), 0, 3));
                    // println!("RHS: {:?}", other.represent(&mut FastCycleDetector::new(), 0, 3));
                    // Closure 的子类型关系：模式逆变，expr 协变
                    let lhs_branches = self.branches.as_ref();
                    let rhs_branches = other.branches.as_ref();

                    let mut i = 0usize;
                    let mut j = 0usize;
                    let mut result = ThreeValuedLogic::True;
                    let mut collector = inner_ctx.pattern_collector;
                    let mut assumptions = inner_ctx.coinductive_assumptions;
                    let lhs_env = inner_ctx.rhs_env; // 逆变交换方向
                    let rhs_env = inner_ctx.lhs_env;
                    let bound_generic_layers = inner_ctx.bound_generic_variables.flip();

                    while i < lhs_branches.len() && j < rhs_branches.len() {
                        let lhs = &lhs_branches[i];
                        let rhs = &rhs_branches[j];

                        let mut inner_loop_ctx = TypeCheckContext::new(
                            assumptions,
                            collector,
                            lhs_env,
                            rhs_env,
                            &bound_generic_layers,
                        );
                        let expr_check = |ctx: &mut TypeCheckContext<_, _>| {
                            let flipped = ctx.bound_generic_variables.flip();
                            let mut ctx = TypeCheckContext::new(
                                ctx.coinductive_assumptions,
                                PatternCollector::None, // expr 不需要收集模式
                                ctx.rhs_env.attach(&lhs.capture_env, None), // 先把逆变的env交换后再拼接
                                ctx.lhs_env.attach(&rhs.capture_env, None),
                                &flipped,
                            );
                            // println!("Checking Closure expr subof: {} <: {}", lhs.expr.represent(&mut FastCycleDetector::new(), 0, 3), rhs.expr.represent(&mut FastCycleDetector::new(), 0, 3));
                            // println!(" bound_generic_layers: {:?}", ctx.bound_generic_variables);
                            lhs.expr.subof(rhs.expr.as_ref_dispatcher(), &mut ctx)
                        };
                        match rhs.pattern.subof_constraint(
                            &lhs.pattern,
                            &mut inner_loop_ctx,
                            Some(expr_check),
                        )? {
                            ThreeValuedLogic::True => {
                                j += 1;
                            }
                            ThreeValuedLogic::False => {
                                i += 1;
                            }
                            ThreeValuedLogic::Unknown => {
                                result &= ThreeValuedLogic::Unknown;
                                i += 1;
                            }
                        }
                        collector = inner_loop_ctx.pattern_collector;
                        assumptions = inner_loop_ctx.coinductive_assumptions;
                    }

                    if j >= rhs_branches.len() { Ok(result) } else { Ok(ThreeValuedLogic::False) }
                }
                _ => Ok(ThreeValuedLogic::False),
            }
        })
    }

    fn reduce(
        &self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        let mut reduced_branches = SmallVec::<[ClosureBranch<_, _>; 4]>::new();
        for inner in self.branches.iter() {
            let branch = inner.clone().capture(ctx.solved_argument, ctx.capture_env)?;
            let reduced_pattern = match branch.pattern.reduce(ctx)? {
                Type::Constraint(v) => v,
                _ => panic!("Reduced pattern is not a Constraint type"),
            };
            reduced_branches.push(ClosureBranch::new(
                branch.capture_env,
                reduced_pattern,
                branch.expr,
            ));
        }

        let reduced_iter = reduced_branches.into_iter();
        let new_branches = Arc::from_iter(reduced_iter);
        let rootless = new_branches.iter().all(|b| b.rootless);
        Ok(Self { branches: new_branches, source_info: self.source_info.clone(), rootless }
            .dispatch())
    }

    fn invoke(&self, ctx: InvokeContext<Type<T>, T>) -> Result<Type<T>, TypeError<Type<T>, T>> {
        if !self.is_reduced() {
            return Err(TypeError::ClosureNotReduced(self.clone().dispatch().into()));
        }
        let mut matched_pattern = Collector::new();
        let mut assumptions = smallvec::smallvec![];
        for branch in self.branches.iter() {
            matched_pattern.clear();
            assumptions.clear();
            let empty_generic_binding = GenericBinding::wait_for_bind(None);
            let mut pattern_check_ctx = TypeCheckContext::new(
                &mut assumptions,
                PatternCollector::Deconstruct(&mut matched_pattern),
                ctx.environment,
                ctx.environment,
                &empty_generic_binding,
            );

            if let (ThreeValuedLogic::True, bindings) =
                branch.pattern.deconstruct(ctx.arg.as_ref_dispatcher(), &mut pattern_check_ctx)?
            {
                let params = bindings.into_boxed_slice();

                let mut reduce_ctx = ReductionContext::new(
                    params.as_ref(),
                    CaptureEnvList::new(&branch.capture_env),
                    ctx.rec_assumptions,
                    ctx.gc,
                    ctx.roots,
                );
                let result = branch.expr.reduce(&mut reduce_ctx);
                return result;
            }
        }

        let expect_arg =
            self.branches.iter().map(|b| b.pattern.clone().into_dispatcher()).collect::<Vec<_>>();
        Err(TypeError::AssertFailed(
            (
                AnyOf::new(expect_arg, self.source_info().cloned(), ctx.environment)?,
                ctx.arg.clone(),
            )
                .into(),
        ))
    }

    fn type_of(
        &self,
        _ctx: &mut TypeOfContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        // 提取所有分支的模式类型，构造一个 Lambda 类型
        Ok(Lambda::new(self.branches.iter().map(|b| b.pattern.clone()), self.source_info.clone())
            .dispatch())
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        self.source_info.as_ref()
    }

    fn report_source_info(&self) -> crate::types::TypeReport {
        if let Some(loc) = self.source_info() {
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

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Closure<Type<T>, T> {
    fn represent(
        &self,
        path: &mut FastCycleDetector<TaggedPtr<()>>,
        depth: usize,
        max_depth: usize,
    ) -> String {
        if depth > max_depth {
            return "...".to_string();
        }
        let mut repr = String::from("match");
        for inner in self.branches.iter() {
            let captured_vars: Vec<String> = if inner.capture_env.is_solved() {
                inner
                    .capture_env
                    .solved_vars()
                    .unwrap()
                    .iter()
                    .map(|(v, ty)| {
                        format!("{}: {}", v.as_ref(), ty.represent(path, depth + 1, max_depth))
                    })
                    .collect()
            } else {
                inner
                    .capture_env
                    .unsolved_vars()
                    .unwrap()
                    .iter()
                    .map(|(v, ty)| {
                        format!(
                            "{}: {}",
                            v.as_ref(),
                            match ty {
                                CaptureOrigin::FromParentArgument => "FromParentArgument",
                                CaptureOrigin::FromParentEnv => "FromParentEnv",
                            }
                        )
                    })
                    .collect()
            };
            repr.push_str(" | ");
            if !captured_vars.is_empty() {
                repr.push_str("capture { ");
                repr.push_str(&captured_vars.join(", "));
                repr.push_str(" } ");
            }
            repr.push_str(&inner.pattern.represent(path, depth + 1, max_depth));
            repr.push_str(" => ");
            repr.push_str(&inner.expr.represent(path, depth + 1, max_depth));
        }
        repr.push_str(" | panic");
        repr
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Closure<Type<T>, T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new<V, I, S>(
        branches: Vec<(I, Constraint<Type<T>, T>, V)>,

        source_info: Option<Arc<SourceLocation>>,
    ) -> Type<T>
    where
        V: AsDispatcher<Type<T>, T>,
        I: IntoIterator<Item = (S, CaptureOrigin)>,
        S: Into<Arc<str>>,
    {
        let len = branches.len();
        let mut branches_iter = branches.into_iter();
        let branches = (0..len).map(|_| {
            let (captures, pattern, expr) = branches_iter.next().unwrap();
            let expr_ty = expr.into_dispatcher();
            let capture_env = CaptureEnv::new_unsolved(SmallVec::from_iter(
                captures.into_iter().map(|(s, origin)| (s.into(), origin.clone())),
            ));
            ClosureBranch::new(capture_env, pattern, expr_ty)
        });
        let branches = Arc::from_iter(branches);
        let rootless = branches.iter().all(|b| b.rootless);
        Type::Closure(Closure { branches, rootless, source_info })
    }

    pub fn branches(&self) -> &[ClosureBranch<Type<T>, T>] {
        &self.branches
    }

    pub fn is_reduced(&self) -> bool {
        let mut reduced = true;
        for branch in self.branches().iter() {
            if !branch.capture_env.is_solved() {
                reduced = false;
                break;
            }
        }
        reduced
    }

    pub fn impls(
        &self,
        other: &Self,

        source_info: Option<Arc<SourceLocation>>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        if !self.is_reduced() {
            return Err(TypeError::ClosureNotReduced(self.clone().dispatch().into()));
        }
        if !other.is_reduced() {
            return Err(TypeError::ClosureNotReduced(other.clone().dispatch().into()));
        }
        let self_len = self.branches.len();
        let len = self_len + other.branches.len();
        let branches = (0..len).map(|i| {
            if i < self_len {
                self.branches[i].clone()
            } else {
                other.branches[i - self_len].clone()
            }
        });
        let branches = Arc::from_iter(branches);
        let rootless = branches.iter().all(|b| b.rootless);
        Ok(Closure { branches, rootless, source_info }.dispatch())
    }

    pub fn identity(
        source_info: Option<Arc<SourceLocation>>,
        env: CaptureEnvList<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Self::lazy(source_info, "var#x", Variable::new_argument("var#x", None), env)
    }

    pub fn lazy<S: Into<Arc<str>>, V: AsDispatcher<Type<T>, T>>(
        source_info: Option<Arc<SourceLocation>>,
        bind_name: S,
        expr: V,
        env: CaptureEnvList<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        let bind_name: Arc<str> = bind_name.into();
        let branch = ClosureBranch::new(
            CaptureEnv::Solved(SmallVec::new()),
            Constraint::new_constraint(
                Pattern::<Type<T>, T>::new(bind_name.clone(), AllOf::unknown(None), None),
                vec![(bind_name, AllOf::unknown(None))],
                env,
                None,
            )?,
            expr.into_dispatcher(),
        );
        let rootless = branch.rootless;
        Ok(Closure { branches: Arc::from([branch]), rootless, source_info }.dispatch())
    }
}
