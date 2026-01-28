use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, CollectorExt, Environment,
        GcAllocObject, InvokeContext, PatternCollector, ReductionContext, Representable, Rootable,
        TaggedPtr, Type, TypeCheckContext, TypeError, TypeRef,
        allof::AllOf,
        anyof::AnyOf,
        constraint::Constraint,
        pattern::Pattern,
        unify::{EnvironmentStack, EnvironmentVarState, EnvironmentView, collector::Collector},
        variable::Variable,
    },
    util::{
        arc_opt::ArcOpt, cycle_detector::FastCycleDetector, source_info::SourceLocation,
        three_valued_logic::ThreeValuedLogic,
    },
};

pub struct ClosureBranch<T: GcAllocObject<T, Inner = Type<T>>> {
    pub captured_vars: Environment<Type<T>, T>,
    pub pattern: Constraint<T>,
    pub expr: Type<T>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for ClosureBranch<T> {
    fn clone(&self) -> Self {
        Self {
            captured_vars: self.captured_vars.clone(),
            pattern: self.pattern.clone(),
            expr: self.expr.clone(),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for ClosureBranch<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        for (_, var) in self.captured_vars.type_vars() {
            if let EnvironmentVarState::Bound(ty) = var {
                ty.collect(queue);
            }
        }
        self.pattern.collect(queue);
        self.expr.collect(queue);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for ClosureBranch<T> {
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        for (_, var) in self.captured_vars.type_vars() {
            if let EnvironmentVarState::Bound(ty) = var {
                ty.upgrade(collected);
            }
        }
        self.pattern.upgrade(collected);
        self.expr.upgrade(collected);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> ClosureBranch<T> {
    pub fn capture(
        mut self,
        pattern_env: EnvironmentView<Type<T>, T>,
        capture_env: EnvironmentView<Type<T>, T>,
    ) -> Result<Self, TypeError<Type<T>, T>> {
        self.captured_vars = self.captured_vars.capture_from(pattern_env, capture_env)?;
        Ok(self)
    }
}

pub struct Closure<T: GcAllocObject<T, Inner = Type<T>>> {
    #[allow(clippy::type_complexity)]
    inner: ArcOpt<(Vec<ClosureBranch<T>>, Option<Arc<SourceLocation>>)>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Closure<T> {
    fn clone(&self) -> Self {
        Self { inner: self.inner.clone() }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Closure<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        let (branches, _) = self.inner.as_ref();
        for inner in branches {
            inner.collect(queue);
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Closure<T> {
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        let (branches, _) = self.inner.as_ref();
        for inner in branches {
            inner.upgrade(collected);
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
        ctx.pattern_collector.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.instance_assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.collected_bindings,
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
                    let lhs_branches = self.branches();
                    let rhs_patterns = other.patterns();

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
        ctx: &mut super::TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        ctx.pattern_collector.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.instance_assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.collected_bindings,
            );
            match other {
                TypeRef::Any(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::All(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Closure(other) => {
                    // Closure 的子类型关系：模式逆变，expr 协变
                    let lhs_branches = self.branches();
                    let rhs_branches = other.branches();

                    let mut i = 0usize;
                    let mut j = 0usize;
                    let mut result = ThreeValuedLogic::True;

                    while i < lhs_branches.len() && j < rhs_branches.len() {
                        let lhs = &lhs_branches[i];
                        let rhs = &rhs_branches[j];
                        let expr_check = |ctx: &mut TypeCheckContext<_, _>| {
                            lhs.expr.subof(rhs.expr.as_ref_dispatcher(), ctx)
                        };
                        match rhs.pattern.subof_constraint(
                            &lhs.pattern,
                            &mut inner_ctx,
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
                    }

                    if j >= rhs_branches.len() { Ok(result) } else { Ok(ThreeValuedLogic::False) }
                }

                // TypeRef::Closure(v) => {
                //     let (self_branches, _) = self.inner.as_ref();
                //     let (v_branches, _) = v.inner.as_ref();

                //     if self_branches.len() != v_branches.len() {
                //         return Ok(ThreeValuedLogic::False);
                //     }

                //     let mut all = ThreeValuedLogic::True;

                //     for (self_inner, other_inner) in self_branches.iter().zip(v_branches.iter()) {
                //         let mut pattern_ctx = TypeCheckContext::new(
                //             ctx.assumptions,
                //             None,
                //             ctx.lhs_env,
                //             ctx.rhs_env,
                //             ctx.collected,
                //         );

                //         all &= test_true!(
                //             self_inner
                //                 .expr
                //                 .subof(other_inner.expr.as_ref_dispatcher(), &mut pattern_ctx)?
                //         );

                //         let mut pattern_ctx = TypeCheckContext::new(
                //             ctx.assumptions,
                //             None,
                //             ctx.rhs_env,
                //             ctx.lhs_env,
                //             ctx.collected,
                //         );
                //         all &= test_true!(
                //             other_inner
                //                 .pattern
                //                 .subof(self_inner.pattern.as_ref_dispatcher(), &mut pattern_ctx)?
                //         )
                //     }
                //     Ok(all)
                // }
                _ => Ok(ThreeValuedLogic::False),
            }
        })
    }

    fn reduce(
        mut self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        match self.inner.modify(|(branches, source_info)| {
            let reduced_branches = branches
                .into_iter()
                .map(|branch| {
                    let branch =
                        branch.capture(ctx.pattern_environment, ctx.capture_environment)?;
                    let reduced_pattern = match branch.pattern.reduce(ctx)? {
                        Type::Constraint(v) => v,
                        _ => panic!("Reduced pattern is not a Constraint type"),
                    };
                    Ok(ClosureBranch {
                        captured_vars: branch.captured_vars,
                        pattern: reduced_pattern,
                        expr: branch.expr,
                    })
                })
                .collect::<Result<Vec<_>, TypeError<Type<T>, T>>>()?;

            Ok((reduced_branches, source_info))
        })? {
            Some(()) => Ok(self.dispatch()),
            None => {
                let (branches, source_info) = self.inner.as_ref();
                let mut reduced_branches = Vec::with_capacity(branches.len());
                for inner in branches.iter() {
                    let branch =
                        inner.clone().capture(ctx.pattern_environment, ctx.capture_environment)?;
                    let reduced_pattern = match branch.pattern.reduce(ctx)? {
                        Type::Constraint(v) => v,
                        _ => panic!("Reduced pattern is not a Constraint type"),
                    };
                    reduced_branches.push(ClosureBranch {
                        captured_vars: branch.captured_vars,
                        pattern: reduced_pattern,
                        expr: branch.expr,
                    });
                }
                Ok(Self { inner: ArcOpt::new((reduced_branches, source_info.clone())) }.dispatch())
            }
        }
    }

    fn invoke(self, ctx: InvokeContext<Type<T>, T>) -> Result<Type<T>, TypeError<Type<T>, T>> {
        let (branches, _) = self.inner.as_ref();
        let mut matched_pattern = Collector::new();
        let mut assumptions = smallvec::smallvec![];
        for branch in branches.iter() {
            matched_pattern.clear();
            assumptions.clear();
            let mut env_stack = EnvironmentStack::new();
            let mut pattern_check_ctx = TypeCheckContext::new(
                &mut assumptions,
                PatternCollector::Deconstruct(&mut matched_pattern),
                ctx.environment,
                ctx.environment,
                &mut env_stack,
            );

            if let (ThreeValuedLogic::True, bindings) =
                branch.pattern.deconstruct(ctx.arg.as_ref_dispatcher(), &mut pattern_check_ctx)?
            {
                let param_env = Environment::new_bound(bindings);

                let mut reduce_ctx = ReductionContext::new(
                    param_env.view(),
                    branch.captured_vars.view(),
                    ctx.rec_assumptions,
                    ctx.gc,
                    ctx.roots,
                );
                let result = branch.expr.clone().reduce(&mut reduce_ctx);
                return result;
            }
        }

        let expect_arg =
            self.branches().iter().map(|b| b.pattern.clone().into_dispatcher()).collect::<Vec<_>>();
        Err(TypeError::AssertFailed(
            (
                AnyOf::new(expect_arg, self.source_info().cloned(), ctx.environment)?,
                ctx.arg.clone(),
            )
                .into(),
        ))
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        let (_, source_info) = self.inner.as_ref();
        source_info.as_ref()
    }

    fn report_source_info(&self) -> crate::types::TypeReport {
        let (_, source_info) = self.inner.as_ref();
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
        let (branches, _) = self.inner.as_ref();
        let mut repr = String::from("match");
        for inner in branches.iter() {
            repr.push_str(" | capture { ");
            let captured_vars: Vec<String> = inner
                .captured_vars
                .type_vars()
                .iter()
                .map(|(v, ty)| {
                    let ty_str = match ty {
                        EnvironmentVarState::Bound(ty) => ty.represent(path, depth + 1, max_depth),
                        EnvironmentVarState::FromPattern => "FromPattern".to_string(),
                        EnvironmentVarState::FromCapture => "FromCapture".to_string(),
                        EnvironmentVarState::BoundList(_) => panic!(
                            "CRITICAL: Trying to represent a BoundList variable from an environment which didn't finalize it."
                        ),
                        EnvironmentVarState::Phantom(_) => unreachable!(),
                    };
                    format!("{}: {}", v.as_ref(), ty_str)
                })
                .collect();
            repr.push_str(&captured_vars.join(", "));
            repr.push_str(" } ");
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
    pub fn new<V, I, S>(
        branches: Vec<(I, Constraint<T>, V)>,
        source_info: Option<Arc<SourceLocation>>,
    ) -> Type<T>
    where
        V: AsDispatcher<Type<T>, T>,
        I: IntoIterator<Item = (S, EnvironmentVarState<Type<T>, T>)>,
        S: Into<Arc<str>>,
    {
        let branches_vec = branches
            .into_iter()
            .map(|(captures, pattern, expr)| {
                let expr_ty = expr.into_dispatcher();
                ClosureBranch {
                    captured_vars: Environment::new(
                        captures,
                        Vec::<(Arc<str>, Arc<str>, usize, usize)>::new(),
                    ),
                    pattern,
                    expr: expr_ty,
                }
            })
            .collect::<Vec<_>>();

        Type::Closure(Closure { inner: ArcOpt::new((branches_vec, source_info)) })
    }

    pub fn branches(&self) -> &[ClosureBranch<T>] {
        &self.inner.as_ref().0
    }

    pub fn impls(self, other: Self, source_info: Option<Arc<SourceLocation>>) -> Type<T> {
        let mut new_branches = self.branches().to_vec();
        new_branches.extend_from_slice(other.branches());

        Closure { inner: ArcOpt::new((new_branches, source_info)) }.into_dispatcher()
    }

    pub fn identity(
        source_info: Option<Arc<SourceLocation>>,
        env: EnvironmentView<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Self::lazy(source_info, "var#x", Variable::new_argument("var#x", None), env)
    }

    pub fn lazy<S: Into<Arc<str>>, V: AsDispatcher<Type<T>, T>>(
        source_info: Option<Arc<SourceLocation>>,
        bind_name: S,
        expr: V,
        env: EnvironmentView<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        let bind_name: Arc<str> = bind_name.into();
        let branch = ClosureBranch {
            captured_vars: Environment::default(),
            pattern: Constraint::new_constraint(
                Pattern::<T>::new(bind_name.clone(), 0, None),
                vec![(bind_name, AllOf::unknown(None))],
                env,
                None,
            )?,
            expr: expr.into_dispatcher(),
        };
        Ok(Closure { inner: ArcOpt::new((vec![branch], source_info)) }.into_dispatcher())
    }
}
