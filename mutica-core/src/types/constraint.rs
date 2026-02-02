use std::{ops::ControlFlow, sync::Arc};

use arc_gc::{arc::GCArc, traceable::GCTraceable};
use smallvec::SmallVec;

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeRef, CoinductiveTypeWithAny, CollectorExt,
        GcAllocObject, PatternCollector, Representable, Rootable, TaggedPtr, Type,
        TypeCheckContext, TypeError, TypeRef,
        allof::AllOf,
        unify::{
            ArgumentBinding, GenericBinding, capture_env::CaptureEnvList, collector::Collector,
            path_collector::PathCollector,
        },
    },
    util::{arc_opt::ArcOpt, source_info::SourceLocation, three_valued_logic::ThreeValuedLogic},
};

pub struct Constraint<T: GcAllocObject<T, Inner = Type<T>>>(
    // P(x) where {x1: T1, x2: T2, ...}
    #[allow(clippy::type_complexity)]
    ArcOpt<(Type<T>, Vec<(Arc<str>, Type<T>)>, Option<Arc<SourceLocation>>)>,
);

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Constraint<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Constraint<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        self.0.0.collect(queue);
        for (_, ty) in self.0.1.iter() {
            ty.collect(queue);
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Constraint<T> {
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        self.0.0.upgrade(collected);
        for (_, ty) in self.0.1.iter() {
            ty.upgrade(collected);
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Constraint<T> {
    fn represent(
        &self,
        path: &mut crate::util::cycle_detector::FastCycleDetector<TaggedPtr<()>>,
        depth: usize,
        max_depth: usize,
    ) -> String {
        if depth > max_depth {
            return "...".to_string();
        }
        // exist P(x) where {x1: T1, x2: T2, ...}
        let mut repr =
            format!("exist {} where {{ ", self.expr().represent(path, depth + 1, max_depth));
        let constraints: Vec<String> = self
            .constraint()
            .iter()
            .map(|(k, v)| format!("{}: {}", k, v.represent(path, depth + 1, max_depth)))
            .collect();
        repr.push_str(&constraints.join(", "));
        repr.push_str(" }");
        repr
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for Constraint<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;

    fn into_dispatcher(self) -> Type<T> {
        Type::Constraint(self)
    }

    fn as_ref_dispatcher<'a>(&'a self) -> Self::RefDispatcher<'a> {
        TypeRef::Constraint(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for Constraint<T> {
    fn check(
        &self,
        other: TypeRef<T>,
        ctx: &mut super::TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        ctx.pattern_collector.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.instance_assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.bound_generic_variables,
            );
            match other {
                TypeRef::Any(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::All(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Constraint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::SubOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

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
                ctx.bound_generic_variables,
            );
            match other {
                TypeRef::Any(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::All(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Constraint(other) => self.subof_constraint(
                    other,
                    &mut inner_ctx,
                    None::<
                        fn(
                            &mut TypeCheckContext<Type<T>, T>,
                        )
                            -> Result<ThreeValuedLogic, TypeError<Type<T>, T>>,
                    >,
                ),
                _ => Ok(ThreeValuedLogic::False),
            }
        })
    }

    fn reduce(
        mut self,
        ctx: &mut super::ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        match self.0.modify(|(p, c, source_info)| {
            let p = p.reduce(ctx)?;
            let c = c
                .into_iter()
                .map(|(k, v)| v.reduce(ctx).map(|v| (k, v)))
                .collect::<Result<Vec<_>, TypeError<Type<T>, T>>>()?;
            Ok((p, c, source_info))
        })? {
            Some(()) => Ok(self.dispatch()),
            None => {
                let (p, c, source_info) = self.0.as_ref();
                let p = p.clone().reduce(ctx)?;
                let c = c
                    .iter()
                    .map(|(k, v)| v.clone().reduce(ctx).map(|v| (k.clone(), v)))
                    .collect::<Result<Vec<_>, TypeError<Type<T>, T>>>()?;
                Ok(Self(ArcOpt::new((p, c, source_info.clone()))).dispatch())
            }
        }
    }

    fn invoke(
        self,
        _ctx: super::InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.dispatch().into()))
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        self.0.2.as_ref()
    }

    fn report_source_info(&self) -> crate::types::TypeReport {
        if let Some(loc) = self.source_info() {
            let span = loc.span().clone();
            let filepath = loc.source().filepath().to_string();
            ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                .with_message(format!("Constraint type at {}", filepath))
                .with_label(
                    ariadne::Label::new((filepath, span))
                        .with_message("Constraint type defined here"),
                )
                .finish()
        } else {
            ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                .with_message("Constraint type has no source location")
                .with_label(
                    ariadne::Label::new(("<unknown>".to_string(), 0..0))
                        .with_message("Location unknown"),
                )
                .finish()
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveTypeWithAny<Type<T>, T> for Constraint<T> {
    #[stacksafe::stacksafe]
    fn accept(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        let (result, _) = self.deconstruct(other, ctx)?;
        Ok(result)
    }

    #[stacksafe::stacksafe]
    fn superof(
        &self,
        _other: Self::RefDispatcher<'_>,
        _ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        panic!("This method should not be called")
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Constraint<T> {
    pub fn subof_constraint(
        &self,
        other: &Constraint<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
        additional_check: Option<
            impl Fn(
                &mut TypeCheckContext<Type<T>, T>,
            ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>>,
        >,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        // println!(
        //     "Checking Pattern Subtype: {} <: {}",
        //     self.represent(&mut Default::default(), 0, 5),
        //     other.represent(&mut Default::default(), 0, 5)
        // );
        // println!(" bound_generic_layers: {:?}", ctx.bound_generic_variables);
        let lhs_pattern = self.expr();
        let rhs_pattern = other.expr();
        let mut collected_path = Vec::new();
        let mut path_collector = PathCollector::from(&mut collected_path);
        let empty_bindings = GenericBinding::wait_for_bind(Some(ctx.bound_generic_variables));
        let fipped_empty_bindings = empty_bindings.flip();
        let mut pattern_check_ctx = TypeCheckContext::new(
            ctx.instance_assumptions,
            PatternCollector::Subtyping(&mut path_collector),
            ctx.lhs_env,
            ctx.rhs_env,
            &empty_bindings,
        );
        let pattern_result =
            lhs_pattern.subof(rhs_pattern.as_ref_dispatcher(), &mut pattern_check_ctx)?;
        if let ThreeValuedLogic::False = pattern_result {
            return Ok(ThreeValuedLogic::False);
        }
        // println!("Pattern subtype check passed.");

        let mut pass = ThreeValuedLogic::False;
        let mut rhs_subtype_assumptions = Vec::new();

        let _ = path_collector.walk(|lhs_subtype_assumptions| {
            let mut lhs_tys = SmallVec::<[(&Arc<str>, usize); 8]>::new();
            let mut rhs_tys = SmallVec::<[(&Arc<str>, usize); 8]>::new();
            rhs_subtype_assumptions.clear();

            for (lhs, rhs) in lhs_subtype_assumptions {
                // 插入lhs_tys和rhs_tys
                match lhs_tys.iter_mut().find(|v| v.0.as_ref() == lhs.as_ref()) {
                    Some((_, c)) => {
                        *c += 1;
                    }
                    None => lhs_tys.push((lhs, 0)),
                }
                match rhs_tys.iter_mut().find(|v| v.0.as_ref() == rhs.as_ref()) {
                    Some((_, c)) => {
                        *c += 1;
                    }
                    None => rhs_tys.push((rhs, 0)),
                }
            }

            let bound_subtype_assumptions = GenericBinding::subtype_assumption(
                lhs_subtype_assumptions,
                &rhs_subtype_assumptions,
                Some(ctx.bound_generic_variables),
            );
            let mut new_ctx = TypeCheckContext::new(
                ctx.instance_assumptions,
                PatternCollector::None,
                ctx.lhs_env,
                ctx.rhs_env,
                &bound_subtype_assumptions,
            );

            let mut constraint_result = ThreeValuedLogic::True;
            for (lhs, rhs) in lhs_subtype_assumptions {
                // println!("LHS var: {}, RHS var: {}", lhs, rhs);
                // 检查计数lhs >= rhs
                let lhs_count = lhs_tys
                    .iter()
                    .find(|(k, _)| k.as_ref() == lhs.as_ref())
                    .map(|(_, c)| *c)
                    .unwrap_or(0);
                let rhs_count = rhs_tys
                    .iter()
                    .find(|(k, _)| k.as_ref() == rhs.as_ref())
                    .map(|(_, c)| *c)
                    .unwrap_or(0);
                if lhs_count < rhs_count {
                    constraint_result = ThreeValuedLogic::False;
                    break;
                }
                let lhs_ty = self
                    .constraint()
                    .iter()
                    .find_map(|(k, v)| if k.eq(lhs) { Some(v) } else { None })
                    .ok_or_else(|| TypeError::MissingVariable(lhs.as_ref().into()))?;
                let rhs_ty = other
                    .constraint()
                    .iter()
                    .find_map(|(k, v)| if k.eq(rhs) { Some(v) } else { None })
                    .ok_or_else(|| TypeError::MissingVariable(rhs.as_ref().into()))?;
                constraint_result &= lhs_ty.subof(rhs_ty.as_ref_dispatcher(), &mut new_ctx)?;
                if let ThreeValuedLogic::False = constraint_result {
                    break;
                }
            }

            // 正向约束检查通过后，进行反向检查
            if let ThreeValuedLogic::True = constraint_result {
                for (lhs, rhs) in lhs_subtype_assumptions {
                    let lhs_ty = self
                        .constraint()
                        .iter()
                        .find_map(|(k, v)| if k.eq(lhs) { Some(v) } else { None })
                        .ok_or_else(|| TypeError::MissingVariable(lhs.as_ref().into()))?;
                    let rhs_ty = other
                        .constraint()
                        .iter()
                        .find_map(|(k, v)| if k.eq(rhs) { Some(v) } else { None })
                        .ok_or_else(|| TypeError::MissingVariable(rhs.as_ref().into()))?;

                    let mut new_ctx = TypeCheckContext::new(
                        ctx.instance_assumptions,
                        PatternCollector::None,
                        ctx.rhs_env,
                        ctx.lhs_env,
                        &fipped_empty_bindings,
                    );
                    // 如果它居然是等价的(即当 U <: V 的时候发现 V <: U 也成立)，则加入反向假设
                    let subof_result = rhs_ty.subof(lhs_ty.as_ref_dispatcher(), &mut new_ctx)?;
                    if let ThreeValuedLogic::True = subof_result {
                        rhs_subtype_assumptions.push((rhs.clone(), lhs.clone())); // 加入反向假设
                    }
                }
            }

            // println!("LHS: {:?}", lhs_subtype_assumptions);
            // println!("RHS: {:?}", rhs_subtype_assumptions);
            if let Some(check_fn) = &additional_check {
                let bound_subtype_assumptions = GenericBinding::param_subtype_assumption(
                    lhs_subtype_assumptions,
                    &rhs_subtype_assumptions,
                    Some(ctx.bound_generic_variables),
                );

                let mut new_ctx = TypeCheckContext::new(
                    ctx.instance_assumptions,
                    PatternCollector::None,
                    ctx.lhs_env,
                    ctx.rhs_env,
                    &bound_subtype_assumptions,
                );
                constraint_result &= check_fn(&mut new_ctx)?;
            }

            pass |= constraint_result;

            if let ThreeValuedLogic::True = pass {
                Ok(ControlFlow::Break(()))
            } else {
                Ok(ControlFlow::Continue(()))
            }
        })?;
        // println!("Path: {:?}", &collected_path);
        Ok(pass & pattern_result)
    }

    #[allow(clippy::type_complexity)]
    pub fn deconstruct(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<
        (ThreeValuedLogic, SmallVec<[(Arc<str>, ArgumentBinding<Type<T>, T>); 4]>),
        TypeError<Type<T>, T>,
    > {
        let mut collector = Collector::new();
        let empty_bindings = GenericBinding::wait_for_bind(Some(ctx.bound_generic_variables));
        let mut new_ctx = TypeCheckContext::new(
            ctx.instance_assumptions,
            PatternCollector::Deconstruct(&mut collector),
            ctx.lhs_env,
            ctx.rhs_env,
            &empty_bindings,
        );
        // 先检查主体类型，即 X where {...} 的 X 部分
        let result = other.check(self.expr().as_ref_dispatcher(), &mut new_ctx)?;
        if result == ThreeValuedLogic::False {
            return Ok((ThreeValuedLogic::False, SmallVec::new()));
        }
        // 收集变量绑定，并进行非线性约束检查
        let mut pool = self
            .constraint()
            .iter()
            .map(|(v, _)| (v.clone(), ArgumentBinding::Collect(SmallVec::new())))
            .collect::<Vec<_>>();
        let collected = collector.take_items().expect("Unable to take items from collector");
        for (k, v) in collected {
            GenericBinding::bind(&mut pool, k, v)?
        }
        GenericBinding::finalize(&mut pool, ctx.lhs_env)?; // 解构的值都来源于于 LHS 环境
        let env = GenericBinding::pattern(&pool, &[], Some(ctx.bound_generic_variables));

        let mut bindings: SmallVec<[(Arc<str>, ArgumentBinding<Type<T>, T>); 4]> = SmallVec::new();
        // 确保所有变量都已绑定
        for (k, v) in env.type_vars(env.is_lhs()) {
            match v {
                ArgumentBinding::Bound(_) => bindings.push((k.clone(), v.clone())),
                _ => {
                    return Ok((ThreeValuedLogic::False, bindings)); // 未绑定变量，失败
                }
            }
        }
        // 先推入当前已绑定环境到 collected 栈
        let mut check_result = ThreeValuedLogic::True;
        for (x, c) in self.constraint() {
            // println!("Checking constraint for variable: {}, c: {:?}", x, c);
            let ty = env.lookup(x, env.is_lhs()).expect("Variable should be bound").clone();
            let mut new_ctx = TypeCheckContext::new(
                ctx.instance_assumptions,
                PatternCollector::None,
                ctx.lhs_env,
                ctx.rhs_env,
                &env,
            );
            check_result &= ty.check(c.as_ref_dispatcher(), &mut new_ctx)?;
            if let ThreeValuedLogic::False = check_result {
                break;
            }
        }
        Ok((result & check_result, bindings))
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Constraint<T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new<P, C>(
        expr: P,
        constraint: impl IntoIterator<Item = (Arc<str>, C)>,
        env: CaptureEnvList<Type<T>, T>,
        source_info: Option<Arc<SourceLocation>>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>>
    where
        P: AsDispatcher<Type<T>, T>,
        C: AsDispatcher<Type<T>, T>,
    {
        Self::new_constraint(expr, constraint, env, source_info).map(|c| c.into_dispatcher())
    }

    pub fn new_constraint<P, C>(
        expr: P,
        constraint: impl IntoIterator<Item = (Arc<str>, C)>,
        env: CaptureEnvList<Type<T>, T>,
        source_info: Option<Arc<SourceLocation>>,
    ) -> Result<Self, TypeError<Type<T>, T>>
    where
        P: AsDispatcher<Type<T>, T>,
        C: AsDispatcher<Type<T>, T>,
    {
        let mut set = SmallVec::<[(Arc<str>, SmallVec<[Type<T>; 2]>); 4]>::new();
        for (k, v) in constraint.into_iter() {
            // 合并同名约束
            if let Some((_, existing)) = set.iter_mut().find(|(key, _)| *key == k) {
                existing.push(v.into_dispatcher());
            } else {
                set.push((k, SmallVec::from_iter([v.into_dispatcher()])));
            }
        }
        let merged_constraints = set
            .into_iter()
            .map(|(k, v)| AllOf::new(v.into_iter(), None, env).map(|v| (k, v)))
            .collect::<Result<Vec<_>, TypeError<Type<T>, T>>>()?;
        Ok(Self(ArcOpt::new((expr.into_dispatcher(), merged_constraints, source_info))))
    }

    pub fn expr(&self) -> &Type<T> {
        &self.0.0
    }

    pub fn constraint(&self) -> &[(Arc<str>, Type<T>)] {
        &self.0.1
    }
}
