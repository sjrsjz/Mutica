use std::{ops::ControlFlow, sync::Arc};

use arc_gc::{arc::GCArc, traceable::GCTraceable};
use arena_arc::{ArcSingle, ArcSlice};
use smallvec::SmallVec;

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeRef, CoinductiveTypeWithAny, CollectorExt,
        GcAllocObject, PatternCollector, Representable, Rootable, TaggedPtr, Type,
        TypeCheckContext, TypeError, TypeRef,
        allocator::Allocators,
        allof::AllOf,
        unify::{
            ArgumentBinding, GenericBinding, capture_env::CaptureEnvList, collector::Collector,
            path_collector::PathCollector,
        },
    },
    util::{source_info::SourceLocation, three_valued_logic::ThreeValuedLogic},
};

pub struct Constraint<U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    pattern: ArcSingle<U, usize>,
    constraint: ArcSlice<(Arc<str>, U), usize>,
    rootless: bool,
    source_info: Option<Arc<SourceLocation>>,
    #[doc(hidden)]
    _phantom: std::marker::PhantomData<V>,
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Clone for Constraint<U, V> {
    fn clone(&self) -> Self {
        Self {
            pattern: self.pattern.clone(),
            constraint: self.constraint.clone(),
            rootless: self.rootless,
            source_info: self.source_info.clone(),
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> GCTraceable<V> for Constraint<U, V> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<V>>) {
        if self.rootless {
            return;
        }
        self.pattern.collect(queue);
        for (_, ty) in self.constraint.iter() {
            ty.collect(queue);
        }
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Rootable<V> for Constraint<U, V> {
    fn upgrade(&self, collected: &mut Vec<GCArc<V>>) {
        if self.rootless {
            return;
        }
        self.pattern.upgrade(collected);
        for (_, ty) in self.constraint.iter() {
            ty.upgrade(collected);
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Constraint<Type<T>, T> {
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
            format!("exist {} where {{ ", self.pattern().represent(path, depth + 1, max_depth));
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

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for Constraint<Type<T>, T> {
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

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for Constraint<Type<T>, T> {
    fn check(
        &self,
        other: TypeRef<T>,
        ctx: &mut super::TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        ctx.pattern_collector.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.coinductive_assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.bound_generic_variables,
                ctx.allocators,
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
                ctx.coinductive_assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.bound_generic_variables,
                ctx.allocators,
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
        &self,
        ctx: &mut super::ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        // 约束类型的规约主要是规约其主体类型，并尝试将约束条件中的变量绑定到规约后的主体类型上
        let reduced_pattern = self.pattern.get().reduce(ctx)?;
        let mut reduced_constraint = SmallVec::<[(Arc<str>, Type<T>); 8]>::new();
        for (k, v) in self.constraint.iter() {
            reduced_constraint.push((k.clone(), v.reduce(ctx)?));
        }
        let rootless =
            reduced_pattern.rootless() && reduced_constraint.iter().all(|(_, ty)| ty.rootless());
        let len = reduced_constraint.len();
        let mut iter = reduced_constraint.into_iter();
        Ok(Type::Constraint(Constraint {
            pattern: ctx.allocators.v.alloc_value(reduced_pattern),
            constraint: ctx.allocators.kv.alloc(len, |_| iter.next().unwrap()),
            rootless,
            source_info: self.source_info.clone(),
            _phantom: std::marker::PhantomData,
        }))
    }

    fn invoke(
        &self,
        _ctx: super::InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.clone().dispatch().into()))
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        self.source_info.as_ref()
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

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveTypeWithAny<Type<T>, T>
    for Constraint<Type<T>, T>
{
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

impl<T: GcAllocObject<T, Inner = Type<T>>> Constraint<Type<T>, T> {
    pub fn subof_constraint(
        &self,
        other: &Constraint<Type<T>, T>,
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
        let lhs_pattern = self.pattern();
        let rhs_pattern = other.pattern();
        let mut collected_path = Vec::new();
        let mut path_collector = PathCollector::from(&mut collected_path);
        let empty_bindings = GenericBinding::wait_for_bind(Some(ctx.bound_generic_variables));
        let mut pattern_check_ctx = TypeCheckContext::new(
            ctx.coinductive_assumptions,
            PatternCollector::Subtyping(&mut path_collector),
            ctx.lhs_env,
            ctx.rhs_env,
            &empty_bindings,
            ctx.allocators,
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
                &[],
                Some(ctx.bound_generic_variables),
            );
            let mut new_ctx = TypeCheckContext::new(
                ctx.coinductive_assumptions,
                PatternCollector::None,
                ctx.lhs_env,
                ctx.rhs_env,
                &bound_subtype_assumptions,
                ctx.allocators,
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
                let flipped = ctx.bound_generic_variables.flip();
                let bound_subtype_assumptions = GenericBinding::subtype_assumption(
                    lhs_subtype_assumptions,
                    &[],
                    Some(&flipped),
                );
                let mut new_ctx = TypeCheckContext::new(
                    ctx.coinductive_assumptions,
                    PatternCollector::None,
                    ctx.rhs_env,
                    ctx.lhs_env,
                    &bound_subtype_assumptions,
                    ctx.allocators,
                );
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
                    ctx.coinductive_assumptions,
                    PatternCollector::None,
                    ctx.lhs_env,
                    ctx.rhs_env,
                    &bound_subtype_assumptions,
                    ctx.allocators,
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
            ctx.coinductive_assumptions,
            PatternCollector::Deconstruct(&mut collector),
            ctx.lhs_env,
            ctx.rhs_env,
            &empty_bindings,
            ctx.allocators,
        );
        // 先检查主体类型，即 X where {...} 的 X 部分
        let result = other.check(self.pattern().as_ref_dispatcher(), &mut new_ctx)?;
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
        GenericBinding::finalize(&mut pool, ctx.lhs_env, ctx.allocators)?; // 解构的值都来源于于 LHS 环境
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
            let ty = env
                .lookup(x, 0, env.is_lhs())
                .expect("Layer should be found")
                .expect("Variable should be bound")
                .clone();
            let mut new_ctx = TypeCheckContext::new(
                ctx.coinductive_assumptions,
                PatternCollector::None,
                ctx.lhs_env,
                ctx.rhs_env,
                &env,
                ctx.allocators,
            );
            check_result &= ty.check(c.as_ref_dispatcher(), &mut new_ctx)?;
            if let ThreeValuedLogic::False = check_result {
                break;
            }
        }
        Ok((result & check_result, bindings))
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Constraint<Type<T>, T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new<P, C>(
        expr: P,
        constraint: impl IntoIterator<Item = (Arc<str>, C)>,
        env: CaptureEnvList<Type<T>, T>,
        allocators: &mut Allocators<Type<T>, T>,
        source_info: Option<Arc<SourceLocation>>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>>
    where
        P: AsDispatcher<Type<T>, T>,
        C: AsDispatcher<Type<T>, T>,
    {
        Self::new_constraint(expr, constraint, env, allocators, source_info)
            .map(|c| c.into_dispatcher())
    }

    pub fn new_constraint<P, C>(
        expr: P,
        constraint: impl IntoIterator<Item = (Arc<str>, C)>,
        env: CaptureEnvList<Type<T>, T>,
        allocators: &mut Allocators<Type<T>, T>,
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
            .map(|(k, v)| AllOf::new(v.into_iter(), allocators, None, env).map(|v| (k, v)))
            .collect::<Result<SmallVec<[(Arc<str>, Type<T>); 4]>, TypeError<Type<T>, T>>>()?;
        let expr = expr.into_dispatcher();
        let rootless = expr.rootless() && merged_constraints.iter().all(|(_, ty)| ty.rootless());
        let len = merged_constraints.len();
        let mut iter = merged_constraints.into_iter();
        Ok(Constraint {
            pattern: allocators.v.alloc_value(expr),
            constraint: allocators.kv.alloc(len, |_| iter.next().unwrap()),
            rootless,
            source_info,
            _phantom: std::marker::PhantomData,
        })
    }

    pub fn pattern(&self) -> &Type<T> {
        &self.pattern
    }

    pub fn constraint(&self) -> &[(Arc<str>, Type<T>)] {
        &self.constraint
    }
}
