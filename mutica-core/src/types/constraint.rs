use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};
use smallvec::SmallVec;

use crate::{
    test_true,
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeRef, CoinductiveTypeWithAny, GcAllocObject,
        Representable, Rootable, TaggedPtr, Type, TypeCheckContext, TypeError, TypeRef,
        unify::{Environment, EnvironmentVarState},
    },
    util::{
        arc_opt::ArcOpt, collector::Collector, source_info::SourceLocation,
        three_valued_logic::ThreeValuedLogic,
    },
};

pub struct Constraint<T: GcAllocObject<T, Inner = Type<T>>>(
    // x, P(x), F(x), G(x)
    // => P(x) exist x where F(x): G(x)
    #[allow(clippy::type_complexity)]
    ArcOpt<(Arc<[String]>, Type<T>, Type<T>, Type<T>, Option<Arc<SourceLocation>>)>,
);

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Constraint<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Constraint<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        self.0.1.collect(queue);
        self.0.2.collect(queue);
        self.0.3.collect(queue);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Constraint<T> {
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        self.0.1.upgrade(collected);
        self.0.2.upgrade(collected);
        self.0.3.upgrade(collected);
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
        // T exist {x, y, ...} where F: G
        let expr = self.expr().represent(path, depth + 1, max_depth);
        let f = self.0.2.represent(path, depth + 1, max_depth);
        let g = self.0.3.represent(path, depth + 1, max_depth);
        let vars = self.0.0.join(", ");
        if self.0.0.len() == 1 {
            return format!("exist {vars} {expr} where {f}: {g}");
        }
        format!("exist {{{vars}}} {expr} where {f}: {g}")
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
                ctx.assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.collected,
            );
            match other {
                TypeRef::Any(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::All(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Constraint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::EqOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::SubOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Bound(v)
                    if matches!(&v.kind, crate::types::type_bound::TypeBoundKind::Top) =>
                {
                    Ok(ThreeValuedLogic::True)
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
                ctx.assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.collected,
            );
            match other {
                TypeRef::Any(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::All(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Constraint(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Bound(v)
                    if matches!(&v.kind, crate::types::type_bound::TypeBoundKind::Top) =>
                {
                    Ok(ThreeValuedLogic::True)
                }
                _ => Ok(ThreeValuedLogic::False),
            }
        })
    }

    fn reduce(
        mut self,
        ctx: &mut super::ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        match self.0.modify(|(vars, p, f, g, source_info)| {
            let p = p.reduce(ctx)?;
            let f = f.reduce(ctx)?;
            let g = g.reduce(ctx)?;
            Ok((vars, p, f, g, source_info))
        })? {
            Some(()) => Ok(self.dispatch()),
            None => {
                let (vars, p, f, g, source_info) = self.0.as_ref();
                let p = p.clone().reduce(ctx)?;
                let f = f.clone().reduce(ctx)?;
                let g = g.clone().reduce(ctx)?;
                Ok(Self(ArcOpt::new((vars.clone(), p, f, g, source_info.clone()))).dispatch())
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
        self.0.4.as_ref()
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
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        let mut collector = Collector::new();
        let mut new_ctx = TypeCheckContext::new(
            ctx.assumptions,
            &mut collector,
            ctx.lhs_env,
            ctx.rhs_env,
            ctx.collected,
        );
        let result = test_true!(other.subof(self.expr().as_ref_dispatcher(), &mut new_ctx)?);
        let mut env = Environment::new(
            self.generic_variables().iter().map(|v| (v.clone(), EnvironmentVarState::FromPattern)),
        );
        let collected = collector.take_items().expect("Unable to take items from collector");
        for (k, v) in collected {
            env.bind(k, v, ctx.lhs_env, ctx.rhs_env)?
        }
        let mut bindings = SmallVec::<[(Arc<str>, Type<T>); 4]>::new();
        for (k, v) in env.type_vars() {
            match v {
                EnvironmentVarState::Bound(ty) => bindings.push((k.clone(), ty.clone())),
                _ => {
                    return Ok(ThreeValuedLogic::False); // 未绑定变量，失败
                }
            }
        }
        ctx.collected.push(env);
        let (f, g) = self.constraint();
        let check_result = f.subof(g.as_ref_dispatcher(), ctx);
        ctx.collected.pop();
        Ok(result & check_result?)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Constraint<T> {
    #[allow(clippy::type_complexity)]
    pub fn deconstruct(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<(ThreeValuedLogic, SmallVec<[(Arc<str>, Type<T>); 4]>), TypeError<Type<T>, T>> {
        let mut collector = Collector::new();
        let mut new_ctx = TypeCheckContext::new(
            ctx.assumptions,
            &mut collector,
            ctx.lhs_env,
            ctx.rhs_env,
            ctx.collected,
        );
        // 先检查主体类型，即 X where {...} 的 X 部分
        let result = other.check(self.expr().as_ref_dispatcher(), &mut new_ctx)?;
        if result == ThreeValuedLogic::False {
            return Ok((ThreeValuedLogic::False, SmallVec::new()));
        }
        // 收集变量绑定，并进行非线性约束检查
        let mut env = Environment::new(
            self.generic_variables().iter().map(|v| (v.clone(), EnvironmentVarState::FromPattern)),
        );
        let collected = collector.take_items().expect("Unable to take items from collector");
        for (k, v) in collected {
            if !env.bind_no_except(k, v, ctx.lhs_env, ctx.rhs_env)? {
                return Ok((ThreeValuedLogic::False, SmallVec::new()));
            }
        }
        let mut bindings: SmallVec<[(Arc<str>, Type<T>); 4]> =
            SmallVec::<[(Arc<str>, Type<T>); 4]>::new();
        // 确保所有变量都已绑定
        for (k, v) in env.type_vars() {
            match v {
                EnvironmentVarState::Bound(ty) => bindings.push((k.clone(), ty.clone())),
                _ => {
                    return Ok((ThreeValuedLogic::False, bindings)); // 未绑定变量，失败
                }
            }
        }
        // 先推入当前已绑定环境到 collected 栈
        ctx.collected.push(env);
        let (f, g) = self.constraint();
        let check_result = f.check(g.as_ref_dispatcher(), ctx);
        ctx.collected.pop();
        Ok((result & check_result?, bindings))
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Constraint<T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new<P, F, G>(
        generic_variables: impl IntoIterator<Item = String>,
        expr: P,
        constraint: (F, G),
        source_info: Option<Arc<SourceLocation>>,
    ) -> Type<T>
    where
        P: AsDispatcher<Type<T>, T>,
        F: AsDispatcher<Type<T>, T>,
        G: AsDispatcher<Type<T>, T>,
    {
        Self::new_constraint(generic_variables, expr, constraint, source_info).dispatch()
    }

    pub fn new_constraint<P, F, G>(
        generic_variables: impl IntoIterator<Item = String>,
        expr: P,
        constraint: (F, G),
        source_info: Option<Arc<SourceLocation>>,
    ) -> Self
    where
        P: AsDispatcher<Type<T>, T>,
        F: AsDispatcher<Type<T>, T>,
        G: AsDispatcher<Type<T>, T>,
    {
        Self(ArcOpt::new((
            Arc::from_iter(generic_variables),
            expr.into_dispatcher(),
            constraint.0.into_dispatcher(),
            constraint.1.into_dispatcher(),
            source_info,
        )))
    }

    pub fn expr(&self) -> &Type<T> {
        &self.0.1
    }

    pub fn constraint(&self) -> (&Type<T>, &Type<T>) {
        (&self.0.2, &self.0.3)
    }

    pub fn generic_variables(&self) -> &[String] {
        self.0.0.as_ref()
    }
}
