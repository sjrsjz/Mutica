use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};
use smallvec::SmallVec;

use crate::{
    test_true,
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeRef, CoinductiveTypeWithAny, GcAllocObject,
        Representable, Rootable, TaggedPtr, Type, TypeCheckContext, TypeError, TypeRef,
        sequence::Sequence, unify::Environment,
    },
    util::{
        arc_opt::ArcOpt, collector::Collector, source_info::SourceLocation,
        three_valued_logic::ThreeValuedLogic,
    },
};

pub struct Constraint<T: GcAllocObject<T, Inner = Type<T>>>(
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
        self.0.1.iter().for_each(|(_, ty)| ty.collect(queue));
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Constraint<T> {
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        self.0.0.upgrade(collected);
        self.0.1.iter().for_each(|(_, ty)| ty.upgrade(collected));
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
        // T where { k1: v1, k2: v2, ... }
        let (value, constraints, _) = self.0.as_ref();
        let constraints_repr: Vec<String> = constraints
            .iter()
            .map(|(k, v)| format!("{}: {}", k, v.represent(path, depth + 1, max_depth)))
            .collect();
        format!(
            "{} where {{ {} }}",
            value.represent(path, depth + 1, max_depth),
            constraints_repr.join(", ")
        )
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
        match self.0.modify(|(expr, mut constraints, source_info)| {
            let new_expr = expr.reduce(ctx)?;
            // 原地获得所有权以避免多次克隆
            for (_, ty) in constraints.iter_mut() {
                let v = std::mem::replace(ty, Sequence::unit_seq(None).dispatch());
                let new_ty = v.reduce(ctx)?;
                *ty = new_ty;
            }
            Ok((new_expr, constraints, source_info))
        })? {
            Some(()) => Ok(self.dispatch()),
            None => {
                let (expr, constraints, source_info) = self.0.as_ref();
                let new_constraints: Vec<(Arc<str>, Type<T>)> =
                    constraints.iter().map(|(k, ty)| (k.clone(), ty.clone())).collect();
                let new_expr = expr.clone().reduce(ctx)?;
                Ok(Self(ArcOpt::new((new_expr, new_constraints, source_info.clone()))).dispatch())
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
        self.0.as_ref().2.as_ref()
    }

    fn report_source_info(&self) -> crate::types::TypeReport {
        if let Some(loc) = self.0.as_ref().2.as_ref() {
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
        let mut result = test_true!(other.subof(self.expr().as_ref_dispatcher(), &mut new_ctx)?);
        let mut env = Environment::new(self.iter_keys().cloned());
        let collected = collector.take_items().expect("Unable to take items from collector");
        for (k, v) in collected {
            env.bind(k, v, ctx.lhs_env, ctx.rhs_env)?
        }
        let mut bindings = SmallVec::<[(Arc<str>, Type<T>); 4]>::new();
        for (k, v) in env.type_vars() {
            match v {
                Some(ty) => bindings.push((k.clone(), ty.clone())),
                None => {
                    return Ok(ThreeValuedLogic::False); // 未绑定变量，失败
                }
            }
        }
        ctx.collected.push(env);
        for (var, checker) in self.constraints() {
            let bound = bindings
                .iter()
                .find(|(k, _)| k.as_ref() == var.as_ref())
                .map(|(_, v)| v)
                .expect("Variable not found in bindings");
            let check_result = bound.subof(checker.as_ref_dispatcher(), ctx)?;
            if check_result == ThreeValuedLogic::False {
                ctx.collected.pop();
                return Ok(ThreeValuedLogic::False);
            }
            result &= check_result;
        }
        ctx.collected.pop();
        Ok(result)
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
        let mut result = other.check(self.expr().as_ref_dispatcher(), &mut new_ctx)?;
        if result == ThreeValuedLogic::False {
            return Ok((ThreeValuedLogic::False, SmallVec::new()));
        }
        // 收集变量绑定，并进行非线性约束检查
        let mut env = Environment::new(self.iter_keys().cloned());
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
                Some(ty) => bindings.push((k.clone(), ty.clone())),
                None => {
                    return Ok((ThreeValuedLogic::False, bindings)); // 未绑定变量，失败
                }
            }
        }
        // 先推入当前已绑定环境到 collected 栈
        ctx.collected.push(env);
        // 检查所有约束
        for (var, checker) in self.constraints() {
            // 在 bindings 中找到对应绑定
            let bound = bindings
                .iter()
                .find(|(k, _)| k.as_ref() == var.as_ref())
                .map(|(_, v)| v)
                .expect("Variable not found in bindings");
            // 进行约束检查
            let check_result = bound.check(checker.as_ref_dispatcher(), ctx)?;
            // 如果有任何约束失败，则整体失败
            if check_result == ThreeValuedLogic::False {
                ctx.collected.pop();
                return Ok((ThreeValuedLogic::False, bindings));
            }
            result &= check_result;
        }
        ctx.collected.pop();
        Ok((result, bindings))
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Constraint<T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new<X, Y, Z>(
        expr: X,
        constraints: Y,
        source_info: Option<Arc<SourceLocation>>,
    ) -> Type<T>
    where
        X: AsDispatcher<Type<T>, T>,
        Y: IntoIterator<Item = (Arc<str>, Z)>,
        Z: AsDispatcher<Type<T>, T>,
    {
        Self::new_constraint(expr, constraints, source_info).dispatch()
    }

    pub fn new_constraint<X, Y, Z>(
        expr: X,
        constraints: Y,
        source_info: Option<Arc<SourceLocation>>,
    ) -> Self
    where
        X: AsDispatcher<Type<T>, T>,
        Y: IntoIterator<Item = (Arc<str>, Z)>,
        Z: AsDispatcher<Type<T>, T>,
    {
        Self(ArcOpt::new((
            expr.into_dispatcher(),
            constraints.into_iter().map(|(k, v)| (k, v.into_dispatcher())).collect(),
            source_info,
        )))
    }

    pub fn expr(&self) -> &Type<T> {
        &self.0.as_ref().0
    }

    pub fn constraints(&self) -> &Vec<(Arc<str>, Type<T>)> {
        &self.0.as_ref().1
    }

    pub fn iter_keys(&self) -> impl Iterator<Item = &Arc<str>> {
        self.0.as_ref().1.iter().map(|(k, _)| k)
    }
}
