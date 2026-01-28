use std::sync::Arc;

use arc_gc::traceable::GCTraceable;

use crate::{
    as_type,
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, CollectorExt, GcAllocObject,
        InvokeContext, ReductionContext, Representable, Rootable, TaggedPtr, Type,
        TypeCheckContext, TypeError, TypeRef, constraint::Constraint,
    },
    util::{
        arc_opt::ArcOpt, cycle_detector::FastCycleDetector, source_info::SourceLocation,
        three_valued_logic::ThreeValuedLogic,
    },
};

pub struct Lambda<T: GcAllocObject<T, Inner = Type<T>>> {
    patterns: ArcOpt<Vec<Constraint<T>>>,
    source_info: Option<Arc<SourceLocation>>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Lambda<T> {
    fn clone(&self) -> Self {
        Self { patterns: self.patterns.clone(), source_info: self.source_info.clone() }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Lambda<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        for pattern in self.patterns.iter() {
            pattern.collect(queue);
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Lambda<T> {
    fn upgrade(&self, collected: &mut Vec<arc_gc::arc::GCArc<T>>) {
        for pattern in self.patterns.iter() {
            pattern.upgrade(collected);
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for Lambda<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;
    fn as_ref_dispatcher(&self) -> Self::RefDispatcher<'_> {
        TypeRef::<T>::Lambda(self)
    }

    fn into_dispatcher(self) -> Type<T> {
        Type::<T>::Lambda(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for Lambda<T> {
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
                TypeRef::All(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Any(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Constraint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
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
                ctx.collected_bindings,
            );
            match other {
                TypeRef::Any(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::All(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Lambda(other) => {
                    // Lambda类型的子类型关系需要逐个模式进行检查
                    // 并且LHS的每个模式都必须是RHS某个模式的父类型（逆变）
                    // 不仅如此，LHS的某个模式按照顺序覆盖了RHS的多个模式也是允许的
                    // 取i,j分别表示LHS和RHS的模式索引
                    // 对每个i，从j开始尝试匹配RHS的模式，判定RHS_j是否是LHS_i的子类型
                    // 如果是，则j继续增加，直到RHS_j不再是LHS_i的子类型为止，此时i增加（但是j不增加），继续匹配下一个LHS模式
                    // 如果RHS的模式被全部匹配完毕，而LHS的模式还有剩余（或者刚好用完），则说明LHS是RHS的子类型
                    let lhs_patterns = self.patterns.as_ref();
                    let rhs_patterns = other.patterns.as_ref();

                    let mut i = 0usize;
                    let mut j = 0usize;
                    let mut result = ThreeValuedLogic::True;

                    while i < lhs_patterns.len() && j < rhs_patterns.len() {
                        let lhs = &lhs_patterns[i];
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
                                i += 1; // 保守起见，Unknown时也推进LHS
                            }
                        }
                    }

                    if j >= rhs_patterns.len() { Ok(result) } else { Ok(ThreeValuedLogic::False) }
                }
                _ => Ok(ThreeValuedLogic::False),
            }
        })
    }

    fn reduce(
        mut self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        match self.patterns.modify(|patterns| {
            let new_patterns = patterns
                .into_iter()
                .map(|pattern| pattern.reduce(ctx).map(|v| as_type!(v, Type::<T>::Constraint)))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(new_patterns)
        })? {
            Some(()) => Ok(self.dispatch()),
            None => {
                let new_patterns = self
                    .patterns
                    .iter()
                    .map(|p| p.clone().reduce(ctx).map(|v| as_type!(v, Type::<T>::Constraint)))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Self {
                    patterns: ArcOpt::new(new_patterns),
                    source_info: self.source_info.clone(),
                }
                .dispatch())
            }
        }
    }

    fn invoke(self, _ctx: InvokeContext<Type<T>, T>) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.dispatch().into()))
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        self.source_info.as_ref()
    }

    fn report_source_info(&self) -> crate::types::TypeReport {
        if let Some(loc) = &self.source_info {
            let span = loc.span().clone();
            let filepath = loc.source().filepath().to_string();
            ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                .with_message(format!("Lambda type at {}", filepath))
                .with_label(
                    ariadne::Label::new((filepath, span)).with_message("Lambda type defined here"),
                )
                .finish()
        } else {
            ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                .with_message("Lambda type has no source location")
                .with_label(
                    ariadne::Label::new(("<unknown>".to_string(), 0..0))
                        .with_message("Location unknown"),
                )
                .finish()
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Lambda<T> {
    fn represent(
        &self,
        path: &mut FastCycleDetector<TaggedPtr<()>>,
        depth: usize,
        max_depth: usize,
    ) -> String {
        if depth >= max_depth {
            return "...".to_string();
        }
        let mut repr = String::new();
        repr.push_str("lambda | ");
        let patterns: Vec<String> = self
            .patterns
            .iter()
            .map(|constraint| constraint.represent(path, depth + 1, max_depth))
            .collect();
        repr.push_str(&patterns.join(" | "));
        repr.push_str(" | panic");
        repr
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Lambda<T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(patterns: Vec<Constraint<T>>, source_info: Option<Arc<SourceLocation>>) -> Type<T> {
        Lambda { patterns: ArcOpt::new(patterns), source_info }.dispatch()
    }

    pub fn patterns(&self) -> &[Constraint<T>] {
        self.patterns.as_ref()
    }
}
