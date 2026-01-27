use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};
use smallvec::{SmallVec, smallvec};

use crate::{
    test_true,
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, CollectorExt, GcAllocObject,
        InvokeContext, PatternCollector, ReductionContext, Representable, Rootable, TaggedPtr,
        Type, TypeCheckContext, TypeError, TypeRef,
        allof::AllOf,
        unify::{EnvironmentStack, EnvironmentView},
    },
    util::{
        cycle_detector::FastCycleDetector, source_info::SourceLocation,
        three_valued_logic::ThreeValuedLogic,
    },
};

use crate::types::CoinductiveTypeRef;

/// ### check语义（定义性质）
///
/// - **协变性质**：`S : Any<T₁, ..., Tₙ>` **定义为** `∃i. S : Tᵢ`
/// - **逆变性质**：`Any<T₁, ..., Tₙ> : U` **定义为** `∀i. Tᵢ : U`
/// - Any<A₁, ..., Aₙ> : Any<B₁, ..., Bₙ>  当且仅当  ∀i. ∃j. Aⱼ : Bᵢ
pub struct AnyOf<T: GcAllocObject<T, Inner = Type<T>>> {
    types: Arc<[Type<T>]>,
    source_info: Option<Arc<SourceLocation>>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for AnyOf<T> {
    fn clone(&self) -> Self {
        Self { types: self.types.clone(), source_info: self.source_info.clone() }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for AnyOf<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        for sub in self.types.iter() {
            sub.collect(queue);
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for AnyOf<T> {
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        for sub in self.types.iter() {
            sub.upgrade(collected);
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for AnyOf<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;

    fn into_dispatcher(self) -> Type<T> {
        Type::Any(self)
    }

    fn as_ref_dispatcher<'a>(&'a self) -> Self::RefDispatcher<'a> {
        TypeRef::Any(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for AnyOf<T> {
    fn check(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, super::TypeError<Type<T>, T>> {
        ctx.pattern_collector.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.instance_assumptions,
                ctx.subtype_assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.collected_bindings,
            );
            match other {
                TypeRef::All(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Constraint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::SubOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                _ => {
                    let mut found = ThreeValuedLogic::True;
                    if self.types.is_empty() {
                        // Any<> 属于非单例，直接False
                        return Ok(ThreeValuedLogic::False);
                    }
                    let first = self.types.first().unwrap();

                    // 验证LHS是单例类型
                    // 这是因为 check 不是子类型语义，而是验证某个类型是否是某个类型的实例
                    // 而实例一般要求LHS是单例类型，至于RHS为通配符的情况，可以通过Constraint的空约束来实现
                    let mut unique = ThreeValuedLogic::True;
                    for (i, sub) in self.types.iter().enumerate() {
                        found &= test_true!(sub.check(other, &mut inner_ctx)?);
                        if i > 0 {
                            unique &= test_true!(first.equals(
                                sub.as_ref_dispatcher(),
                                inner_ctx.lhs_env,
                                inner_ctx.lhs_env
                            )?);
                        }
                    }
                    Ok(found & unique)
                }
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
                ctx.subtype_assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.collected_bindings,
            );
            match other {
                TypeRef::All(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),

                _ => {
                    let mut found = ThreeValuedLogic::True;
                    for sub in self.types.iter() {
                        found &= test_true!(sub.subof(other, &mut inner_ctx)?);
                    }
                    Ok(found)
                }
            }
        })
    }

    fn reduce(
        self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        let mut result = smallvec::SmallVec::<[Type<T>; 8]>::new();
        for sub in self.types.iter() {
            result.push(sub.clone().reduce(ctx)?);
        }
        Self::new(result, self.source_info.clone(), ctx.capture_environment)
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
                .with_message(format!("Any<...> type at {}", filepath))
                .with_label(
                    ariadne::Label::new((filepath, span))
                        .with_message("Any<...> type defined here"),
                )
                .finish()
        } else {
            ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                .with_message("Any<...> type has no source location")
                .with_label(
                    ariadne::Label::new(("<unknown>".to_string(), 0..0))
                        .with_message("Location unknown"),
                )
                .finish()
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveTypeWithAny<Type<T>, T> for AnyOf<T> {
    #[stacksafe::stacksafe]
    fn accept(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, super::TypeError<Type<T>, T>> {
        ctx.pattern_collector.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.instance_assumptions,
                ctx.subtype_assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.collected_bindings,
            );
            let mut matched = ThreeValuedLogic::False;
            for sub in self.types.iter() {
                matched |= other.check(sub.as_ref_dispatcher(), &mut inner_ctx)?
            }
            Ok(matched)
        })
    }

    #[stacksafe::stacksafe]
    fn superof(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, super::TypeError<Type<T>, T>> {
        ctx.pattern_collector.collect(|pattern_env| {
            let mut matched = ThreeValuedLogic::False;
            if let PatternCollector::Subtyping(c) = pattern_env {
                let mut marker = c.mark_dynamic_oneof();

                for sub in self.types.iter() {
                    let result = marker.wrap(|path| {
                        let mut inner_ctx = TypeCheckContext::new(
                            ctx.instance_assumptions,
                            None,
                            PatternCollector::Subtyping(path),
                            ctx.lhs_env,
                            ctx.rhs_env,
                            ctx.collected_bindings,
                        );
                        let sub_result = other.subof(sub.as_ref_dispatcher(), &mut inner_ctx);
                        match sub_result {
                            Ok(ThreeValuedLogic::True) => (true, Ok(ThreeValuedLogic::True)),
                            Ok(ThreeValuedLogic::Unknown) => (true, Ok(ThreeValuedLogic::Unknown)),
                            Ok(val) => (false, Ok(val)),
                            Err(e) => (false, Err(e)),
                        }
                    })?;

                    matched |= result;
                }
            } else {
                let mut inner_ctx = TypeCheckContext::new(
                    ctx.instance_assumptions,
                    ctx.subtype_assumptions,
                    pattern_env,
                    ctx.lhs_env,
                    ctx.rhs_env,
                    ctx.collected_bindings,
                );
                for sub in self.types.iter() {
                    matched |= other.subof(sub.as_ref_dispatcher(), &mut inner_ctx)?
                }
            }
            Ok(matched)
        })
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for AnyOf<T> {
    fn represent(
        &self,
        path: &mut FastCycleDetector<TaggedPtr<()>>,
        depth: usize,
        max_depth: usize,
    ) -> String {
        if depth > max_depth {
            return "...".to_string();
        }
        let mut result = String::new();
        result.push_str("Any<");
        for (i, sub) in self.types.iter().enumerate() {
            if i > 0 {
                result.push_str(", ");
            }
            result.push_str(&sub.represent(path, depth + 1, max_depth));
        }
        result.push('>');
        result
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AnyOf<T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new<I, X>(
        types: I,
        source_info: Option<Arc<SourceLocation>>,
        env: EnvironmentView<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>>
    where
        I: IntoIterator<Item = X>,
        X: AsDispatcher<Type<T>, T>,
    {
        fn collect<T: GcAllocObject<T, Inner = Type<T>>>(
            collected: &mut SmallVec<[Type<T>; 8]>,
            path: &mut FastCycleDetector<TaggedPtr<()>>,
            is_unknown: &mut bool,
            x: Type<T>,
        ) -> Result<(), TypeError<Type<T>, T>> {
            if *is_unknown {
                return Ok(());
            }
            if x.map(path, |path, t| -> Result<bool, TypeError<Type<T>, T>> {
                Ok(match t {
                    TypeRef::Any(anyof) => {
                        for sub in anyof.types.iter() {
                            collect(collected, path, is_unknown, sub.clone())?;
                        }
                        false
                    }
                    TypeRef::All(allof) if allof.types().is_empty() => {
                        // 空 AllOf<> 为 Top 类型，与 Top 求 Any 结果为 Top，这个逻辑去掉后仍能在语义上成立，但为了规范化类型，保留此逻辑
                        *is_unknown = true;
                        false
                    }
                    _ => true,
                })
            })?
            .unwrap_or(Ok(true))?
            {
                collected.push(x);
            }
            Ok(())
        }
        let mut collected = SmallVec::new();

        let mut is_unknown = false;
        for t in types.into_iter() {
            if is_unknown {
                break;
            }
            collect(
                &mut collected,
                &mut FastCycleDetector::new(),
                &mut is_unknown,
                t.into_dispatcher(),
            )?;
        }
        if is_unknown {
            return Ok(AllOf::unknown(source_info));
        }

        let mut absorbed: SmallVec<[bool; 8]> = smallvec![false; collected.len()];
        let mut assumptions = smallvec![];
        let mut collected_pattern = EnvironmentStack::new();
        let mut context = TypeCheckContext::new(
            &mut assumptions,
            None,
            PatternCollector::None,
            env,
            env,
            &mut collected_pattern,
        );

        for i in 0..collected.len() {
            for j in 0..i {
                if absorbed[j] {
                    continue;
                }
                if let ThreeValuedLogic::True =
                    collected[i].subof(collected[j].as_ref_dispatcher(), &mut context)?
                {
                    absorbed[i] = true;
                    break;
                }
            }
            if absorbed[i] {
                continue;
            }
            for j in (i + 1)..collected.len() {
                if let ThreeValuedLogic::True =
                    collected[i].subof(collected[j].as_ref_dispatcher(), &mut context)?
                {
                    absorbed[i] = true;
                    break;
                }
            }
        }

        let mut result = Vec::new();
        for (i, t) in collected.into_iter().enumerate() {
            if !absorbed[i] {
                result.push(t);
            }
        }
        let new_type = match result.len() {
            1 => result.into_iter().next().unwrap(),
            _ => AnyOf { types: Arc::from(result), source_info }.dispatch(),
        };
        Ok(new_type)
    }

    pub fn types(&self) -> &[Type<T>] {
        &self.types
    }

    pub fn never(source_info: Option<Arc<SourceLocation>>) -> Type<T> {
        AnyOf { types: Arc::from([]), source_info }.dispatch()
    }
}
