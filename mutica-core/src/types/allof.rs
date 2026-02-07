use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};
use arena_arc::ArcSlice;
use smallvec::{SmallVec, smallvec};

use crate::{
    test_true,
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, CollectorExt, GcAllocObject,
        InvokeContext, PatternCollector, ReductionContext, Representable, Rootable, TaggedPtr,
        Type, TypeCheckContext, TypeError, TypeRef,
        allocator::Allocators,
        anyof::AnyOf,
        unify::{GenericBinding, capture_env::CaptureEnvList},
    },
    util::{
        cycle_detector::FastCycleDetector, source_info::SourceLocation,
        three_valued_logic::ThreeValuedLogic,
    },
};

use crate::types::CoinductiveTypeRef;

/// - **协变性质**：`S : All<T₁, ..., Tₙ>` **定义为** `∀i. S : Tᵢ`
/// - **逆变性质**：`All<T₁, ..., Tₙ> : U` **定义为** `∃i. Tᵢ : U`
/// - All<A₁, ..., Aₙ> : All<B₁, ..., Bₙ>  当且仅当  ∀j. ∃i. Aᵢ : Bⱼ
pub struct AllOf<U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    types: ArcSlice<U, usize>,
    rootless: bool,
    source_info: Option<Arc<SourceLocation>>,
    _phantom: std::marker::PhantomData<V>,
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Clone for AllOf<U, V> {
    fn clone(&self) -> Self {
        Self {
            types: self.types.clone(),
            source_info: self.source_info.clone(),
            rootless: self.rootless,
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> GCTraceable<V> for AllOf<U, V> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<V>>) {
        if self.rootless {
            return;
        }
        for sub in self.types.iter() {
            sub.collect(queue);
        }
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Rootable<V> for AllOf<U, V> {
    fn upgrade(&self, collected: &mut Vec<GCArc<V>>) {
        if self.rootless {
            return;
        }
        for sub in self.types.iter() {
            sub.upgrade(collected);
        }
    }

    fn rootless(&self) -> bool {
        self.rootless
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for AllOf<Type<T>, T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;

    fn into_dispatcher(self) -> Type<T> {
        Type::All(self)
    }

    fn as_ref_dispatcher<'a>(&'a self) -> Self::RefDispatcher<'a> {
        TypeRef::All(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for AllOf<Type<T>, T> {
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
                ctx.allocators,
            );
            match other {
                TypeRef::All(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Constraint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::SubOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                _ => {
                    let mut matched = ThreeValuedLogic::False;
                    for sub in self.types.iter() {
                        matched |= sub.check(other, &mut inner_ctx)?
                    }
                    Ok(matched)
                }
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
                ctx.allocators,
            );
            match other {
                TypeRef::All(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                _ => {
                    let mut matched = ThreeValuedLogic::False;
                    if let PatternCollector::Subtyping(c) = &mut inner_ctx.pattern_collector {
                        let mut marker = c.mark_dynamic_oneof();

                        for sub in self.types.iter() {
                            let result = marker.wrap(|path| {
                                let mut inner_ctx = TypeCheckContext::new(
                                    inner_ctx.coinductive_assumptions,
                                    PatternCollector::Subtyping(path),
                                    inner_ctx.lhs_env,
                                    inner_ctx.rhs_env,
                                    inner_ctx.bound_generic_variables,
                                    inner_ctx.allocators,
                                );
                                // result: subof 结果
                                let sub_result = sub.subof(other, &mut inner_ctx);
                                match sub_result {
                                    Ok(ThreeValuedLogic::True) => {
                                        (true, Ok(ThreeValuedLogic::True))
                                    }
                                    Ok(ThreeValuedLogic::Unknown) => {
                                        (true, Ok(ThreeValuedLogic::Unknown))
                                    }
                                    Ok(val) => (false, Ok(val)),
                                    Err(e) => (false, Err(e)),
                                }
                            })?;

                            matched |= result;
                        }
                    } else {
                        for sub in self.types.iter() {
                            matched |= sub.subof(other, &mut inner_ctx)?
                        }
                    }
                    Ok(matched)
                }
            }
        })
    }

    fn reduce(
        &self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        let mut result = smallvec::SmallVec::<[Type<T>; 8]>::new();
        for sub in self.types.iter() {
            result.push(sub.reduce(ctx)?);
        }
        Self::new(&result, ctx.allocators, self.source_info.clone(), ctx.capture_env)
    }

    fn invoke(&self, _ctx: InvokeContext<Type<T>, T>) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.clone().dispatch().into()))
    }
    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        self.source_info.as_ref()
    }

    fn report_source_info(&self) -> crate::types::TypeReport {
        if let Some(loc) = &self.source_info {
            let span = loc.span().clone();
            let filepath = loc.source().filepath().to_string();
            ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                .with_message(format!("All<...> type at {}", filepath))
                .with_label(
                    ariadne::Label::new((filepath, span))
                        .with_message("All<...> type defined here"),
                )
                .finish()
        } else {
            ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                .with_message("All<...> type has no source location")
                .with_label(
                    ariadne::Label::new(("<unknown>".to_string(), 0..0))
                        .with_message("Location unknown"),
                )
                .finish()
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveTypeWithAny<Type<T>, T>
    for AllOf<Type<T>, T>
{
    #[stacksafe::stacksafe]
    fn accept(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, super::TypeError<Type<T>, T>> {
        ctx.pattern_collector.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.coinductive_assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.bound_generic_variables,
                ctx.allocators,
            );
            let mut found = ThreeValuedLogic::True;
            for sub in self.types.iter() {
                found &= test_true!(other.check(sub.as_ref_dispatcher(), &mut inner_ctx)?)
            }
            Ok(found)
        })
    }

    #[stacksafe::stacksafe]
    fn superof(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, super::TypeError<Type<T>, T>> {
        ctx.pattern_collector.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.coinductive_assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.bound_generic_variables,
                ctx.allocators,
            );
            let mut found = ThreeValuedLogic::True;
            for sub in self.types.iter() {
                found &= test_true!(other.subof(sub.as_ref_dispatcher(), &mut inner_ctx)?)
            }
            Ok(found)
        })
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for AllOf<Type<T>, T> {
    fn represent(
        &self,
        path: &mut FastCycleDetector<TaggedPtr<()>>,
        depth: usize,
        max_depth: usize,
    ) -> String {
        if depth > max_depth {
            return "...".to_string();
        }
        if self.types.is_empty() {
            return "Unknown".to_string();
        }
        let mut result = String::new();
        result.push_str("All<");
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

impl<T: GcAllocObject<T, Inner = Type<T>>> AllOf<Type<T>, T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new<I, X>(
        types: I,
        allocators: &mut Allocators<Type<T>, T>,
        source_info: Option<Arc<SourceLocation>>,
        env: CaptureEnvList<'_, Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>>
    where
        I: IntoIterator<Item = X>,
        X: AsDispatcher<Type<T>, T>,
    {
        fn collect<T: GcAllocObject<T, Inner = Type<T>>>(
            collected: &mut SmallVec<[Type<T>; 8]>,
            path: &mut FastCycleDetector<TaggedPtr<()>>,
            is_never: &mut bool,
            x: Type<T>,
        ) -> Result<(), TypeError<Type<T>, T>> {
            if *is_never {
                return Ok(());
            }
            if x.map(path, |path, t| -> Result<bool, TypeError<Type<T>, T>> {
                Ok(match t {
                    TypeRef::All(allof) => {
                        for sub in allof.types.iter() {
                            collect(collected, path, is_never, sub.clone())?;
                        }
                        false
                    }
                    TypeRef::Any(anyof) if anyof.types().is_empty() => {
                        // 空 Any<> 表示 Bottom 类型，与 Bottom 求 All 结果仍为 Bottom，这个逻辑去掉后仍能在语义上成立，但为了规范化类型，保留此逻辑
                        *is_never = true;
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

        let mut is_never = false;
        for t in types.into_iter() {
            if is_never {
                break;
            }
            collect(
                &mut collected,
                &mut FastCycleDetector::new(),
                &mut is_never,
                t.into_dispatcher(),
            )?;
        }
        if is_never {
            return Ok(AnyOf::never(source_info));
        }

        let mut absorbed: SmallVec<[bool; 8]> = smallvec![false; collected.len()];
        let mut assumptions = smallvec![];
        let empty_generic_binding = GenericBinding::wait_for_bind(None);
        let mut context = TypeCheckContext::new(
            &mut assumptions,
            PatternCollector::None,
            env,
            env,
            &empty_generic_binding,
            allocators,
        );

        for i in 0..collected.len() {
            for j in 0..i {
                if absorbed[j] {
                    continue;
                }
                if let ThreeValuedLogic::True =
                    collected[j].subof(collected[i].as_ref_dispatcher(), &mut context)?
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
                    collected[j].subof(collected[i].as_ref_dispatcher(), &mut context)?
                {
                    absorbed[i] = true;
                    break;
                }
            }
        }

        let kept_len = absorbed.iter().filter(|&&abs| !abs).count();
        let mut kept_iter = collected
            .into_iter()
            .enumerate()
            .filter_map(|(i, t)| if !absorbed[i] { Some(t) } else { None });

        let new_type = match kept_len {
            1 => kept_iter.next().unwrap(),
            _ => {
                let types = allocators.v.alloc(kept_len, |_| kept_iter.next().unwrap());
                let rootless = types.iter().all(|t| t.rootless());
                AllOf { types, rootless, source_info, _phantom: std::marker::PhantomData }
            }
            .dispatch(),
        };
        Ok(new_type)
    }

    pub fn types(&self) -> &[Type<T>] {
        &self.types
    }

    pub fn unknown(source_info: Option<Arc<SourceLocation>>) -> Type<T> {
        AllOf {
            types: ArcSlice::empty(),
            rootless: true,
            source_info,
            _phantom: std::marker::PhantomData,
        }
        .dispatch()
    }
}
