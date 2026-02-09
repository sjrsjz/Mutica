use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};
use smallvec::{SmallVec, smallvec};

use crate::{
    test_true,
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, CollectorExt, GcAllocObject,
        InvokeContext, PatternCollector, ReductionContext, Representable, Rootable, TaggedPtr,
        Type, TypeCheckContext, TypeError, TypeOfContext, TypeRef,
        allof::AllOf,
        subof::SubOf,
        unify::{GenericBinding, capture_env::CaptureEnvList},
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
///   - 由于 `check` 是“实例判定”而非纯子类型语义，
///     当 LHS 为 `Any<...>` 时还要求它是**单例**（长度为 1）。
///     规范化保证等价元素已被吸收，因此无需额外等价性检验。
///     空 `Any<>` 视为非单例，判定为 False。
/// - Any<A₁, ..., Aₙ> : Any<B₁, ..., Bₙ>  当且仅当  ∀i. ∃j. Aᵢ : Bⱼ
pub struct AnyOf<U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    types: Arc<[U]>,
    rootless: bool,
    source_info: Option<Arc<SourceLocation>>,
    _phantom: std::marker::PhantomData<V>,
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Clone for AnyOf<U, V> {
    fn clone(&self) -> Self {
        Self {
            types: self.types.clone(),
            rootless: self.rootless,
            source_info: self.source_info.clone(),
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> GCTraceable<V> for AnyOf<U, V> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<V>>) {
        if self.rootless {
            return;
        }
        for sub in self.types.iter() {
            sub.collect(queue);
        }
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Rootable<V> for AnyOf<U, V> {
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

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for AnyOf<Type<T>, T> {
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

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for AnyOf<Type<T>, T> {
    fn check(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, super::TypeError<Type<T>, T>> {
        ctx.pattern_collector.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.coinductive_assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.bound_generic_variables,
            );
            match other {
                TypeRef::All(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Constraint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::SubOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                _ => {
                    if self.types.len() != 1 {
                        // Any<> 或 Any<...>（多元素）在实例判定中视为非单例
                        // 规范化保证等价元素已被吸收，因此无需额外等价性检验
                        return Ok(ThreeValuedLogic::False);
                    }

                    // check 不是子类型语义，而是验证某个类型是否是某个类型的实例
                    // 规范化已保证 Any<...> 内部不会出现等价元素，因此这里只需单元素判断
                    let sub = self.types.first().unwrap();
                    Ok(sub.check(other, &mut inner_ctx)?)
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
                ctx.coinductive_assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.bound_generic_variables,
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
        &self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        let mut result = smallvec::SmallVec::<[Type<T>; 8]>::new();
        for sub in self.types.iter() {
            result.push(sub.reduce(ctx)?);
        }
        Self::new(result, self.source_info.clone(), ctx.capture_env)
    }

    fn invoke(&self, _ctx: InvokeContext<Type<T>, T>) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.clone().dispatch().into()))
    }

    fn type_of(
        &self,
        ctx: &mut TypeOfContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        // 先检查是否为单例，如果不是单例则返回SubOf<>
        if self.types.len() != 1 {
            return Ok(SubOf::new(self.clone(), self.source_info.clone()));
        }
        let sub = self.types.first().unwrap();
        sub.type_of(ctx)
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

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveTypeWithAny<Type<T>, T>
    for AnyOf<Type<T>, T>
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
                            ctx.coinductive_assumptions,
                            PatternCollector::Subtyping(path),
                            ctx.lhs_env,
                            ctx.rhs_env,
                            ctx.bound_generic_variables,
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
                    ctx.coinductive_assumptions,
                    pattern_env,
                    ctx.lhs_env,
                    ctx.rhs_env,
                    ctx.bound_generic_variables,
                );
                for sub in self.types.iter() {
                    matched |= other.subof(sub.as_ref_dispatcher(), &mut inner_ctx)?
                }
            }
            Ok(matched)
        })
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for AnyOf<Type<T>, T> {
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
            return "Never".to_string();
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

impl<T: GcAllocObject<T, Inner = Type<T>>> AnyOf<Type<T>, T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new<I, X>(
        types: I,

        source_info: Option<Arc<SourceLocation>>,
        env: CaptureEnvList<Type<T>, T>,
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
        let empty_generic_binding = GenericBinding::wait_for_bind(None);
        let mut context = TypeCheckContext::new(
            &mut assumptions,
            PatternCollector::None,
            env,
            env,
            &empty_generic_binding,
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

        let kept_len = absorbed.iter().filter(|&&abs| !abs).count();
        let mut kept_iter = collected
            .into_iter()
            .enumerate()
            .filter_map(|(i, t)| if !absorbed[i] { Some(t) } else { None });

        let new_type = match kept_len {
            1 => kept_iter.next().unwrap(),
            _ => {
                let types = Arc::from_iter(kept_iter);
                let rootless = types.iter().all(|t| t.rootless());
                AnyOf { types, rootless, source_info, _phantom: std::marker::PhantomData }
            }
            .dispatch(),
        };
        Ok(new_type)
    }

    pub fn types(&self) -> &[Type<T>] {
        &self.types
    }

    pub fn never(source_info: Option<Arc<SourceLocation>>) -> Type<T> {
        AnyOf {
            types: Arc::from([]),
            rootless: true,
            source_info,
            _phantom: std::marker::PhantomData,
        }
        .dispatch()
    }
}
