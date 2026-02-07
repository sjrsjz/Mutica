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
        allof::AllOf,
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
/// - Any<A₁, ..., Aₙ> : Any<B₁, ..., Bₙ>  当且仅当  ∀i. ∃j. Aⱼ : Bᵢ
pub struct AnyOf<U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    types: ArcSlice<U, usize>,
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

                    // 首先验证所有类型都满足条件
                    for sub in self.types.iter() {
                        found &= test_true!(sub.check(other, &mut inner_ctx)?);
                    }

                    let mut assumptions = inner_ctx.coinductive_assumptions;
                    let mut collector = inner_ctx.pattern_collector;
                    let lhs_env = inner_ctx.lhs_env;
                    let rhs_env = inner_ctx.lhs_env; // 这里使用lhs_env是因为equals的语义只对LHS要求单例，左右环境相同
                    let mut bound_generic_layers = inner_ctx.bound_generic_variables;
                    let mut allocators = inner_ctx.allocators;

                    // 然后验证所有类型互相等价（手动实现equals：双向subof，第二次需要交换lhs_env和rhs_env）
                    for sub in self.types.iter().skip(1) {
                        // 第一次：sub <: first，使用正常的env顺序
                        let mut ctx1 = TypeCheckContext::new(
                            assumptions,
                            collector,
                            lhs_env,
                            rhs_env,
                            bound_generic_layers,
                            allocators,
                        );
                        unique &= test_true!(sub.subof(first.as_ref_dispatcher(), &mut ctx1)?);

                        // 第二次：first <: sub，需要交换lhs_env和rhs_env（equals的语义）
                        let mut ctx2 = TypeCheckContext::new(
                            ctx1.coinductive_assumptions,
                            ctx1.pattern_collector,
                            rhs_env,
                            lhs_env,
                            ctx1.bound_generic_variables,
                            ctx1.allocators,
                        );
                        unique &= test_true!(first.subof(sub.as_ref_dispatcher(), &mut ctx2)?);
                        assumptions = ctx2.coinductive_assumptions;
                        collector = ctx2.pattern_collector;
                        bound_generic_layers = ctx2.bound_generic_variables;
                        allocators = ctx2.allocators;
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
        Self::new(result, ctx.allocators, self.source_info.clone(), ctx.capture_env)
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
                ctx.allocators,
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
                            ctx.allocators,
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
                    ctx.allocators,
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
        allocators: &mut Allocators<Type<T>, T>,
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
            allocators,
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
                let types = allocators.v.alloc(kept_len, |_| kept_iter.next().unwrap());
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
            types: ArcSlice::empty(),
            rootless: true,
            source_info,
            _phantom: std::marker::PhantomData,
        }
        .dispatch()
    }
}
