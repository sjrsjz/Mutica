use std::sync::{Arc, RwLock};

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::{
    test_true,
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, TaggedPtr, Type, TypeCheckContext, TypeError,
        TypeRef,
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
    is_nf: Arc<RwLock<ThreeValuedLogic>>, // 仅仅只是为了能可变的存储ThreeValuedLogic
                                          // 也许存在更好的方法
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for AnyOf<T> {
    fn clone(&self) -> Self {
        Self {
            types: self.types.clone(),
            source_info: self.source_info.clone(),
            is_nf: self.is_nf.clone(),
        }
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
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.rhs);
            match other {
                TypeRef::All(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::EqOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::SubOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                _ => {
                    let mut found = ThreeValuedLogic::True;
                    for sub in self.types.iter() {
                        found &= test_true!(sub.check(other, &mut inner_ctx)?);
                    }
                    Ok(found)
                }
            }
        })
    }

    fn subof(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut super::TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.rhs);
            match other {
                TypeRef::All(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Bound(v)
                    if matches!(&v.kind, crate::types::type_bound::TypeBoundKind::Top) =>
                {
                    Ok(ThreeValuedLogic::True)
                }
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
        Ok(Self::new(result, self.source_info.clone()))
    }

    fn invoke(self, _ctx: InvokeContext<Type<T>, T>) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.dispatch().into()))
    }

    fn is_normal_form(&self) -> ThreeValuedLogic {
        match self.is_nf.read() {
            Ok(v) => *v,
            Err(_) => ThreeValuedLogic::False,
        }
    }

    fn recalculate_normal_form(&self, cycle_detector: &mut FastCycleDetector<TaggedPtr<()>>) {
        let mut new_nf = ThreeValuedLogic::True;
        for sub in self.types.iter() {
            sub.recalculate_normal_form(cycle_detector);
            new_nf &= sub.is_normal_form();
        }
        if let Ok(mut nf_lock) = self.is_nf.write() {
            *nf_lock = new_nf;
        }
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        self.source_info.as_ref()
    }

    fn report_source_info(&self) -> crate::types::TypeReport {
        if let Some(loc) = &self.source_info {
            let span = loc.span().clone();
            let filepath = loc.source().filepath().to_string();
            ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                .with_message(format!("Type 'Any<...>' at {}", filepath))
                .with_label(
                    ariadne::Label::new((filepath, span)).with_message("Any<...> defined here"),
                )
                .finish()
        } else {
            ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                .with_message("Type 'Any<...>' has no source location")
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
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.rhs);
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
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.rhs);
            let mut matched = ThreeValuedLogic::False;
            for sub in self.types.iter() {
                matched |= other.subof(sub.as_ref_dispatcher(), &mut inner_ctx)?
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
    pub fn new<I, X>(types: I, source_info: Option<Arc<SourceLocation>>) -> Type<T>
    where
        I: IntoIterator<Item = X>,
        X: AsDispatcher<Type<T>, T>,
    {
        let collected: Vec<_> = types.into_iter().map(|t| t.into_dispatcher()).collect();
        match collected.len() {
            0 => panic!("Generalize requires at least one type"),
            1 => collected.into_iter().next().unwrap(),
            _ => Self {
                types: Arc::from(collected),
                source_info,
                is_nf: Arc::new(RwLock::new(ThreeValuedLogic::False)),
            }
            .dispatch(),
        }
    }

    pub fn types(&self) -> &[Type<T>] {
        &self.types
    }
}
