use std::sync::Arc;

use arc_gc::traceable::GCTraceable;

use crate::{
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

/// 抽象自然数类型
/// u64 表示自然数的值，对应为定长元组/列表
pub struct NatureNumber<T: GcAllocObject<T, Inner = Type<T>>> {
    ty: Arc<(Type<T>, Option<Arc<SourceLocation>>)>,
    value: usize,
}
impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for NatureNumber<T> {
    fn clone(&self) -> Self {
        Self {
            ty: self.ty.clone(),
            value: self.value,
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for NatureNumber<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        self.ty.0.collect(queue);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for NatureNumber<T> {
    fn upgrade(&self, collected: &mut Vec<arc_gc::arc::GCArc<T>>) {
        self.ty.0.upgrade(collected);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for NatureNumber<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;
    fn as_ref_dispatcher(&self) -> Self::RefDispatcher<'_> {
        TypeRef::<T>::NatureNumber(self)
    }

    fn into_dispatcher(self) -> Type<T> {
        Type::<T>::NatureNumber(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for NatureNumber<T> {
    fn check(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.rhs);
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
                TypeRef::NatureNumber(v) => {
                    let (self_ty, _) = self.ty.as_ref();
                    let (other_ty, _) = v.ty.as_ref();
                    if self.value == v.value {
                        self_ty.check(other_ty.as_ref_dispatcher(), &mut inner_ctx)
                    } else {
                        Ok(ThreeValuedLogic::False)
                    }
                }
                TypeRef::Range(_) => Ok(ThreeValuedLogic::True),
                _ => Ok(ThreeValuedLogic::False),
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
                TypeRef::NatureNumber(v) => {
                    let (self_ty, _) = self.ty.as_ref();
                    let (v_ty, _) = v.ty.as_ref();
                    if self.value == v.value {
                        self_ty.subof(v_ty.as_ref_dispatcher(), &mut inner_ctx)
                    } else {
                        Ok(ThreeValuedLogic::False)
                    }
                }
                _ => Ok(ThreeValuedLogic::False),
            }
        })
    }

    fn reduce(
        self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Ok(Self::new(
            self.value,
            self.ty.0.clone().reduce(ctx)?,
            self.ty.1.clone(),
        ))
    }

    fn invoke(self, _ctx: InvokeContext<Type<T>, T>) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.dispatch().into()))
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        self.ty.1.as_ref()
    }

    fn report_source_info(&self) -> crate::types::TypeReport {
        if let Some(loc) = &self.ty.1 {
            let span = loc.span().clone();
            let filepath = loc.source().filepath().to_string();
            ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                .with_message(format!(
                    "Nature number value {} at {}",
                    self.value, filepath
                ))
                .with_label(
                    ariadne::Label::new((filepath, span))
                        .with_message(format!("Nature number value {} defined here", self.value)),
                )
                .finish()
        } else {
            ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                .with_message(format!(
                    "Nature number value {} has no source location",
                    self.value
                ))
                .with_label(
                    ariadne::Label::new(("<unknown>".to_string(), 0..0))
                        .with_message("Location unknown"),
                )
                .finish()
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for NatureNumber<T> {
    fn represent(
        &self,
        path: &mut FastCycleDetector<TaggedPtr<()>>,
        depth: usize,
        max_depth: usize,
    ) -> String {
        format!(
            "{}<{}>",
            self.value,
            self.ty.0.represent(path, depth + 1, max_depth)
        )
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> NatureNumber<T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new<V: AsDispatcher<Type<T>, T>>(
        value: usize,
        ty: V,
        source_info: Option<Arc<SourceLocation>>,
    ) -> Type<T> {
        NatureNumber {
            ty: Arc::new((ty.into_dispatcher(), source_info)),
            value,
        }
        .dispatch()
    }

    pub fn value(&self) -> usize {
        self.value
    }

    pub fn ty(&self) -> &Type<T> {
        &self.ty.0
    }

    pub fn succ(&self) -> Type<T> {
        Self {
            ty: self.ty.clone(),
            value: self.value + 1,
        }
        .dispatch()
    }

    pub fn pred(&self) -> Option<Type<T>> {
        if self.value == 0 {
            return None;
        }
        Some(
            Self {
                ty: self.ty.clone(),
                value: self.value - 1,
            }
            .dispatch(),
        )
    }
}
