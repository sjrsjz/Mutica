use std::sync::Arc;

use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, TaggedPtr, Type, TypeCheckContext, TypeError,
        TypeRef, nature_number::NatureNumber,
    },
    util::{
        cycle_detector::FastCycleDetector, source_info::SourceLocation,
        three_valued_logic::ThreeValuedLogic,
    },
};

/// 区间类型，表示一组不同长度元组的Any
pub enum Range<T: GcAllocObject<T, Inner = Type<T>>> {
    #[allow(clippy::type_complexity)]
    Simple(Arc<(usize, usize, Type<T>, Option<Arc<SourceLocation>>)>), // [Min, Min + delta]
    GreaterThan(Arc<(usize, Type<T>, Option<Arc<SourceLocation>>)>), // [Min, inf)
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Range<T> {
    fn clone(&self) -> Self {
        match self {
            Self::Simple(v) => Self::Simple(v.clone()),
            Self::GreaterThan(v) => Self::GreaterThan(v.clone()),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Range<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        match self {
            Self::Simple(v) => v.2.collect(queue),
            Self::GreaterThan(v) => v.1.collect(queue),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Range<T> {
    fn upgrade(&self, collected: &mut Vec<arc_gc::arc::GCArc<T>>) {
        match self {
            Self::Simple(v) => v.2.upgrade(collected),
            Self::GreaterThan(v) => v.1.upgrade(collected),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for Range<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;
    fn as_ref_dispatcher(&self) -> Self::RefDispatcher<'_> {
        TypeRef::<T>::Range(self)
    }

    fn into_dispatcher(self) -> Type<T> {
        Type::<T>::Range(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for Range<T> {
    fn check(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.rhs);
            match other {
                TypeRef::All(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Any(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
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

                TypeRef::Range(v) => {
                    let (min_s, max_s, ty_s) = match self {
                        Self::Simple(r) => (r.0, Some(r.0 + r.1), &r.2),
                        Self::GreaterThan(r) => (r.0, None, &r.1),
                    };
                    let (min_o, max_o, ty_o) = match v {
                        Range::Simple(r) => (r.0, Some(r.0 + r.1), &r.2),
                        Range::GreaterThan(r) => (r.0, None, &r.1),
                    };

                    if min_s < min_o {
                        return Ok(ThreeValuedLogic::False);
                    }

                    if let Some(max_o) = max_o {
                        if let Some(max_s) = max_s {
                            if max_s > max_o {
                                return Ok(ThreeValuedLogic::False);
                            }
                        } else {
                            return Ok(ThreeValuedLogic::False);
                        }
                    }

                    ty_s.subof(ty_o.as_ref_dispatcher(), &mut inner_ctx)
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

                TypeRef::Range(v) => {
                    let (min_s, max_s, ty_s) = match self {
                        Self::Simple(r) => (r.0, Some(r.0 + r.1), &r.2),
                        Self::GreaterThan(r) => (r.0, None, &r.1),
                    };
                    let (min_o, max_o, ty_o) = match v {
                        Range::Simple(r) => (r.0, Some(r.0 + r.1), &r.2),
                        Range::GreaterThan(r) => (r.0, None, &r.1),
                    };

                    if min_s < min_o {
                        return Ok(ThreeValuedLogic::False);
                    }

                    if let Some(max_o) = max_o {
                        if let Some(max_s) = max_s {
                            if max_s > max_o {
                                return Ok(ThreeValuedLogic::False);
                            }
                        } else {
                            return Ok(ThreeValuedLogic::False);
                        }
                    }

                    ty_s.subof(ty_o.as_ref_dispatcher(), &mut inner_ctx)
                }

                _ => Ok(ThreeValuedLogic::False),
            }
        })
    }

    fn reduce(
        self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        match self {
            Self::Simple(v) => {
                let new_ty = v.2.clone().reduce(ctx)?;
                Ok(Self::Simple(Arc::new((v.0, v.1, new_ty, v.3.clone()))).dispatch())
            }
            Self::GreaterThan(v) => {
                let new_ty = v.1.clone().reduce(ctx)?;
                Ok(Self::GreaterThan(Arc::new((v.0, new_ty, v.2.clone()))).dispatch())
            }
        }
    }

    fn invoke(self, _ctx: InvokeContext<Type<T>, T>) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.into_dispatcher().into()))
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        match self {
            Self::Simple(v) => v.3.as_ref(),
            Self::GreaterThan(v) => v.2.as_ref(),
        }
    }

    fn report_source_info(&self) -> crate::types::TypeReport {
        if let Some(loc) = self.source_info() {
            let span = loc.span().clone();
            let filepath = loc.source().filepath().to_string();
            ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                .with_message(format!("Type 'Range' at {}", filepath))
                .with_label(
                    ariadne::Label::new((filepath, span)).with_message("Range type defined here"),
                )
                .finish()
        } else {
            ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                .with_message("Type 'Range' has no source location")
                .with_label(
                    ariadne::Label::new(("<unknown>".to_string(), 0..0))
                        .with_message("Location unknown"),
                )
                .finish()
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Range<T> {
    fn represent(
        &self,
        path: &mut FastCycleDetector<TaggedPtr<()>>,
        depth: usize,
        max_depth: usize,
    ) -> String {
        match self {
            Self::Simple(v) => {
                format!(
                    "{}..={}<{}>",
                    v.0,
                    v.0 + v.1,
                    v.2.represent(path, depth + 1, max_depth)
                )
            }
            Self::GreaterThan(v) => {
                format!("{}..<{}>", v.0, v.1.represent(path, depth + 1, max_depth))
            }
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Range<T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new<V: AsDispatcher<Type<T>, T>>(
        min: usize,
        delta: Option<usize>, // 实际上 delta = 0 的时候长度为 1，这是因为零长range没有意义
        ty: V,
        source_info: Option<Arc<SourceLocation>>,
    ) -> Type<T> {
        let ty = ty.into_dispatcher();
        match delta {
            Some(0usize) => return NatureNumber::new(min, ty, source_info),
            Some(delta) => Self::Simple(Arc::new((min, delta, ty, source_info))),
            None => Self::GreaterThan(Arc::new((min, ty, source_info))),
        }
        .into_dispatcher()
    }
}
