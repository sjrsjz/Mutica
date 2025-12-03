use std::sync::Arc;

use arc_gc::traceable::GCTraceable;

use crate::{
    test_true,
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, TaggedPtr, Type, TypeCheckContext, TypeError,
        TypeRef, tuple::Tuple,
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
    len: usize,
}
impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for NatureNumber<T> {
    fn clone(&self) -> Self {
        Self {
            ty: self.ty.clone(),
            len: self.len,
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
                    if self.len == v.len {
                        self_ty.check(other_ty.as_ref_dispatcher(), &mut inner_ctx)
                    } else {
                        Ok(ThreeValuedLogic::False)
                    }
                }
                TypeRef::Range(v) => {
                    if !v.contains(self.len) {
                        return Ok(ThreeValuedLogic::False);
                    }
                    self.ty().check(v.ty().as_ref_dispatcher(), &mut inner_ctx)
                }
                TypeRef::Tuple(v) => {
                    let (self_ty, _) = self.ty.as_ref();
                    if v.len() == self.len {
                        let mut matched = ThreeValuedLogic::True;
                        for sub in v.iter() {
                            matched &=
                                test_true!(self_ty.check(sub.as_ref_dispatcher(), &mut inner_ctx)?);
                        }
                        Ok(matched)
                    } else {
                        Ok(ThreeValuedLogic::False)
                    }
                }
                TypeRef::Construct(v) => match self.tail() {
                    Some(self_tail) => {
                        let (self_ty, _) = self.ty.as_ref();
                        let head = v.prefix();
                        let tail = v.tail();
                        let mut matched =
                            test_true!(self_ty.check(head.as_ref_dispatcher(), &mut inner_ctx)?);
                        matched &=
                            test_true!(self_tail.check(tail.as_ref_dispatcher(), &mut inner_ctx)?);
                        Ok(matched)
                    }
                    None => Ok(ThreeValuedLogic::False),
                },
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
                    if self.len == v.len {
                        self_ty.subof(v_ty.as_ref_dispatcher(), &mut inner_ctx)
                    } else {
                        Ok(ThreeValuedLogic::False)
                    }
                }
                TypeRef::Range(v) => {
                    if !v.contains(self.len) {
                        return Ok(ThreeValuedLogic::False);
                    }
                    self.ty().subof(v.ty().as_ref_dispatcher(), &mut inner_ctx)
                }
                TypeRef::Tuple(v) => {
                    let (self_ty, _) = self.ty.as_ref();
                    if v.len() == self.len {
                        let mut matched = ThreeValuedLogic::True;
                        for sub in v.iter() {
                            matched &=
                                test_true!(self_ty.subof(sub.as_ref_dispatcher(), &mut inner_ctx)?);
                        }
                        Ok(matched)
                    } else {
                        Ok(ThreeValuedLogic::False)
                    }
                }
                TypeRef::Construct(v) => match self.tail() {
                    Some(self_tail) => {
                        let (self_ty, _) = self.ty.as_ref();
                        let head = v.prefix();
                        let tail = v.tail();
                        let mut matched =
                            test_true!(self_ty.subof(head.as_ref_dispatcher(), &mut inner_ctx)?);
                        matched &=
                            test_true!(self_tail.subof(tail.as_ref_dispatcher(), &mut inner_ctx)?);
                        Ok(matched)
                    }
                    None => Ok(ThreeValuedLogic::False),
                },
                _ => Ok(ThreeValuedLogic::False),
            }
        })
    }

    fn reduce(
        self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Ok(Self::new(
            self.len,
            self.ty.0.clone().reduce(ctx)?,
            self.ty.1.clone(),
        ))
    }

    fn invoke(self, ctx: InvokeContext<Type<T>, T>) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Ok(Self::new(self.len, ctx.arg, self.ty.1.clone()))
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
                    self.len, filepath
                ))
                .with_label(
                    ariadne::Label::new((filepath, span))
                        .with_message(format!("Nature number value {} defined here", self.len)),
                )
                .finish()
        } else {
            ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                .with_message(format!(
                    "Nature number value {} has no source location",
                    self.len
                ))
                .with_label(
                    ariadne::Label::new(("<unknown>".to_string(), 0..0))
                        .with_message("Location unknown"),
                )
                .finish()
        }
    }

    fn tagged_ptr(&self) -> TaggedPtr<()> {
        TaggedPtr::new(self.ty() as *const _ as *const (), self.len)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for NatureNumber<T> {
    fn represent(
        &self,
        path: &mut FastCycleDetector<TaggedPtr<()>>,
        depth: usize,
        max_depth: usize,
    ) -> String {
        if let Type::Tuple(Tuple::Unit { .. }) = &self.ty.0 {
            return format!("{}", self.len);
        }
        format!(
            "{}<{}>",
            self.len,
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
            len: value,
        }
        .dispatch()
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn ty(&self) -> &Type<T> {
        &self.ty.0
    }

    pub fn head(&self) -> Option<&Type<T>> {
        match self.len {
            0 => None,
            _ => Some(&self.ty.0),
        }
    }

    pub fn tail(&self) -> Option<Type<T>> {
        self.pred()
    }

    pub fn succ(&self) -> Type<T> {
        Self {
            ty: self.ty.clone(),
            len: self.len + 1,
        }
        .dispatch()
    }

    pub fn pred(&self) -> Option<Type<T>> {
        if self.len == 0 {
            return None;
        }
        Some(
            Self {
                ty: self.ty.clone(),
                len: self.len - 1,
            }
            .dispatch(),
        )
    }

    pub fn view(&self, start: usize) -> Option<Type<T>> {
        if self.len < start {
            return None
        }
        Some(
            Self {
                ty: self.ty.clone(),
                len: self.len - start,
            }
            .dispatch(),
        )
    }
}
