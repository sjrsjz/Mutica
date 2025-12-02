use std::sync::Arc;

use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeRef, CoinductiveTypeWithAny, GcAllocObject,
        InvokeContext, ReductionContext, Representable, Rootable, TaggedPtr, Type,
        TypeCheckContext, TypeError, TypeRef, construct::Construct, nature_number::NatureNumber,
        tuple::Tuple,
    },
    util::{
        cycle_detector::FastCycleDetector, source_info::SourceLocation,
        three_valued_logic::ThreeValuedLogic,
    },
};

/// 区间类型，表示一组不同长度元组的Any
pub struct Range<T: GcAllocObject<T, Inner = Type<T>>> {
    ty: Arc<(Type<T>, Option<Arc<SourceLocation>>)>,
    min: usize,
    delta: Option<usize>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Range<T> {
    fn clone(&self) -> Self {
        Self {
            ty: self.ty.clone(),
            min: self.min,
            delta: self.delta,
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Range<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        self.ty.0.collect(queue);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Range<T> {
    fn upgrade(&self, collected: &mut Vec<arc_gc::arc::GCArc<T>>) {
        self.ty.0.upgrade(collected);
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
                    let (min_s, max_s, ty_s) =
                        (self.min, self.delta.map(|d| self.min + d), &self.ty.0);
                    let (min_o, max_o, ty_o) = (v.min, v.delta.map(|d| v.min + d), &v.ty.0);

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
                _ => {
                    // 按 cons 处理，对于min=0的无穷range,需要推入假设集
                    // 即 T @ T @ T ... @ T 或者 T @ T @ T ... @ (rec tail: () | T @ tail)
                    // 即 T @ tail
                    if self.min == 0 && self.delta.is_none() {
                        if inner_ctx
                            .assumptions
                            .contains(&(self.tagged_ptr(), other.tagged_ptr()))
                        {
                            return Ok(ThreeValuedLogic::True);
                        }
                        inner_ctx
                            .assumptions
                            .push((self.tagged_ptr(), other.tagged_ptr()))
                    }
                    let result = match self.tail() {
                        Some(tail_type) => {
                            let cons =
                                Construct::new(self.head(), tail_type, self.source_info().cloned());
                            cons.check(other, &mut inner_ctx)
                        }
                        None => {
                            let cons = Construct::new(
                                self.head(),
                                Tuple::unit(),
                                self.source_info().cloned(),
                            );
                            cons.check(other, &mut inner_ctx)
                        }
                    };
                    if self.min == 0 && self.delta.is_none() {
                        inner_ctx.assumptions.pop();
                    }
                    result
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
                TypeRef::FixPoint(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Bound(v)
                    if matches!(&v.kind, crate::types::type_bound::TypeBoundKind::Top) =>
                {
                    Ok(ThreeValuedLogic::True)
                }

                TypeRef::Range(v) => {
                    let (min_s, max_s, ty_s) =
                        (self.min, self.delta.map(|d| self.min + d), &self.ty.0);
                    let (min_o, max_o, ty_o) = (v.min, v.delta.map(|d| v.min + d), &v.ty.0);

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

                _ => {
                    // 按 cons 处理，对于min=0的无穷range,需要推入假设集
                    // 即 T @ T @ T ... @ T 或者 T @ T @ T ... @ (rec tail: () | T @ tail)
                    // 即 T @ tail
                    if self.min == 0 && self.delta.is_none() {
                        if inner_ctx
                            .assumptions
                            .contains(&(self.tagged_ptr(), other.tagged_ptr()))
                        {
                            return Ok(ThreeValuedLogic::True);
                        }
                        inner_ctx
                            .assumptions
                            .push((self.tagged_ptr(), other.tagged_ptr()))
                    }
                    let result = match self.tail() {
                        Some(tail_type) => {
                            let cons =
                                Construct::new(self.head(), tail_type, self.source_info().cloned());
                            cons.subof(other, &mut inner_ctx)
                        }
                        None => {
                            let cons = Construct::new(
                                self.head(),
                                Tuple::unit(),
                                self.source_info().cloned(),
                            );
                            cons.subof(other, &mut inner_ctx)
                        }
                    };
                    if self.min == 0 && self.delta.is_none() {
                        inner_ctx.assumptions.pop();
                    }
                    result
                }
            }
        })
    }

    fn reduce(
        self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Ok(Self::new(
            self.min,
            self.delta,
            self.ty.0.clone().reduce(ctx)?,
            self.ty.1.clone(),
        ))
    }

    fn invoke(self, _ctx: InvokeContext<Type<T>, T>) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.into_dispatcher().into()))
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        self.ty.1.as_ref()
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

    fn tagged_ptr(&self) -> TaggedPtr<()> {
        match self.delta {
            Some(delta) => {
                TaggedPtr::new(self.ty() as *const _ as *const (), self.min).with_length(delta)
            }
            None => TaggedPtr::new(self.ty() as *const _ as *const (), self.min),
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
        match self.delta {
            Some(delta) => format!(
                "{}..={} <{}>",
                self.min,
                self.min + delta,
                self.ty.0.represent(path, depth + 1, max_depth)
            ),
            None => format!(
                "{}..<{}>",
                self.min,
                self.ty.0.represent(path, depth + 1, max_depth)
            ),
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
            v => Self {
                min,
                delta: v,
                ty: Arc::new((ty, source_info)),
            },
        }
        .into_dispatcher()
    }

    pub fn contains(&self, v: usize) -> bool {
        match self.delta {
            Some(delta) => v >= self.min && v <= self.min + delta,
            None => v >= self.min,
        }
    }

    pub fn min(&self) -> usize {
        self.min
    }

    pub fn delta(&self) -> Option<usize> {
        self.delta
    }

    pub fn ty(&self) -> &Type<T> {
        &self.ty.0
    }

    pub fn head(&self) -> &Type<T> {
        self.ty()
    }

    pub fn tail(&self) -> Option<Type<T>> {
        match self.delta {
            Some(delta) => {
                if self.min > 0 {
                    Some(
                        Self {
                            ty: self.ty.clone(),
                            min: self.min - 1,
                            delta: Some(delta),
                        }
                        .dispatch(),
                    )
                } else if delta > 1 {
                    Some(
                        Self {
                            ty: self.ty.clone(),
                            min: 0,
                            delta: Some(delta - 1),
                        }
                        .dispatch(),
                    )
                } else {
                    None
                }
            }
            None => Some(
                Self {
                    ty: self.ty.clone(),
                    min: if self.min > 0 { self.min - 1 } else { self.min },
                    delta: None,
                }
                .dispatch(),
            ),
        }
    }
}
