use std::{num::NonZero, sync::Arc};

use arc_gc::traceable::GCTraceable;

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

pub enum SequenceTail<T: GcAllocObject<T, Inner = Type<T>>> {
    Repeat(Arc<Type<T>>), // 任意长度
    Cons(Arc<Type<T>>),   // 余下的结构
    Nothing,              // 无剩余元素
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for SequenceTail<T> {
    fn clone(&self) -> Self {
        match self {
            SequenceTail::Repeat(ty) => SequenceTail::Repeat(ty.clone()),
            SequenceTail::Cons(ty) => SequenceTail::Cons(ty.clone()),
            SequenceTail::Nothing => SequenceTail::Nothing,
        }
    }
}

/// 区间类型，表示一组不同长度元组的Any
pub struct Sequence<T: GcAllocObject<T, Inner = Type<T>>> {
    prefix: Arc<Vec<(Type<T>, NonZero<usize>)>>,
    tail: SequenceTail<T>,
    source_info: Option<Arc<SourceLocation>>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Sequence<T> {
    fn clone(&self) -> Self {
        Self {
            prefix: self.prefix.clone(),
            tail: self.tail.clone(),
            source_info: self.source_info.clone(),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Sequence<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        for v in self.prefix.as_ref() {
            v.0.collect(queue);
        }

        match &self.tail {
            SequenceTail::Repeat(ty) => ty.collect(queue),
            SequenceTail::Cons(ty) => ty.collect(queue),
            SequenceTail::Nothing => {}
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Sequence<T> {
    fn upgrade(&self, collected: &mut Vec<arc_gc::arc::GCArc<T>>) {
        for v in self.prefix.as_ref() {
            v.0.upgrade(collected);
        }

        match &self.tail {
            SequenceTail::Repeat(ty) => ty.upgrade(collected),
            SequenceTail::Cons(ty) => ty.upgrade(collected),
            SequenceTail::Nothing => {}
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for Sequence<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;
    fn as_ref_dispatcher(&self) -> Self::RefDispatcher<'_> {
        TypeRef::<T>::Sequence(self)
    }

    fn into_dispatcher(self) -> Type<T> {
        Type::<T>::Sequence(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for Sequence<T> {
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
                TypeRef::Sequence(v) => {
                    let mut cursor_self = 0;
                    let mut cursor_other = 0;
                    let mut acc_len_self = 0;
                    let mut acc_len_other = 0;
                    let len_self = self.prefix_len();
                    let len_other = v.prefix_len();
                    let mut all = ThreeValuedLogic::True;
                    loop {
                        if cursor_self < self.prefix.len() && cursor_other < v.prefix.len() {
                            let (ty_self, count_self) = &self.prefix[cursor_self];
                            let (ty_other, count_other) = &v.prefix[cursor_other];

                            all &= test_true!(
                                ty_self.check(ty_other.as_ref_dispatcher(), &mut inner_ctx)?
                            );

                            if count_self == count_other {
                                cursor_self += 1;
                                cursor_other += 1;
                                acc_len_self += count_self.get();
                                acc_len_other += count_other.get();
                            } else if count_self.get() < count_other.get() {
                                cursor_self += 1;
                                acc_len_self += count_self.get();
                                let remaining = count_other.get() - count_self.get();
                                v.prefix[cursor_other].1 = NonZero::new(remaining).unwrap();
                            } else {
                                cursor_other += 1;
                                acc_len_other += count_other.get();
                                let remaining = count_self.get() - count_other.get();
                                self.prefix[cursor_self].1 = NonZero::new(remaining).unwrap();
                            }
                        } else {
                            break;
                        }
                    }

                    match (
                        self.prefix.len() == cursor_self,
                        v.prefix.len() == cursor_other,
                    ) {
                        (true, true) => match (&self.tail, &v.tail) {
                            (SequenceTail::Nothing, SequenceTail::Nothing) => Ok(all),
                            (SequenceTail::Repeat(ty_self), SequenceTail::Repeat(ty_other))
                            | (SequenceTail::Cons(ty_self), SequenceTail::Cons(ty_other)) => {
                                all &= test_true!(
                                    ty_self.check(ty_other.as_ref_dispatcher(), &mut inner_ctx)?
                                );
                                Ok(all)
                            }
                            _ => Ok(ThreeValuedLogic::False),
                        },
                        (true, false) => match &v.tail {
                            SequenceTail::Repeat(ty_other) => {
                                for i in cursor_other..v.prefix.len() {
                                    let (ty_other, count_other) = &v.prefix[i];
                                    for _ in 0..count_other.get() {
                                        all &=
                                            test_true!(self.tail.as_ref().check(
                                                ty_other.as_ref_dispatcher(),
                                                &mut inner_ctx
                                            )?);
                                    }
                                }
                                all &= match &self.tail {
                                    SequenceTail::Repeat(ty_self) => test_true!(
                                        ty_self
                                            .check(ty_other.as_ref_dispatcher(), &mut inner_ctx)?
                                    ),
                                    _ => ThreeValuedLogic::False,
                                };
                                Ok(all)
                            }
                            _ => Ok(ThreeValuedLogic::False),
                        },
                        _ => Ok(ThreeValuedLogic::False),
                    }
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
                TypeRef::FixPoint(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Bound(v)
                    if matches!(&v.kind, crate::types::type_bound::TypeBoundKind::Top) =>
                {
                    Ok(ThreeValuedLogic::True)
                }
                _ => Ok(ThreeValuedLogic::False),
            }
        })
    }

    fn reduce(
        self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        let mut elements: Vec<(Type<T>, NonZero<usize>)> = Vec::new();

        for (ty, count) in self.prefix.iter() {
            let reduced_ty = ty.clone().reduce(ctx)?;
            elements.push((reduced_ty, *count));
        }

        match &self.tail {
            SequenceTail::Repeat(ty) => {
                let reduced_ty = ty.as_ref().clone().reduce(ctx)?;
                Ok(Self::new_repeat(
                    elements,
                    reduced_ty,
                    self.source_info.clone(),
                ))
            }
            SequenceTail::Cons(ty) => {
                let reduced_ty = ty.as_ref().clone().reduce(ctx)?;
                Ok(Self::new_cons(
                    elements,
                    reduced_ty,
                    self.source_info.clone(),
                ))
            }
            SequenceTail::Nothing => Ok(Self::new_simple(elements, self.source_info.clone())),
        }
    }

    fn invoke(self, _ctx: InvokeContext<Type<T>, T>) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.into_dispatcher().into()))
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        self.source_info.as_ref()
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

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Sequence<T> {
    fn represent(
        &self,
        _path: &mut FastCycleDetector<TaggedPtr<()>>,
        _depth: usize,
        _max_depth: usize,
    ) -> String {
        "Sequence".to_string()
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Sequence<T> {
    pub fn new_repeat<U: AsDispatcher<Type<T>, T>, V: AsDispatcher<Type<T>, T>>(
        prefix: impl IntoIterator<Item = (U, NonZero<usize>)>,
        tail: V,
        source_info: Option<Arc<SourceLocation>>,
    ) -> Type<T> {
        let prefix_vec = prefix
            .into_iter()
            .map(|(ty, count)| (ty.into_dispatcher(), count))
            .collect();
        Self {
            prefix: Arc::new(prefix_vec),
            tail: SequenceTail::Repeat(Arc::new(tail.into_dispatcher())),
            source_info,
        }
        .dispatch()
    }

    pub fn new_cons<U: AsDispatcher<Type<T>, T>, V: AsDispatcher<Type<T>, T>>(
        prefix: impl IntoIterator<Item = (U, NonZero<usize>)>,
        tail: V,
        source_info: Option<Arc<SourceLocation>>,
    ) -> Type<T> {
        let prefix_vec = prefix
            .into_iter()
            .map(|(ty, count)| (ty.into_dispatcher(), count))
            .collect();
        Self {
            prefix: Arc::new(prefix_vec),
            tail: SequenceTail::Cons(Arc::new(tail.into_dispatcher())),
            source_info,
        }
        .dispatch()
    }

    pub fn new_simple<U: AsDispatcher<Type<T>, T>>(
        prefix: impl IntoIterator<Item = (U, NonZero<usize>)>,
        source_info: Option<Arc<SourceLocation>>,
    ) -> Type<T> {
        let prefix_vec = prefix
            .into_iter()
            .map(|(ty, count)| (ty.into_dispatcher(), count))
            .collect();
        Self {
            prefix: Arc::new(prefix_vec),
            tail: SequenceTail::Nothing,
            source_info,
        }
        .dispatch()
    }

    // 计算序列的最小长度，如果有重复或不定长部分则返回Err(最小长度)
    pub fn len(&self) -> Result<usize, usize> {
        let mut total: usize = 0;
        for (_, count) in self.prefix.iter() {
            total += count.get();
        }
        match &self.tail {
            SequenceTail::Repeat(_) | SequenceTail::Cons(_) => Err(total),
            SequenceTail::Nothing => Ok(total),
        }
    }

    pub fn prefix_len(&self) -> usize {
        let mut total: usize = 0;
        for (_, count) in self.prefix.iter() {
            total += count.get();
        }
        total
    }

    pub fn get(&self, index: usize) -> Option<&Type<T>> {
        let mut idx = index;
        for (ty, count) in self.prefix.iter() {
            let cnt = count.get();
            if idx < cnt {
                return Some(ty);
            } else {
                idx -= cnt;
            }
        }

        match &self.tail {
            SequenceTail::Repeat(ty) => Some(ty.as_ref()),
            SequenceTail::Cons(_) => {
                None // 无法确定长度，无法索引
            }
            SequenceTail::Nothing => None,
        }
    }

    pub fn prefix(&self) -> &[(Type<T>, NonZero<usize>)] {
        self.prefix.as_ref()
    }

    pub fn tail(&self) -> &SequenceTail<T> {
        &self.tail
    }

    pub fn concat(self, other: Sequence<T>) -> Option<Sequence<T>> {
        match (&self.tail, &other.tail) {
            (SequenceTail::Nothing, _) => {
                let mut new_prefix = Vec::new();
                new_prefix.extend_from_slice(self.prefix.as_ref());
                new_prefix.extend_from_slice(other.prefix.as_ref());
                Some(Sequence {
                    prefix: Arc::new(new_prefix),
                    tail: other.tail.clone(),
                    source_info: self.source_info.clone(),
                })
            }
            _ => None,
        }
    }
}
