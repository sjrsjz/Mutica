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
    offset: usize,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Sequence<T> {
    fn clone(&self) -> Self {
        Self {
            prefix: self.prefix.clone(),
            tail: self.tail.clone(),
            source_info: self.source_info.clone(),
            offset: self.offset,
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
                    let mut self_seek = self.seek_prefix();
                    let mut other_seek = v.seek_prefix();
                    let mut all = ThreeValuedLogic::True;
                    while let (Some((cursor_self, self_rem)), Some((cursor_other, other_rem))) =
                        (self_seek, other_seek)
                    {
                        let ty_self = &self.prefix[cursor_self].0;
                        let ty_other = &v.prefix[cursor_other].0;
                        all &= test_true!(
                            ty_self.check(ty_other.as_ref_dispatcher(), &mut inner_ctx)?
                        );

                        if self_rem == other_rem {
                            self_seek = self.next_block(cursor_self);
                            other_seek = v.next_block(cursor_other);
                        } else if self_rem < other_rem {
                            // self块用完，other块未用完
                            other_seek = Some((cursor_other, other_rem - self_rem));
                            self_seek = self.next_block(cursor_self);
                        } else {
                            // other块用完，self块未用完
                            self_seek = Some((cursor_self, self_rem - other_rem));
                            other_seek = v.next_block(cursor_other);
                        }
                    }

                    match (self_seek, other_seek) {
                        (None, None) => match (&self.tail, &v.tail) {
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
                        (Some(seek), None) => {
                            match &v.tail {
                                SequenceTail::Nothing => Ok(ThreeValuedLogic::False),
                                SequenceTail::Repeat(ty_other) => {
                                    let mut cursor = Some(seek);
                                    while let Some((cursor_self, _)) = cursor {
                                        let ty_self = &self.prefix[cursor_self].0;
                                        all &=
                                            test_true!(ty_self.check(
                                                ty_other.as_ref_dispatcher(),
                                                &mut inner_ctx
                                            )?);

                                        cursor = self.next_block(cursor_self);
                                    }

                                    Ok(match &self.tail {
                                        SequenceTail::Nothing => all,
                                        SequenceTail::Repeat(ty_self) => {
                                            all & test_true!(ty_self.check(
                                                ty_other.as_ref_dispatcher(),
                                                &mut inner_ctx
                                            )?)
                                        }
                                        SequenceTail::Cons(ty_self) => {
                                            let viewed = Self {
                                                prefix: v.prefix.clone(),
                                                tail: v.tail.clone(),
                                                source_info: v.source_info.clone(),
                                                offset: v.prefix_len(),
                                            };
                                            all & test_true!(ty_self.check(
                                                viewed.as_ref_dispatcher(),
                                                &mut inner_ctx
                                            )?)
                                        }
                                    })
                                }
                                SequenceTail::Cons(cons) => {
                                    let offset = self.block_to_index(seek.0, seek.1);
                                    let viewed = Self {
                                        prefix: self.prefix.clone(),
                                        tail: self.tail.clone(),
                                        source_info: self.source_info.clone(),
                                        offset,
                                    };
                                    all &= test_true!(
                                        viewed.check(cons.as_ref_dispatcher(), &mut inner_ctx)?
                                    );
                                    return Ok(all);
                                }
                            }
                        }

                        (None, Some(seek)) => match &self.tail {
                            SequenceTail::Nothing => Ok(ThreeValuedLogic::False),
                            SequenceTail::Repeat(ty_self) => {
                                let mut cursor = Some(seek);
                                while let Some((cursor_other, _)) = cursor {
                                    let ty_other = &v.prefix[cursor_other].0;
                                    all &= test_true!(
                                        ty_self
                                            .check(ty_other.as_ref_dispatcher(), &mut inner_ctx)?
                                    );
                                    cursor = v.next_block(cursor_other);
                                }
                                Ok(match &v.tail {
                                    SequenceTail::Nothing => all,
                                    SequenceTail::Repeat(ty_other) => {
                                        all & test_true!(
                                            ty_self.check(
                                                ty_other.as_ref_dispatcher(),
                                                &mut inner_ctx
                                            )?
                                        )
                                    }
                                    SequenceTail::Cons(cons) => {
                                        let viewed = Self {
                                            prefix: self.prefix.clone(),
                                            tail: self.tail.clone(),
                                            source_info: self.source_info.clone(),
                                            offset: self.prefix_len(),
                                        };
                                        all & test_true!(
                                            viewed
                                                .check(cons.as_ref_dispatcher(), &mut inner_ctx)?
                                        )
                                    }
                                })
                            }
                            SequenceTail::Cons(cons) => {
                                let offset = v.block_to_index(seek.0, seek.1);
                                let viewed = Self {
                                    prefix: v.prefix.clone(),
                                    tail: v.tail.clone(),
                                    source_info: v.source_info.clone(),
                                    offset,
                                };
                                all &= test_true!(
                                    cons.check(viewed.as_ref_dispatcher(), &mut inner_ctx)?
                                );
                                return Ok(all);
                            }
                        },
                        _ => unreachable!(),
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

    fn tagged_ptr(&self) -> TaggedPtr<()> {
        // 使用offset作为tag
        // 由于使用prefix而没考虑tail部分，我们实际上假设了view操作不会改变结构本身，即不会因为tail部分的不同导致类型身份变化
        TaggedPtr::new(self.prefix() as *const _ as *const (), self.offset)
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
            offset: 0,
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
            offset: 0,
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
            offset: 0,
        }
        .dispatch()
    }

    // 计算序列的最小长度，如果有重复或不定长部分则返回Err(最小长度)
    pub fn len(&self) -> Result<usize, usize> {
        let mut total: usize = 0;
        for (_, count) in self.prefix.iter() {
            total += count.get();
        }
        total -= self.offset;
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
        total - self.offset
    }

    // 返回 (block_index, remaining_count_in_this_block)
    // 如果 offset 超出了 prefix，返回 None
    fn seek_prefix(&self) -> Option<(usize, usize)> {
        let mut pending_offset = self.offset;

        for (i, (_, count)) in self.prefix.iter().enumerate() {
            let cnt = count.get();
            if pending_offset < cnt {
                // 找到了起点：在第 i 个块，还剩 cnt - pending_offset 个元素
                return Some((i, cnt - pending_offset));
            }
            pending_offset -= cnt;
        }

        // Offset 超出了 Prefix，说明当前处于 Tail 区域
        None
    }

    fn next_block(&self, current_idx: usize) -> Option<(usize, usize)> {
        let next_idx = current_idx + 1;
        if next_idx < self.prefix.len() {
            Some((next_idx, self.prefix[next_idx].1.get()))
        } else {
            None
        }
    }

    fn block_to_index(&self, block: usize, offset_in_block: usize) -> usize {
        let mut index = 0;
        for i in 0..block {
            index += self.prefix[i].1.get();
        }
        index + (self.prefix[block].1.get() - offset_in_block)
    }

    pub fn get(&self, index: usize) -> Option<&Type<T>> {
        let mut idx = index + self.offset;
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
                    offset: self.offset,
                })
            }
            _ => None,
        }
    }

    pub fn view(&self, offset: usize) -> Option<Sequence<T>> {
        match &self.tail {
            SequenceTail::Nothing => {
                let prefix_len = self.prefix_len();
                if offset > prefix_len {
                    None
                } else {
                    Some(Self {
                        prefix: self.prefix.clone(),
                        tail: SequenceTail::Nothing,
                        source_info: self.source_info.clone(),
                        offset: self.offset + offset,
                    })
                }
            }
            SequenceTail::Repeat(tail) => {
                let prefix_len = self.prefix_len();
                if offset <= prefix_len {
                    Some(Self {
                        prefix: self.prefix.clone(),
                        tail: SequenceTail::Repeat(tail.clone()),
                        source_info: self.source_info.clone(),
                        offset: self.offset + offset,
                    })
                } else {
                    Some(Self {
                        prefix: self.prefix.clone(),
                        tail: SequenceTail::Repeat(tail.clone()),
                        source_info: self.source_info.clone(),
                        offset: self.offset + prefix_len, // 直接跳到tail部分
                    })
                }
            }
            SequenceTail::Cons(tail) => {
                let prefix_len = self.prefix_len();
                if offset <= prefix_len {
                    Some(Self {
                        prefix: self.prefix.clone(),
                        tail: SequenceTail::Cons(tail.clone()),
                        source_info: self.source_info.clone(),
                        offset: self.offset + offset,
                    })
                } else {
                    None // 无法确定长度，无法view到tail部分
                }
            }
        }
    }
}
