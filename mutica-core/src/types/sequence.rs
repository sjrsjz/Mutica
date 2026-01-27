use std::{num::NonZero, sync::Arc};

use arc_gc::traceable::GCTraceable;

use crate::{
    test_true,
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, CollectorExt, GcAllocObject, InvokeContext, ReductionContext, Representable, Rootable, TaggedPtr, Type, TypeCheckContext, TypeError, TypeRef, unify::EnvironmentView
    },
    util::{
        cycle_detector::FastCycleDetector, source_info::SourceLocation,
        three_valued_logic::ThreeValuedLogic,
    },
};

pub enum SequenceType<T: GcAllocObject<T, Inner = Type<T>>> {
    Repeat(Arc<[(Type<T>, NonZero<usize>)]>, Arc<Type<T>>), // 任意长度, usize仅仅用来做内存身份
    Cons(Arc<[(Type<T>, NonZero<usize>)]>, Arc<Type<T>>),   // 余下的结构
    NonEmptyTuple(Arc<[(Type<T>, NonZero<usize>)]>),        // 无剩余元素
    Unit,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for SequenceType<T> {
    fn clone(&self) -> Self {
        match self {
            SequenceType::Repeat(prefix, tail) => {
                SequenceType::Repeat(prefix.clone(), tail.clone())
            }
            SequenceType::Cons(prefix, tail) => SequenceType::Cons(prefix.clone(), tail.clone()),
            SequenceType::NonEmptyTuple(prefix) => SequenceType::NonEmptyTuple(prefix.clone()),
            SequenceType::Unit => SequenceType::Unit,
        }
    }
}

/// 区间类型，表示一组不同长度元组的Any
pub struct Sequence<T: GcAllocObject<T, Inner = Type<T>>> {
    ty: SequenceType<T>,
    source_info: Option<Arc<SourceLocation>>,
    offset: usize,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Sequence<T> {
    fn clone(&self) -> Self {
        Self { ty: self.ty.clone(), source_info: self.source_info.clone(), offset: self.offset }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Sequence<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        match &self.ty {
            SequenceType::Repeat(prefix, tail) => {
                for (ty, _) in prefix.as_ref() {
                    ty.collect(queue);
                }
                tail.collect(queue);
            }
            SequenceType::Cons(prefix, tail) => {
                for (ty, _) in prefix.as_ref() {
                    ty.collect(queue);
                }
                tail.collect(queue);
            }
            SequenceType::NonEmptyTuple(prefix) => {
                for (ty, _) in prefix.as_ref() {
                    ty.collect(queue);
                }
            }
            SequenceType::Unit => {}
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Sequence<T> {
    fn upgrade(&self, collected: &mut Vec<arc_gc::arc::GCArc<T>>) {
        match &self.ty {
            SequenceType::Repeat(prefix, tail) => {
                for (ty, _) in prefix.as_ref() {
                    ty.upgrade(collected);
                }
                tail.upgrade(collected);
            }
            SequenceType::Cons(prefix, tail) => {
                for (ty, _) in prefix.as_ref() {
                    ty.upgrade(collected);
                }
                tail.upgrade(collected);
            }
            SequenceType::NonEmptyTuple(prefix) => {
                for (ty, _) in prefix.as_ref() {
                    ty.upgrade(collected);
                }
            }
            SequenceType::Unit => {}
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
        ctx.pattern_collector.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.collected,
            );
            match other {
                TypeRef::All(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Any(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Constraint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::SubOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Sequence(v) => {
                    match (&self.ty, &v.ty) {
                        (SequenceType::Unit, SequenceType::Unit) => Ok(ThreeValuedLogic::True),
                        (SequenceType::Unit, SequenceType::Repeat(_, _)) => {
                            if v.is_prefix_empty() {
                                Ok(ThreeValuedLogic::True)
                            } else {
                                Ok(ThreeValuedLogic::False)
                            }
                        }
                        (SequenceType::Unit, SequenceType::Cons(_, cons)) => {
                            if v.is_prefix_empty() {
                                self.check(cons.as_ref_dispatcher(), &mut inner_ctx)
                            } else {
                                Ok(ThreeValuedLogic::False)
                            }
                        }
                        (SequenceType::NonEmptyTuple(_), SequenceType::NonEmptyTuple(_)) => {
                            let (all, self_seek, other_seek) =
                                self.check_prefix(v, &mut inner_ctx)?;
                            if let (None, None) = (self_seek, other_seek) {
                                Ok(all)
                            } else {
                                Ok(ThreeValuedLogic::False)
                            }
                        }
                        (SequenceType::NonEmptyTuple(_), SequenceType::Repeat(_, r_repeat)) => {
                            let (mut all, self_seek, other_seek) =
                                self.check_prefix(v, &mut inner_ctx)?;
                            test_true!(all);
                            match (self_seek, other_seek) {
                                (None, None) => Ok(all),
                                (mut seek @ Some(_), None) => {
                                    while let Some((cursor, _)) = seek {
                                        let ty_self = &self.physical_prefix()[cursor].0;
                                        all &=
                                            test_true!(ty_self.check(
                                                r_repeat.as_ref_dispatcher(),
                                                &mut inner_ctx
                                            )?);
                                        seek = self.next_block(cursor);
                                    }
                                    Ok(all)
                                }
                                // (_, Some(_)) 说明 other 还有前缀未匹配完，self已经没有元素了，必然失败
                                _ => Ok(ThreeValuedLogic::False),
                            }
                        }
                        (SequenceType::NonEmptyTuple(_), SequenceType::Cons(_, cons)) => {
                            let (mut all, self_seek, other_seek) =
                                self.check_prefix(v, &mut inner_ctx)?;
                            test_true!(all);
                            match (self_seek, other_seek) {
                                (None, None) => {
                                    let unit = Self::unit_seq(None);
                                    all &= test_true!(
                                        unit.check(cons.as_ref_dispatcher(), &mut inner_ctx)?
                                    );
                                    Ok(all)
                                }
                                (Some(seek), None) => {
                                    let viewed = Self {
                                        ty: self.ty.clone(),
                                        source_info: self.source_info.clone(),
                                        offset: self.block_to_index(seek.0, seek.1),
                                    };
                                    all &= test_true!(
                                        viewed.check(cons.as_ref_dispatcher(), &mut inner_ctx)?
                                    );
                                    Ok(all)
                                }
                                // (_, Some(_)) 说明 other 还有前缀未匹配完，self已经没有元素了，必然失败
                                _ => Ok(ThreeValuedLogic::False),
                            }
                        }

                        // check 要求LHS是单例类型，而Repeat不是单例类型，因此不能让Repeat去check Repeat否则会违反单例类型的要求
                        // (SequenceType::Repeat(_, l_repeat), SequenceType::Repeat(_, r_repeat)) => {
                        //     let (mut all, self_seek, other_seek) =
                        //         self.check_prefix(v, &mut inner_ctx)?;
                        //     test_true!(all);
                        //     all &= test_true!(
                        //         l_repeat.check(r_repeat.as_ref_dispatcher(), &mut inner_ctx)?
                        //     );
                        //     match (self_seek, other_seek) {
                        //         (None, None) => Ok(all),
                        //         (mut seek @ Some(_), None) => {
                        //             while let Some((cursor, _)) = seek {
                        //                 let ty_self = &self.physical_prefix()[cursor].0;
                        //                 all &=
                        //                     test_true!(ty_self.check(
                        //                         r_repeat.as_ref_dispatcher(),
                        //                         &mut inner_ctx
                        //                     )?);
                        //                 seek = self.next_block(cursor);
                        //             }
                        //             Ok(all)
                        //         }
                        //         (None, mut seek @ Some(_)) => {
                        //             while let Some((cursor, _)) = seek {
                        //                 let ty_other = &v.physical_prefix()[cursor].0;
                        //                 all &=
                        //                     test_true!(l_repeat.check(
                        //                         ty_other.as_ref_dispatcher(),
                        //                         &mut inner_ctx
                        //                     )?);
                        //                 seek = v.next_block(cursor);
                        //             }
                        //             Ok(all)
                        //         }
                        //         _ => unreachable!(),
                        //     }
                        // }

                        // Repeat可以去check Cons，因为Repeat的tail部分可以视为Cons，递归会详细处理，这样允许Cons尾部被设为Any来匹配无限长序列
                        // 这个是合理的，因为我们允许Any接受任意类型
                        (SequenceType::Repeat(_, _), SequenceType::Cons(_, r_cons)) => {
                            let (mut all, self_seek, other_seek) =
                                self.check_prefix(v, &mut inner_ctx)?;
                            test_true!(all);
                            match (self_seek, other_seek) {
                                (None, None) => {
                                    let viewed = Self {
                                        ty: self.ty.clone(),
                                        source_info: self.source_info.clone(),
                                        offset: self.physical_prefix_len(),
                                    };
                                    // 由于不消耗任何前缀元素就直接匹配剩余部分，可能会导致无限递归，因此需要做循环假设检测
                                    let pair = (viewed.tagged_ptr(), r_cons.tagged_ptr());
                                    if inner_ctx.assumptions.contains(&pair) {
                                        return Ok(ThreeValuedLogic::True);
                                    }
                                    inner_ctx.assumptions.push(pair);
                                    let result =
                                        viewed.check(r_cons.as_ref_dispatcher(), &mut inner_ctx);
                                    inner_ctx.assumptions.pop();
                                    all &= result?;
                                    Ok(all)
                                }
                                (Some(seek), None) => {
                                    let viewed = Self {
                                        ty: self.ty.clone(),
                                        source_info: self.source_info.clone(),
                                        offset: self.block_to_index(seek.0, seek.1),
                                    };
                                    all &= test_true!(
                                        viewed.check(r_cons.as_ref_dispatcher(), &mut inner_ctx)?
                                    );
                                    Ok(all)
                                }
                                (None, Some(_)) => {
                                    // 如果Cons还有前缀未匹配完，由于单例类型的要求，这里必须返回False
                                    // 这个是合理的，因为Repeat可以匹配任意长度（包括0长）的序列，而如果Cons还有前缀未匹配完，说明Repeat不可能匹配成功
                                    Ok(ThreeValuedLogic::False)
                                }
                                _ => unreachable!(),
                            }
                        }

                        (SequenceType::Cons(_, cons), SequenceType::Unit) => {
                            if self.is_prefix_empty() {
                                // 空的前缀才能匹配成功
                                let unit = Self::unit_seq(None);
                                cons.check(unit.as_ref_dispatcher(), &mut inner_ctx)
                            } else {
                                Ok(ThreeValuedLogic::False)
                            }
                        }

                        (SequenceType::Cons(_, cons), SequenceType::NonEmptyTuple(_)) => {
                            let (mut all, self_seek, other_seek) =
                                self.check_prefix(v, &mut inner_ctx)?;
                            test_true!(all);
                            match (self_seek, other_seek) {
                                (None, None) => {
                                    let unit = Self::unit_seq(None);
                                    all &= test_true!(
                                        cons.check(unit.as_ref_dispatcher(), &mut inner_ctx)?
                                    );
                                    Ok(all)
                                }
                                (None, Some(seek)) => {
                                    let viewed = Self {
                                        ty: v.ty.clone(),
                                        source_info: v.source_info.clone(),
                                        offset: v.block_to_index(seek.0, seek.1),
                                    };
                                    all &= test_true!(
                                        cons.check(viewed.as_ref_dispatcher(), &mut inner_ctx)?
                                    );
                                    Ok(all)
                                }
                                // (Some(_), _) 说明 self 还有前缀未匹配完，other已经没有元素了，必然失败
                                _ => Ok(ThreeValuedLogic::False),
                            }
                        }

                        (SequenceType::Cons(_, cons), SequenceType::Repeat(_, r_repeat)) => {
                            let (mut all, self_seek, other_seek) =
                                self.check_prefix(v, &mut inner_ctx)?;
                            test_true!(all);
                            match (self_seek, other_seek) {
                                (None, None) => {
                                    let viewed = Self {
                                        ty: v.ty.clone(),
                                        source_info: v.source_info.clone(),
                                        offset: v.physical_prefix_len(),
                                    };
                                    let pair = (cons.tagged_ptr(), viewed.tagged_ptr());
                                    if inner_ctx.assumptions.contains(&pair) {
                                        return Ok(ThreeValuedLogic::True);
                                    }
                                    inner_ctx.assumptions.push(pair);
                                    let result =
                                        cons.check(viewed.as_ref_dispatcher(), &mut inner_ctx);
                                    inner_ctx.assumptions.pop();
                                    all &= result?;
                                    Ok(all)
                                }
                                (None, Some(seek)) => {
                                    let viewed = Self {
                                        ty: v.ty.clone(),
                                        source_info: v.source_info.clone(),
                                        offset: v.block_to_index(seek.0, seek.1),
                                    };
                                    all &= test_true!(
                                        cons.check(viewed.as_ref_dispatcher(), &mut inner_ctx)?
                                    );
                                    Ok(all)
                                }
                                (seek @ Some(_), None) => {
                                    // self还有前缀未匹配完，other剩余部分是repeat，可以继续匹配
                                    let mut seek = seek;
                                    while let Some((cursor, _)) = seek {
                                        let ty_self = &self.physical_prefix()[cursor].0;
                                        all &=
                                            test_true!(ty_self.check(
                                                r_repeat.as_ref_dispatcher(),
                                                &mut inner_ctx
                                            )?);
                                        seek = self.next_block(cursor);
                                    }
                                    // 处理完前缀后，检查剩余部分
                                    let viewed = Self {
                                        ty: v.ty.clone(),
                                        source_info: v.source_info.clone(),
                                        offset: v.physical_prefix_len(),
                                    };
                                    let pair = (cons.tagged_ptr(), viewed.tagged_ptr());
                                    if inner_ctx.assumptions.contains(&pair) {
                                        return Ok(ThreeValuedLogic::True);
                                    }
                                    inner_ctx.assumptions.push(pair);
                                    let result =
                                        cons.check(viewed.as_ref_dispatcher(), &mut inner_ctx);
                                    inner_ctx.assumptions.pop();
                                    all &= result?;
                                    Ok(all)
                                }
                                _ => unreachable!(),
                            }
                        }

                        (SequenceType::Cons(_, l_cons), SequenceType::Cons(_, r_cons)) => {
                            let (mut all, self_seek, other_seek) =
                                self.check_prefix(v, &mut inner_ctx)?;
                            test_true!(all);
                            match (self_seek, other_seek) {
                                (None, None) => {
                                    all &= test_true!(
                                        l_cons.check(r_cons.as_ref_dispatcher(), &mut inner_ctx)?
                                    );
                                    Ok(all)
                                }
                                (None, Some(seek)) => {
                                    let viewed = Self {
                                        ty: v.ty.clone(),
                                        source_info: v.source_info.clone(),
                                        offset: v.block_to_index(seek.0, seek.1),
                                    };
                                    all &= test_true!(
                                        l_cons.check(viewed.as_ref_dispatcher(), &mut inner_ctx)?
                                    );
                                    Ok(all)
                                }
                                (Some(seek), None) => {
                                    let viewed = Self {
                                        ty: self.ty.clone(),
                                        source_info: self.source_info.clone(),
                                        offset: self.block_to_index(seek.0, seek.1),
                                    };
                                    all &= test_true!(
                                        viewed.check(r_cons.as_ref_dispatcher(), &mut inner_ctx)?
                                    );
                                    Ok(all)
                                }
                                _ => unreachable!(),
                            }
                        }
                        _ => Ok(ThreeValuedLogic::False),
                    }
                }

                // 由于试图证明无穷 Sequence 和 rec x: (() | T @ x) 之间的等价性是极其困难的，因此这里暂时不支持
                _ => Ok(ThreeValuedLogic::False),
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
                ctx.assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.collected,
            );
            match other {
                TypeRef::All(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Any(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Sequence(v) => {
                    match (&self.ty, &v.ty) {
                        (SequenceType::Unit, SequenceType::Unit) => Ok(ThreeValuedLogic::True),
                        (SequenceType::Unit, SequenceType::Repeat(_, _)) => {
                            if v.is_prefix_empty() {
                                Ok(ThreeValuedLogic::True)
                            } else {
                                Ok(ThreeValuedLogic::False)
                            }
                        }
                        (SequenceType::Unit, SequenceType::Cons(_, cons)) => {
                            if v.is_prefix_empty() {
                                self.subof(cons.as_ref_dispatcher(), &mut inner_ctx)
                            } else {
                                Ok(ThreeValuedLogic::False)
                            }
                        }
                        (SequenceType::NonEmptyTuple(_), SequenceType::NonEmptyTuple(_)) => {
                            let (all, self_seek, other_seek) =
                                self.subof_prefix(v, &mut inner_ctx)?;
                            if let (None, None) = (self_seek, other_seek) {
                                Ok(all)
                            } else {
                                Ok(ThreeValuedLogic::False)
                            }
                        }
                        (SequenceType::NonEmptyTuple(_), SequenceType::Repeat(_, r_repeat)) => {
                            let (mut all, self_seek, other_seek) =
                                self.subof_prefix(v, &mut inner_ctx)?;
                            test_true!(all);
                            match (self_seek, other_seek) {
                                (None, None) => Ok(all),
                                (mut seek @ Some(_), None) => {
                                    while let Some((cursor, _)) = seek {
                                        let ty_self = &self.physical_prefix()[cursor].0;
                                        all &=
                                            test_true!(ty_self.subof(
                                                r_repeat.as_ref_dispatcher(),
                                                &mut inner_ctx
                                            )?);
                                        seek = self.next_block(cursor);
                                    }
                                    Ok(all)
                                }
                                // (_, Some(_)) 说明 other 还有前缀未匹配完，self已经没有元素了，必然失败
                                _ => Ok(ThreeValuedLogic::False),
                            }
                        }
                        (SequenceType::NonEmptyTuple(_), SequenceType::Cons(_, cons)) => {
                            let (mut all, self_seek, other_seek) =
                                self.subof_prefix(v, &mut inner_ctx)?;
                            test_true!(all);
                            match (self_seek, other_seek) {
                                (None, None) => {
                                    let unit = Self::unit_seq(None);
                                    all &= test_true!(
                                        unit.subof(cons.as_ref_dispatcher(), &mut inner_ctx)?
                                    );
                                    Ok(all)
                                }
                                (Some(seek), None) => {
                                    let viewed = Self {
                                        ty: v.ty.clone(),
                                        source_info: v.source_info.clone(),
                                        offset: v.block_to_index(seek.0, seek.1),
                                    };
                                    all &= test_true!(
                                        viewed.subof(cons.as_ref_dispatcher(), &mut inner_ctx)?
                                    );
                                    Ok(all)
                                }
                                // (_, Some(_)) 说明 other 还有前缀未匹配完，self已经没有元素了，必然失败
                                _ => Ok(ThreeValuedLogic::False),
                            }
                        }

                        (SequenceType::Repeat(_, l_repeat), SequenceType::Repeat(_, r_repeat)) => {
                            let (mut all, self_seek, other_seek) =
                                self.subof_prefix(v, &mut inner_ctx)?;
                            test_true!(all);
                            all &= test_true!(
                                l_repeat.subof(r_repeat.as_ref_dispatcher(), &mut inner_ctx)?
                            );
                            match (self_seek, other_seek) {
                                (None, None) => Ok(all),
                                (mut seek @ Some(_), None) => {
                                    while let Some((cursor, _)) = seek {
                                        let ty_self = &self.physical_prefix()[cursor].0;
                                        all &=
                                            test_true!(ty_self.subof(
                                                r_repeat.as_ref_dispatcher(),
                                                &mut inner_ctx
                                            )?);
                                        seek = self.next_block(cursor);
                                    }
                                    Ok(all)
                                }
                                (None, mut seek @ Some(_)) => {
                                    while let Some((cursor, _)) = seek {
                                        let ty_other = &v.physical_prefix()[cursor].0;
                                        all &=
                                            test_true!(l_repeat.subof(
                                                ty_other.as_ref_dispatcher(),
                                                &mut inner_ctx
                                            )?);
                                        seek = v.next_block(cursor);
                                    }
                                    Ok(all)
                                }
                                _ => unreachable!(),
                            }
                        }

                        (SequenceType::Repeat(_, _), SequenceType::Cons(_, r_cons)) => {
                            let (mut all, self_seek, other_seek) =
                                self.subof_prefix(v, &mut inner_ctx)?;
                            test_true!(all);
                            match (self_seek, other_seek) {
                                (None, None) => {
                                    let viewed = Self {
                                        ty: self.ty.clone(),
                                        source_info: self.source_info.clone(),
                                        offset: self.physical_prefix_len(),
                                    };
                                    let pair = (viewed.tagged_ptr(), r_cons.tagged_ptr());
                                    if inner_ctx.assumptions.contains(&pair) {
                                        return Ok(ThreeValuedLogic::True);
                                    }
                                    inner_ctx.assumptions.push(pair);
                                    let result =
                                        viewed.subof(r_cons.as_ref_dispatcher(), &mut inner_ctx);
                                    inner_ctx.assumptions.pop();
                                    all &= result?;
                                    Ok(all)
                                }
                                (Some(seek), None) => {
                                    let viewed = Self {
                                        ty: self.ty.clone(),
                                        source_info: self.source_info.clone(),
                                        offset: self.block_to_index(seek.0, seek.1),
                                    };
                                    all &= test_true!(
                                        viewed.subof(r_cons.as_ref_dispatcher(), &mut inner_ctx)?
                                    );
                                    Ok(all)
                                }
                                (None, Some(_)) => {
                                    // 如果Cons还有前缀未匹配完，由于Repeat可以匹配任意长度（包括0长）的序列，这里显然不可能成功，因为Cons还有前缀未匹配完，它能能匹配的最短序列长度大于0
                                    Ok(ThreeValuedLogic::False)
                                }
                                _ => unreachable!(),
                            }
                        }

                        (SequenceType::Cons(_, cons), SequenceType::Unit) => {
                            if self.is_prefix_empty() {
                                // 空的前缀才能匹配成功
                                let unit = Self::unit_seq(None);
                                cons.subof(unit.as_ref_dispatcher(), &mut inner_ctx)
                            } else {
                                Ok(ThreeValuedLogic::False)
                            }
                        }

                        (SequenceType::Cons(_, cons), SequenceType::NonEmptyTuple(_)) => {
                            let (mut all, self_seek, other_seek) =
                                self.subof_prefix(v, &mut inner_ctx)?;
                            test_true!(all);
                            match (self_seek, other_seek) {
                                (None, None) => {
                                    let unit = Self::unit_seq(None);
                                    all &= test_true!(
                                        cons.subof(unit.as_ref_dispatcher(), &mut inner_ctx)?
                                    );
                                    Ok(all)
                                }
                                (None, Some(seek)) => {
                                    let viewed = Self {
                                        ty: v.ty.clone(),
                                        source_info: v.source_info.clone(),
                                        offset: v.block_to_index(seek.0, seek.1),
                                    };
                                    all &= test_true!(
                                        cons.subof(viewed.as_ref_dispatcher(), &mut inner_ctx)?
                                    );
                                    Ok(all)
                                }
                                // (Some(_), _) 说明 self 还有前缀未匹配完，other已经没有元素了，必然失败
                                _ => Ok(ThreeValuedLogic::False),
                            }
                        }

                        (SequenceType::Cons(_, cons), SequenceType::Repeat(_, r_repeat)) => {
                            let (mut all, self_seek, other_seek) =
                                self.subof_prefix(v, &mut inner_ctx)?;
                            test_true!(all);
                            match (self_seek, other_seek) {
                                (None, None) => {
                                    let viewed = Self {
                                        ty: v.ty.clone(),
                                        source_info: v.source_info.clone(),
                                        offset: v.physical_prefix_len(),
                                    };
                                    let pair = (cons.tagged_ptr(), viewed.tagged_ptr());
                                    if inner_ctx.assumptions.contains(&pair) {
                                        return Ok(ThreeValuedLogic::True);
                                    }
                                    inner_ctx.assumptions.push(pair);
                                    let result =
                                        cons.subof(viewed.as_ref_dispatcher(), &mut inner_ctx);
                                    inner_ctx.assumptions.pop();
                                    all &= result?;
                                    Ok(all)
                                }
                                (None, Some(seek)) => {
                                    let viewed = Self {
                                        ty: v.ty.clone(),
                                        source_info: v.source_info.clone(),
                                        offset: v.block_to_index(seek.0, seek.1),
                                    };
                                    all &= test_true!(
                                        cons.subof(viewed.as_ref_dispatcher(), &mut inner_ctx)?
                                    );
                                    Ok(all)
                                }
                                (seek @ Some(_), None) => {
                                    // self还有前缀未匹配完，other剩余部分是repeat，可以继续匹配
                                    let mut seek = seek;
                                    while let Some((cursor, _)) = seek {
                                        let ty_self = &self.physical_prefix()[cursor].0;
                                        all &=
                                            test_true!(ty_self.subof(
                                                r_repeat.as_ref_dispatcher(),
                                                &mut inner_ctx
                                            )?);
                                        seek = self.next_block(cursor);
                                    }
                                    // 处理完前缀后，检查剩余部分
                                    let viewed = Self {
                                        ty: v.ty.clone(),
                                        source_info: v.source_info.clone(),
                                        offset: v.physical_prefix_len(),
                                    };
                                    let pair = (cons.tagged_ptr(), viewed.tagged_ptr());
                                    if inner_ctx.assumptions.contains(&pair) {
                                        return Ok(ThreeValuedLogic::True);
                                    }
                                    inner_ctx.assumptions.push(pair);
                                    let result =
                                        cons.subof(viewed.as_ref_dispatcher(), &mut inner_ctx);
                                    inner_ctx.assumptions.pop();
                                    all &= result?;
                                    Ok(all)
                                }
                                _ => unreachable!(),
                            }
                        }

                        (SequenceType::Cons(_, l_cons), SequenceType::Cons(_, r_cons)) => {
                            let (mut all, self_seek, other_seek) =
                                self.subof_prefix(v, &mut inner_ctx)?;
                            test_true!(all);
                            match (self_seek, other_seek) {
                                (None, None) => {
                                    all &= test_true!(
                                        l_cons.subof(r_cons.as_ref_dispatcher(), &mut inner_ctx)?
                                    );
                                    Ok(all)
                                }
                                (None, Some(seek)) => {
                                    let viewed = Self {
                                        ty: v.ty.clone(),
                                        source_info: v.source_info.clone(),
                                        offset: v.block_to_index(seek.0, seek.1),
                                    };
                                    all &= test_true!(
                                        l_cons.subof(viewed.as_ref_dispatcher(), &mut inner_ctx)?
                                    );
                                    Ok(all)
                                }
                                (Some(seek), None) => {
                                    let viewed = Self {
                                        ty: self.ty.clone(),
                                        source_info: self.source_info.clone(),
                                        offset: self.block_to_index(seek.0, seek.1),
                                    };
                                    all &= test_true!(
                                        viewed.subof(r_cons.as_ref_dispatcher(), &mut inner_ctx)?
                                    );
                                    Ok(all)
                                }
                                _ => unreachable!(),
                            }
                        }
                        _ => Ok(ThreeValuedLogic::False),
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
        match self.ty {
            SequenceType::Unit => Ok(self.into_dispatcher()),
            SequenceType::NonEmptyTuple(prefix) => {
                let mut new_prefix = Vec::with_capacity(prefix.len());
                for (ty, count) in prefix.iter() {
                    let reduced_ty = ty.clone().reduce(ctx)?.into_dispatcher();
                    new_prefix.push((reduced_ty, *count));
                }
                Ok(Sequence::new_simple(new_prefix, self.source_info.clone()))
            }
            SequenceType::Repeat(prefix, tail) => {
                let mut new_prefix = Vec::with_capacity(prefix.len());
                for (ty, count) in prefix.iter() {
                    let reduced_ty = ty.clone().reduce(ctx)?.into_dispatcher();
                    new_prefix.push((reduced_ty, *count));
                }
                let reduced_tail = tail.as_ref().clone().reduce(ctx)?.into_dispatcher();
                Ok(Sequence::new_repeat(new_prefix, reduced_tail, self.source_info.clone()))
            }
            SequenceType::Cons(prefix, tail) => {
                let mut new_prefix = Vec::with_capacity(prefix.len());
                for (ty, count) in prefix.iter() {
                    let reduced_ty = ty.clone().reduce(ctx)?.into_dispatcher();
                    new_prefix.push((reduced_ty, *count));
                }
                let reduced_tail = tail.as_ref().clone().reduce(ctx)?.into_dispatcher();
                Ok(Sequence::new_cons(new_prefix, reduced_tail, self.source_info.clone()))
            }
        }
    }

    fn invoke(self, _ctx: InvokeContext<Type<T>, T>) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.into_dispatcher().into()))
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        self.source_info.as_ref()
    }

    fn tagged_ptr(&self) -> TaggedPtr<()> {
        if let SequenceType::Unit = self.ty {
            // Unit类型使用特殊的tagged ptr
            return TaggedPtr::unit();
        }
        // 使用offset作为tag
        // 由于使用prefix而没考虑tail部分，我们实际上假设了view操作不会改变结构本身，即不会因为tail部分的不同导致类型身份变化
        TaggedPtr::new(self.physical_prefix() as *const _ as *const (), self.offset)
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
        path: &mut FastCycleDetector<TaggedPtr<()>>,
        depth: usize,
        max_depth: usize,
    ) -> String {
        // 处理 offset，只显示视图中可见的部分
        if let Some((start_idx, start_rem)) = self.seek_prefix() {
            let prefix = self.physical_prefix();
            let mut parts = Vec::new();

            // 处理第一个被切断的块
            let first_ty = &prefix[start_idx].0;
            if start_rem == 1 {
                parts.push(first_ty.represent(path, depth + 1, max_depth));
            } else {
                parts.push(format!(
                    "{} @ {}",
                    start_rem,
                    first_ty.represent(path, depth + 1, max_depth)
                ));
            }

            // 处理后续完整的块
            for (ty, count) in prefix.iter().skip(start_idx + 1) {
                if count.get() == 1 {
                    parts.push(ty.represent(path, depth + 1, max_depth));
                } else {
                    parts.push(format!("{} @ {}", count, ty.represent(path, depth + 1, max_depth)));
                }
            }

            // 根据类型决定如何格式化
            match &self.ty {
                SequenceType::NonEmptyTuple(_) => {
                    format!("({})", parts.join(", ") + if parts.len() == 1 { "," } else { "" })
                }
                SequenceType::Repeat(_, tail) => {
                    format!(
                        "({}..{})",
                        parts.join(", "),
                        tail.represent(path, depth + 1, max_depth)
                    )
                }
                SequenceType::Cons(_, tail) => {
                    format!("({}~{})", parts.join(", "), tail.represent(path, depth + 1, max_depth))
                }
                SequenceType::Unit => unreachable!("seek_prefix returned Some for Unit"),
            }
        } else {
            // offset 已经超出 prefix，或者是 Unit
            match &self.ty {
                SequenceType::Unit => "Unit".to_string(),
                SequenceType::Repeat(_, tail) => {
                    // prefix 已经全部被跳过，只剩下 repeat 部分
                    format!("(..{})", tail.represent(path, depth + 1, max_depth))
                }
                SequenceType::Cons(_, tail) => {
                    // prefix 已经全部被跳过，只剩下 cons 的 tail
                    tail.represent(path, depth + 1, max_depth)
                }
                SequenceType::NonEmptyTuple(_) => {
                    // offset 超出了 tuple 的长度，这应该是空的
                    "Unit".to_string()
                }
            }
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Sequence<T> {
    pub fn new_repeat<U: AsDispatcher<Type<T>, T>, V: AsDispatcher<Type<T>, T>>(
        prefix: impl IntoIterator<Item = (U, NonZero<usize>)>,
        tail: V,
        source_info: Option<Arc<SourceLocation>>,
    ) -> Type<T> {
        let prefix_vec =
            prefix.into_iter().map(|(ty, count)| (ty.into_dispatcher(), count)).collect::<Vec<_>>();
        Self {
            ty: SequenceType::Repeat(Arc::from(prefix_vec), Arc::new(tail.into_dispatcher())),
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
        let prefix_vec =
            prefix.into_iter().map(|(ty, count)| (ty.into_dispatcher(), count)).collect::<Vec<_>>();
        Self {
            ty: SequenceType::Cons(Arc::from(prefix_vec), Arc::new(tail.into_dispatcher())),
            source_info,
            offset: 0,
        }
        .dispatch()
    }

    pub fn new_simple<U: AsDispatcher<Type<T>, T>>(
        prefix: impl IntoIterator<Item = (U, NonZero<usize>)>,
        source_info: Option<Arc<SourceLocation>>,
    ) -> Type<T> {
        let prefix_vec =
            prefix.into_iter().map(|(ty, count)| (ty.into_dispatcher(), count)).collect::<Vec<_>>();
        if prefix_vec.is_empty() {
            return Self::unit(source_info);
        }
        Self { ty: SequenceType::NonEmptyTuple(Arc::from(prefix_vec)), source_info, offset: 0 }
            .dispatch()
    }

    pub fn new_tuple<U: AsDispatcher<Type<T>, T>>(
        prefix: impl IntoIterator<Item = U>,
        source_info: Option<Arc<SourceLocation>>,
    ) -> Type<T> {
        let prefix_vec = prefix
            .into_iter()
            .map(|ty| (ty.into_dispatcher(), NonZero::new(1).unwrap()))
            .collect::<Vec<_>>();
        if prefix_vec.is_empty() {
            return Self::unit(source_info);
        }
        Self { ty: SequenceType::NonEmptyTuple(Arc::from(prefix_vec)), source_info, offset: 0 }
            .dispatch()
    }

    pub fn unit(source_info: Option<Arc<SourceLocation>>) -> Type<T> {
        Self { ty: SequenceType::Unit, source_info, offset: 0 }.dispatch()
    }

    pub fn unit_seq(source_info: Option<Arc<SourceLocation>>) -> Sequence<T> {
        Self { ty: SequenceType::Unit, source_info, offset: 0 }
    }

    pub fn nature_number<V: AsDispatcher<Type<T>, T>>(
        num: usize,
        ty: V,
        source_info: Option<Arc<SourceLocation>>,
    ) -> Type<T> {
        if num == 0 {
            Self::unit(source_info)
        } else {
            Self {
                ty: SequenceType::NonEmptyTuple(Arc::from(vec![(
                    ty.into_dispatcher(),
                    NonZero::new(num).unwrap(),
                )])),
                source_info,
                offset: 0,
            }
            .dispatch()
        }
    }

    #[allow(clippy::type_complexity)]
    pub fn check_prefix(
        &self,
        other: &Sequence<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<
        (ThreeValuedLogic, Option<(usize, usize)>, Option<(usize, usize)>),
        TypeError<Type<T>, T>,
    > {
        let mut self_seek = self.seek_prefix();
        let mut other_seek = other.seek_prefix();
        let mut all = ThreeValuedLogic::True;

        while let (Some((cursor_self, self_rem)), Some((cursor_other, other_rem))) =
            (self_seek, other_seek)
        {
            let ty_self = &self.physical_prefix()[cursor_self].0;
            let ty_other = &other.physical_prefix()[cursor_other].0;
            all &= ty_self.check(ty_other.as_ref_dispatcher(), ctx)?;
            if let ThreeValuedLogic::False = all {
                return Ok((ThreeValuedLogic::False, self_seek, other_seek));
            }

            if self_rem == other_rem {
                self_seek = self.next_block(cursor_self);
                other_seek = other.next_block(cursor_other);
            } else if self_rem < other_rem {
                // self块用完，other块未用完
                other_seek = Some((cursor_other, other_rem - self_rem));
                self_seek = self.next_block(cursor_self);
            } else {
                // other块用完，self块未用完
                self_seek = Some((cursor_self, self_rem - other_rem));
                other_seek = other.next_block(cursor_other);
            }
        }

        Ok((all, self_seek, other_seek))
    }

    #[allow(clippy::type_complexity)]
    pub fn subof_prefix(
        &self,
        other: &Sequence<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<
        (ThreeValuedLogic, Option<(usize, usize)>, Option<(usize, usize)>),
        TypeError<Type<T>, T>,
    > {
        let mut self_seek = self.seek_prefix();
        let mut other_seek = other.seek_prefix();
        let mut all = ThreeValuedLogic::True;

        while let (Some((cursor_self, self_rem)), Some((cursor_other, other_rem))) =
            (self_seek, other_seek)
        {
            let ty_self = &self.physical_prefix()[cursor_self].0;
            let ty_other = &other.physical_prefix()[cursor_other].0;
            all &= ty_self.subof(ty_other.as_ref_dispatcher(), ctx)?;
            if let ThreeValuedLogic::False = all {
                return Ok((ThreeValuedLogic::False, self_seek, other_seek));
            }

            if self_rem == other_rem {
                self_seek = self.next_block(cursor_self);
                other_seek = other.next_block(cursor_other);
            } else if self_rem < other_rem {
                // self块用完，other块未用完
                other_seek = Some((cursor_other, other_rem - self_rem));
                self_seek = self.next_block(cursor_self);
            } else {
                // other块用完，self块未用完
                self_seek = Some((cursor_self, self_rem - other_rem));
                other_seek = other.next_block(cursor_other);
            }
        }

        Ok((all, self_seek, other_seek))
    }

    // 返回 (block_index, remaining_count_in_this_block)
    // 如果 offset 超出了 prefix，返回 None
    fn seek_prefix(&self) -> Option<(usize, usize)> {
        let mut pending_offset = self.offset;

        for (i, (_, count)) in self.physical_prefix().iter().enumerate() {
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
        if next_idx < self.physical_prefix().len() {
            Some((next_idx, self.physical_prefix()[next_idx].1.get()))
        } else {
            None
        }
    }

    fn block_to_index(&self, block: usize, offset_in_block: usize) -> usize {
        let mut index = 0;
        for i in 0..block {
            index += self.physical_prefix()[i].1.get();
        }
        index + (self.physical_prefix()[block].1.get() - offset_in_block)
    }

    pub fn physical_prefix(&self) -> &[(Type<T>, NonZero<usize>)] {
        match &self.ty {
            SequenceType::Repeat(prefix, _) => prefix.as_ref(),
            SequenceType::Cons(prefix, _) => prefix.as_ref(),
            SequenceType::NonEmptyTuple(prefix) => prefix.as_ref(),
            SequenceType::Unit => &[],
        }
    }

    pub fn physical_prefix_len(&self) -> usize {
        let mut total: usize = 0;
        for (_, count) in self.physical_prefix().iter() {
            total += count.get();
        }
        total
    }

    pub fn offset(&self) -> usize {
        self.offset
    }

    pub fn is_prefix_empty(&self) -> bool {
        self.physical_prefix_len() == self.offset
    }

    pub fn get_prefix_value(&self, index: usize) -> Option<&Type<T>> {
        let index = index + self.offset;
        let mut total = 0;
        for (ty, count) in self.physical_prefix().iter() {
            let cnt = count.get();
            if total <= index && index < total + cnt {
                return Some(ty);
            }
            total += cnt;
        }
        // 如果没有找到，说明 index 超出了范围
        None
    }

    pub fn view(&self, offset: usize) -> Option<Sequence<T>> {
        match &self.ty {
            SequenceType::NonEmptyTuple(_) => {
                let len = self.physical_prefix_len();
                if offset + self.offset > len {
                    None
                } else if offset + self.offset == len {
                    // 视图正好是空的
                    Some(Sequence {
                        ty: SequenceType::Unit,
                        source_info: self.source_info.clone(),
                        offset: 0,
                    })
                } else {
                    // 视图仍然有元素
                    Some(Sequence {
                        ty: self.ty.clone(),
                        source_info: self.source_info.clone(),
                        offset: self.offset + offset,
                    })
                }
            }
            SequenceType::Cons(_, _) => {
                let len = self.physical_prefix_len();
                if offset + self.offset > len {
                    None
                } else {
                    Some(Sequence {
                        ty: self.ty.clone(),
                        source_info: self.source_info.clone(),
                        offset: self.offset + offset,
                    })
                }
            }
            SequenceType::Repeat(_, _) => {
                let len = self.physical_prefix_len();
                Some(Sequence {
                    ty: self.ty.clone(),
                    source_info: self.source_info.clone(),
                    offset: if self.offset + offset >= len { len } else { self.offset + offset },
                })
            }
            SequenceType::Unit => {
                if offset == 0 {
                    Some(Sequence {
                        ty: self.ty.clone(),
                        source_info: self.source_info.clone(),
                        offset: 0,
                    })
                } else {
                    None
                }
            }
        }
    }

    pub fn add<'a>(
        &'a self,
        other: &Sequence<T>,
        env: EnvironmentView<'a, Type<T>, T>,
    ) -> Result<Sequence<T>, TypeError<Type<T>, T>> {
        // 1. 检查 Self 是否为有限序列
        // 如果 Self 是无限的 (Repeat) 或未知的 (Cons)，则无法在物理层面上拼接后续内容
        match self.ty {
            SequenceType::Repeat(..) | SequenceType::Cons(..) => {
                return Err(TypeError::TypeMismatch(
                    (
                        self.as_ref_dispatcher().clone_data(),
                        "Finite Sequence (Tuple or Unit)".into(),
                    )
                        .into(),
                ));
            }
            _ => {} // Unit 和 NonEmptyTuple 是合法的左值
        }

        // 2. 准备新的前缀缓冲区
        // 预估容量：左边块数 + 右边块数 (最坏情况)
        let self_phys = self.physical_prefix();
        let other_phys = other.physical_prefix();
        let mut new_prefix: Vec<(Type<T>, NonZero<usize>)> =
            Vec::with_capacity(self_phys.len() + other_phys.len());

        // 3. 定义 RLE 推入与合并逻辑
        let mut push_rle = |ty: &Type<T>, count: usize| -> Result<(), TypeError<Type<T>, T>> {
            if count == 0 {
                return Ok(());
            }
            match new_prefix.last_mut() {
                // 类型相同 -> 合并计数
                Some((last_ty, last_count))
                    if matches!(
                        last_ty.equals(ty.as_ref_dispatcher(), env, env)?,
                        ThreeValuedLogic::True
                    ) =>
                {
                    let current = last_count.get();
                    let new_count = current.checked_add(count).ok_or_else(|| {
                        TypeError::RuntimeError(Arc::new(std::io::Error::new(
                            std::io::ErrorKind::InvalidData,
                            "Sequence length overflow during concatenation",
                        )))
                    })?;
                    // Safety: current >= 1, count >= 1 -> new_count >= 2
                    *last_count = unsafe { NonZero::new_unchecked(new_count) };
                }
                // 类型不同 -> 追加新块
                _ => {
                    // Safety: count > 0 check passed
                    new_prefix.push((ty.clone(), unsafe { NonZero::new_unchecked(count) }));
                }
            }
            Ok(())
        };

        // 4. 处理 Self (需要考虑 self.offset)
        // 即使 self 是 Unit，seek_prefix 也会正确返回 None (因为 offset 0 >= len 0 如果逻辑不对，或者 range 为空)
        // 这里的 seek_prefix 需要适配 SequenceType，建议实现一个通用的 seek_prefix 方法
        if let Some((idx, rem)) = self.seek_prefix() {
            // 4.1 第一个被切断的块
            push_rle(&self_phys[idx].0, rem)?;
            // 4.2 后续完整的块
            for (ty, count) in self_phys.iter().skip(idx + 1) {
                push_rle(ty, count.get())?;
            }
        }

        // 5. 处理 Other (需要考虑 other.offset)
        if let Some((idx, rem)) = other.seek_prefix() {
            // 5.1 第一个被切断的块
            push_rle(&other_phys[idx].0, rem)?;
            // 5.2 后续完整的块
            for (ty, count) in other_phys.iter().skip(idx + 1) {
                push_rle(ty, count.get())?;
            }
        }

        // 6. 构造新的 SequenceType
        // 尾部状态完全由 other 决定
        // 注意：我们必须把 new_prefix 包装进去
        let new_prefix_arc: Arc<[_]> = Arc::from(new_prefix);

        let new_ty = match &other.ty {
            // 如果 Other 是定长的，结果也是定长的
            SequenceType::Unit | SequenceType::NonEmptyTuple(_) => {
                if new_prefix_arc.is_empty() {
                    SequenceType::Unit
                } else {
                    SequenceType::NonEmptyTuple(new_prefix_arc)
                }
            }
            // 如果 Other 是变长的，结果继承其尾部定义
            SequenceType::Repeat(_, tail) => {
                // 注意：Other 的原 prefix 已经被我们（部分）合并进 new_prefix 了
                // 我们只需要把原来的 tail 逻辑部分拿过来即可
                SequenceType::Repeat(new_prefix_arc, tail.clone())
            }
            SequenceType::Cons(_, tail) => SequenceType::Cons(new_prefix_arc, tail.clone()),
        };

        // 7. 返回结果
        Ok(Sequence {
            ty: new_ty,
            source_info: self.source_info.clone(), // 或者合并 info
            offset: 0,                             // 物理拼接后，offset 归零
        })
    }

    pub fn is_tuple(&self) -> bool {
        matches!(self.ty, SequenceType::NonEmptyTuple(_) | SequenceType::Unit)
    }

    pub fn len(&self) -> usize {
        match &self.ty {
            SequenceType::Unit => 0,
            SequenceType::NonEmptyTuple(prefix) => {
                let mut total = 0;
                for (_, count) in prefix.iter() {
                    total += count.get();
                }
                total - self.offset
            }
            SequenceType::Repeat(_, _) => {
                panic!("Cannot get length of Repeat sequence");
            }
            SequenceType::Cons(_, _) => {
                panic!("Cannot get length of Cons sequence");
            }
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn is_unit(&self) -> bool {
        matches!(self.ty, SequenceType::Unit)
    }
}
