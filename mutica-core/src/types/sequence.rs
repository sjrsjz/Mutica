use std::sync::Arc;

use arc_gc::traceable::GCTraceable;

use crate::{
    test_true,
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, CollectorExt, GcAllocObject,
        InvokeContext, ReductionContext, Representable, Rootable, TaggedPtr, Type,
        TypeCheckContext, TypeError, TypeRef,
    },
    util::{
        cycle_detector::FastCycleDetector, source_info::SourceLocation,
        three_valued_logic::ThreeValuedLogic,
    },
};

pub enum SequenceType<U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    Repeat(Arc<[U]>, Arc<U>), // 任意长度, usize仅仅用来做内存身份
    Cons(Arc<[U]>, Arc<U>),   // 余下的结构
    NonEmptyTuple(Arc<[U]>),  // 无剩余元素
    Unit,
    Phantom(std::marker::PhantomData<V>),
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Clone for SequenceType<U, V> {
    fn clone(&self) -> Self {
        match self {
            SequenceType::Repeat(prefix, tail) => {
                SequenceType::Repeat(prefix.clone(), tail.clone())
            }
            SequenceType::Cons(prefix, tail) => SequenceType::Cons(prefix.clone(), tail.clone()),
            SequenceType::NonEmptyTuple(prefix) => SequenceType::NonEmptyTuple(prefix.clone()),
            SequenceType::Unit => SequenceType::Unit,
            SequenceType::Phantom(_) => SequenceType::Phantom(std::marker::PhantomData),
        }
    }
}

/// 区间类型，表示一组不同长度元组的Any
pub struct Sequence<U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    ty: SequenceType<U, V>,
    rootless: bool,
    source_info: Option<Arc<SourceLocation>>,
    offset: usize,
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Clone for Sequence<U, V> {
    fn clone(&self) -> Self {
        Self {
            ty: self.ty.clone(),
            source_info: self.source_info.clone(),
            offset: self.offset,
            rootless: self.rootless,
        }
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> GCTraceable<V> for Sequence<U, V> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<V>>) {
        if self.rootless {
            return;
        }
        match &self.ty {
            SequenceType::Repeat(prefix, tail) => {
                for ty in prefix.as_ref() {
                    ty.collect(queue);
                }
                tail.collect(queue);
            }
            SequenceType::Cons(prefix, tail) => {
                for ty in prefix.as_ref() {
                    ty.collect(queue);
                }
                tail.collect(queue);
            }
            SequenceType::NonEmptyTuple(prefix) => {
                for ty in prefix.as_ref() {
                    ty.collect(queue);
                }
            }
            SequenceType::Unit => {}
            SequenceType::Phantom(_) => {}
        }
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Rootable<V> for Sequence<U, V> {
    fn upgrade(&self, collected: &mut Vec<arc_gc::arc::GCArc<V>>) {
        if self.rootless {
            return;
        }
        match &self.ty {
            SequenceType::Repeat(prefix, tail) => {
                for ty in prefix.as_ref() {
                    ty.upgrade(collected);
                }
                tail.upgrade(collected);
            }
            SequenceType::Cons(prefix, tail) => {
                for ty in prefix.as_ref() {
                    ty.upgrade(collected);
                }
                tail.upgrade(collected);
            }
            SequenceType::NonEmptyTuple(prefix) => {
                for ty in prefix.as_ref() {
                    ty.upgrade(collected);
                }
            }
            SequenceType::Unit => {}
            SequenceType::Phantom(_) => {}
        }
    }

    fn rootless(&self) -> bool {
        self.rootless
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for Sequence<Type<T>, T> {
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

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for Sequence<Type<T>, T> {
    fn check(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
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
                        (SequenceType::Unit, SequenceType::Cons(_, _)) => {
                            if v.is_prefix_empty() {
                                Err(TypeError::TypeMayCauseCircularReasoning(
                                    v.clone().into_dispatcher().into(),
                                ))
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
                                    while let Some(cursor) = seek {
                                        let ty_self = &self.physical_prefix()[cursor];
                                        all &=
                                            test_true!(ty_self.check(
                                                r_repeat.as_ref_dispatcher(),
                                                &mut inner_ctx
                                            )?);
                                        seek = self.next_index(cursor);
                                    }
                                    Ok(all)
                                }
                                // (_, Some(_)) 说明 other 还有前缀未匹配完，self已经没有元素了，必然失败
                                _ => Ok(ThreeValuedLogic::False),
                            }
                        }
                        (SequenceType::NonEmptyTuple(_), SequenceType::Cons(_, cons)) => {
                            if v.is_prefix_empty() {
                                return Err(TypeError::TypeMayCauseCircularReasoning(
                                    v.clone().into_dispatcher().into(),
                                ));
                            }
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
                                        offset: seek,
                                        rootless: self.rootless,
                                    };
                                    let pair = (viewed.tagged_ptr(), cons.tagged_ptr());
                                    if inner_ctx.coinductive_assumptions.contains(&pair) {
                                        return Ok(ThreeValuedLogic::True);
                                    }
                                    inner_ctx.coinductive_assumptions.push(pair);
                                    let result =
                                        viewed.check(cons.as_ref_dispatcher(), &mut inner_ctx);
                                    inner_ctx.coinductive_assumptions.pop();
                                    all &= result?;
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
                            if v.is_prefix_empty() {
                                return Err(TypeError::TypeMayCauseCircularReasoning(
                                    v.clone().into_dispatcher().into(),
                                ));
                            }
                            let (mut all, self_seek, other_seek) =
                                self.check_prefix(v, &mut inner_ctx)?;
                            test_true!(all);
                            match (self_seek, other_seek) {
                                (None, None) => {
                                    let viewed = Self {
                                        ty: self.ty.clone(),
                                        source_info: self.source_info.clone(),
                                        offset: self.physical_prefix_len(),
                                        rootless: self.rootless,
                                    };
                                    // 由于不消耗任何前缀元素就直接匹配剩余部分，可能会导致无限递归，因此需要做循环假设检测
                                    let pair = (viewed.tagged_ptr(), r_cons.tagged_ptr());
                                    if inner_ctx.coinductive_assumptions.contains(&pair) {
                                        return Ok(ThreeValuedLogic::True);
                                    }
                                    inner_ctx.coinductive_assumptions.push(pair);
                                    let result =
                                        viewed.check(r_cons.as_ref_dispatcher(), &mut inner_ctx);
                                    inner_ctx.coinductive_assumptions.pop();
                                    all &= result?;
                                    Ok(all)
                                }
                                (Some(seek), None) => {
                                    let viewed = Self {
                                        ty: self.ty.clone(),
                                        source_info: self.source_info.clone(),
                                        offset: seek,
                                        rootless: self.rootless,
                                    };
                                    let pair = (viewed.tagged_ptr(), r_cons.tagged_ptr());
                                    if inner_ctx.coinductive_assumptions.contains(&pair) {
                                        return Ok(ThreeValuedLogic::True);
                                    }
                                    inner_ctx.coinductive_assumptions.push(pair);
                                    let result =
                                        viewed.check(r_cons.as_ref_dispatcher(), &mut inner_ctx);
                                    inner_ctx.coinductive_assumptions.pop();
                                    all &= result?;
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

                        (SequenceType::Cons(_, _), SequenceType::Unit) => {
                            if self.is_prefix_empty() {
                                Err(TypeError::TypeMayCauseCircularReasoning(
                                    self.clone().into_dispatcher().into(),
                                ))
                            } else {
                                Ok(ThreeValuedLogic::False)
                            }
                        }

                        (SequenceType::Cons(_, cons), SequenceType::NonEmptyTuple(_)) => {
                            if self.is_prefix_empty() {
                                return Err(TypeError::TypeMayCauseCircularReasoning(
                                    self.clone().into_dispatcher().into(),
                                ));
                            }
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
                                        offset: seek,
                                        rootless: v.rootless,
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
                            if self.is_prefix_empty() {
                                return Err(TypeError::TypeMayCauseCircularReasoning(
                                    self.clone().into_dispatcher().into(),
                                ));
                            }
                            let (mut all, self_seek, other_seek) =
                                self.check_prefix(v, &mut inner_ctx)?;
                            test_true!(all);
                            match (self_seek, other_seek) {
                                (None, None) => {
                                    let viewed = Self {
                                        ty: v.ty.clone(),
                                        source_info: v.source_info.clone(),
                                        offset: v.physical_prefix_len(),
                                        rootless: v.rootless,
                                    };
                                    let pair = (cons.tagged_ptr(), viewed.tagged_ptr());
                                    if inner_ctx.coinductive_assumptions.contains(&pair) {
                                        return Ok(ThreeValuedLogic::True);
                                    }
                                    inner_ctx.coinductive_assumptions.push(pair);
                                    let result =
                                        cons.check(viewed.as_ref_dispatcher(), &mut inner_ctx);
                                    inner_ctx.coinductive_assumptions.pop();
                                    all &= result?;
                                    Ok(all)
                                }
                                (None, Some(seek)) => {
                                    let viewed = Self {
                                        ty: v.ty.clone(),
                                        source_info: v.source_info.clone(),
                                        offset: seek,
                                        rootless: v.rootless,
                                    };
                                    all &= test_true!(
                                        cons.check(viewed.as_ref_dispatcher(), &mut inner_ctx)?
                                    );
                                    Ok(all)
                                }
                                (seek @ Some(_), None) => {
                                    // self还有前缀未匹配完，other剩余部分是repeat，可以继续匹配
                                    let mut seek = seek;
                                    while let Some(cursor) = seek {
                                        let ty_self = &self.physical_prefix()[cursor];
                                        all &=
                                            test_true!(ty_self.check(
                                                r_repeat.as_ref_dispatcher(),
                                                &mut inner_ctx
                                            )?);
                                        seek = self.next_index(cursor);
                                    }
                                    // 处理完前缀后，检查剩余部分
                                    let viewed = Self {
                                        ty: v.ty.clone(),
                                        source_info: v.source_info.clone(),
                                        offset: v.physical_prefix_len(),
                                        rootless: v.rootless,
                                    };
                                    let pair = (cons.tagged_ptr(), viewed.tagged_ptr());
                                    if inner_ctx.coinductive_assumptions.contains(&pair) {
                                        return Ok(ThreeValuedLogic::True);
                                    }
                                    inner_ctx.coinductive_assumptions.push(pair);
                                    let result =
                                        cons.check(viewed.as_ref_dispatcher(), &mut inner_ctx);
                                    inner_ctx.coinductive_assumptions.pop();
                                    all &= result?;
                                    Ok(all)
                                }
                                _ => unreachable!(),
                            }
                        }

                        (SequenceType::Cons(_, l_cons), SequenceType::Cons(_, r_cons)) => {
                            if self.is_prefix_empty() {
                                return Err(TypeError::TypeMayCauseCircularReasoning(
                                    self.clone().into_dispatcher().into(),
                                ));
                            }
                            if v.is_prefix_empty() {
                                return Err(TypeError::TypeMayCauseCircularReasoning(
                                    v.clone().into_dispatcher().into(),
                                ));
                            }
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
                                        offset: seek,
                                        rootless: v.rootless,
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
                                        offset: seek,
                                        rootless: self.rootless,
                                    };
                                    let pair = (viewed.tagged_ptr(), r_cons.tagged_ptr());
                                    if inner_ctx.coinductive_assumptions.contains(&pair) {
                                        return Ok(ThreeValuedLogic::True);
                                    }
                                    inner_ctx.coinductive_assumptions.push(pair);
                                    let result =
                                        viewed.check(r_cons.as_ref_dispatcher(), &mut inner_ctx);
                                    inner_ctx.coinductive_assumptions.pop();
                                    all &= result?;
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
                ctx.coinductive_assumptions,
                pattern_env,
                ctx.lhs_env,
                ctx.rhs_env,
                ctx.bound_generic_variables,
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
                        (SequenceType::Unit, SequenceType::Cons(_, _)) => {
                            if v.is_prefix_empty() {
                                Err(TypeError::TypeMayCauseCircularReasoning(
                                    v.clone().into_dispatcher().into(),
                                ))
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
                                    while let Some(cursor) = seek {
                                        let ty_self = &self.physical_prefix()[cursor];
                                        all &=
                                            test_true!(ty_self.subof(
                                                r_repeat.as_ref_dispatcher(),
                                                &mut inner_ctx
                                            )?);
                                        seek = self.next_index(cursor);
                                    }
                                    Ok(all)
                                }
                                // (_, Some(_)) 说明 other 还有前缀未匹配完，self已经没有元素了，必然失败
                                _ => Ok(ThreeValuedLogic::False),
                            }
                        }
                        (SequenceType::NonEmptyTuple(_), SequenceType::Cons(_, cons)) => {
                            if v.is_prefix_empty() {
                                return Err(TypeError::TypeMayCauseCircularReasoning(
                                    v.clone().into_dispatcher().into(),
                                ));
                            }
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
                                        ty: self.ty.clone(),
                                        source_info: self.source_info.clone(),
                                        offset: seek,
                                        rootless: self.rootless,
                                    };
                                    let pair = (viewed.tagged_ptr(), cons.tagged_ptr());
                                    if inner_ctx.coinductive_assumptions.contains(&pair) {
                                        return Ok(ThreeValuedLogic::True);
                                    }
                                    inner_ctx.coinductive_assumptions.push(pair);
                                    let result =
                                        viewed.subof(cons.as_ref_dispatcher(), &mut inner_ctx);
                                    inner_ctx.coinductive_assumptions.pop();
                                    all &= result?;
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
                                    while let Some(cursor) = seek {
                                        let ty_self = &self.physical_prefix()[cursor];
                                        all &=
                                            test_true!(ty_self.subof(
                                                r_repeat.as_ref_dispatcher(),
                                                &mut inner_ctx
                                            )?);
                                        seek = self.next_index(cursor);
                                    }
                                    Ok(all)
                                }
                                (None, Some(_)) => {
                                    // while let Some((cursor, _)) = seek {
                                    //     let ty_other = &v.physical_prefix()[cursor].0;
                                    //     all &=
                                    //         test_true!(l_repeat.subof(
                                    //             ty_other.as_ref_dispatcher(),
                                    //             &mut inner_ctx
                                    //         )?);
                                    //     seek = v.next_block(cursor);
                                    // }
                                    // Ok(all)
                                    Ok(ThreeValuedLogic::False) // LHS为None的时候，无法匹配非空的RHS前缀
                                }
                                _ => unreachable!(),
                            }
                        }

                        (SequenceType::Repeat(_, _), SequenceType::Cons(_, r_cons)) => {
                            if v.is_prefix_empty() {
                                return Err(TypeError::TypeMayCauseCircularReasoning(
                                    v.clone().into_dispatcher().into(),
                                ));
                            }
                            let (mut all, self_seek, other_seek) =
                                self.subof_prefix(v, &mut inner_ctx)?;
                            test_true!(all);
                            match (self_seek, other_seek) {
                                (None, None) => {
                                    let viewed = Self {
                                        ty: self.ty.clone(),
                                        source_info: self.source_info.clone(),
                                        offset: self.physical_prefix_len(),
                                        rootless: self.rootless,
                                    };
                                    let pair = (viewed.tagged_ptr(), r_cons.tagged_ptr());
                                    if inner_ctx.coinductive_assumptions.contains(&pair) {
                                        return Ok(ThreeValuedLogic::True);
                                    }
                                    inner_ctx.coinductive_assumptions.push(pair);
                                    let result =
                                        viewed.subof(r_cons.as_ref_dispatcher(), &mut inner_ctx);
                                    inner_ctx.coinductive_assumptions.pop();
                                    all &= result?;
                                    Ok(all)
                                }
                                (Some(seek), None) => {
                                    let viewed = Self {
                                        ty: self.ty.clone(),
                                        source_info: self.source_info.clone(),
                                        offset: seek,
                                        rootless: self.rootless,
                                    };
                                    let pair = (viewed.tagged_ptr(), r_cons.tagged_ptr());
                                    if inner_ctx.coinductive_assumptions.contains(&pair) {
                                        return Ok(ThreeValuedLogic::True);
                                    }
                                    inner_ctx.coinductive_assumptions.push(pair);
                                    let result =
                                        viewed.subof(r_cons.as_ref_dispatcher(), &mut inner_ctx);
                                    inner_ctx.coinductive_assumptions.pop();
                                    all &= result?;
                                    Ok(all)
                                }
                                (None, Some(_)) => {
                                    // 如果Cons还有前缀未匹配完，由于Repeat可以匹配任意长度（包括0长）的序列，这里显然不可能成功，因为Cons还有前缀未匹配完，它能能匹配的最短序列长度大于0
                                    Ok(ThreeValuedLogic::False)
                                }
                                _ => unreachable!(),
                            }
                        }

                        (SequenceType::Cons(_, _), SequenceType::Unit) => {
                            if self.is_prefix_empty() {
                                Err(TypeError::TypeMayCauseCircularReasoning(
                                    self.clone().into_dispatcher().into(),
                                ))
                            } else {
                                Ok(ThreeValuedLogic::False)
                            }
                        }

                        (SequenceType::Cons(_, cons), SequenceType::NonEmptyTuple(_)) => {
                            if self.is_prefix_empty() {
                                return Err(TypeError::TypeMayCauseCircularReasoning(
                                    self.clone().into_dispatcher().into(),
                                ));
                            }
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
                                        offset: seek,
                                        rootless: v.rootless,
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
                            if self.is_prefix_empty() {
                                return Err(TypeError::TypeMayCauseCircularReasoning(
                                    self.clone().into_dispatcher().into(),
                                ));
                            }
                            let (mut all, self_seek, other_seek) =
                                self.subof_prefix(v, &mut inner_ctx)?;
                            test_true!(all);
                            match (self_seek, other_seek) {
                                (None, None) => {
                                    let viewed = Self {
                                        ty: v.ty.clone(),
                                        source_info: v.source_info.clone(),
                                        offset: v.physical_prefix_len(),
                                        rootless: v.rootless,
                                    };
                                    let pair = (cons.tagged_ptr(), viewed.tagged_ptr());
                                    if inner_ctx.coinductive_assumptions.contains(&pair) {
                                        return Ok(ThreeValuedLogic::True);
                                    }
                                    inner_ctx.coinductive_assumptions.push(pair);
                                    let result =
                                        cons.subof(viewed.as_ref_dispatcher(), &mut inner_ctx);
                                    inner_ctx.coinductive_assumptions.pop();
                                    all &= result?;
                                    Ok(all)
                                }
                                (None, Some(seek)) => {
                                    let viewed = Self {
                                        ty: v.ty.clone(),
                                        source_info: v.source_info.clone(),
                                        offset: seek,
                                        rootless: v.rootless,
                                    };
                                    all &= test_true!(
                                        cons.subof(viewed.as_ref_dispatcher(), &mut inner_ctx)?
                                    );
                                    Ok(all)
                                }
                                (seek @ Some(_), None) => {
                                    // self还有前缀未匹配完，other剩余部分是repeat，可以继续匹配
                                    let mut seek = seek;
                                    while let Some(cursor) = seek {
                                        let ty_self = &self.physical_prefix()[cursor];
                                        all &=
                                            test_true!(ty_self.subof(
                                                r_repeat.as_ref_dispatcher(),
                                                &mut inner_ctx
                                            )?);
                                        seek = self.next_index(cursor);
                                    }
                                    // 处理完前缀后，检查剩余部分
                                    let viewed = Self {
                                        ty: v.ty.clone(),
                                        source_info: v.source_info.clone(),
                                        offset: v.physical_prefix_len(),
                                        rootless: v.rootless,
                                    };
                                    let pair = (cons.tagged_ptr(), viewed.tagged_ptr());
                                    if inner_ctx.coinductive_assumptions.contains(&pair) {
                                        return Ok(ThreeValuedLogic::True);
                                    }
                                    inner_ctx.coinductive_assumptions.push(pair);
                                    let result =
                                        cons.subof(viewed.as_ref_dispatcher(), &mut inner_ctx);
                                    inner_ctx.coinductive_assumptions.pop();
                                    all &= result?;
                                    Ok(all)
                                }
                                _ => unreachable!(),
                            }
                        }

                        (SequenceType::Cons(_, l_cons), SequenceType::Cons(_, r_cons)) => {
                            if self.is_prefix_empty() {
                                return Err(TypeError::TypeMayCauseCircularReasoning(
                                    self.clone().into_dispatcher().into(),
                                ));
                            }
                            if v.is_prefix_empty() {
                                return Err(TypeError::TypeMayCauseCircularReasoning(
                                    v.clone().into_dispatcher().into(),
                                ));
                            }
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
                                        offset: seek,
                                        rootless: v.rootless,
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
                                        offset: seek,
                                        rootless: self.rootless,
                                    };
                                    let pair = (viewed.tagged_ptr(), r_cons.tagged_ptr());
                                    if inner_ctx.coinductive_assumptions.contains(&pair) {
                                        return Ok(ThreeValuedLogic::True);
                                    }
                                    inner_ctx.coinductive_assumptions.push(pair);
                                    let result =
                                        viewed.subof(r_cons.as_ref_dispatcher(), &mut inner_ctx);
                                    inner_ctx.coinductive_assumptions.pop();
                                    all &= result?;
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
        &self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        match &self.ty {
            SequenceType::Unit => Ok(self.clone().dispatch()),
            SequenceType::NonEmptyTuple(prefix) => {
                let mut new_prefix = Vec::with_capacity(prefix.len());
                for ty in prefix.iter() {
                    let reduced_ty = ty.reduce(ctx)?.into_dispatcher();
                    new_prefix.push(reduced_ty);
                }
                Ok(Sequence::new_tuple(new_prefix, self.source_info.clone()))
            }
            SequenceType::Repeat(prefix, tail) => {
                let mut new_prefix = Vec::with_capacity(prefix.len());
                for ty in prefix.iter() {
                    let reduced_ty = ty.reduce(ctx)?.into_dispatcher();
                    new_prefix.push(reduced_ty);
                }
                let reduced_tail = tail.reduce(ctx)?.into_dispatcher();
                Ok(Sequence::new_repeat(new_prefix, reduced_tail, self.source_info.clone()))
            }
            SequenceType::Cons(prefix, tail) => {
                let mut new_prefix = Vec::with_capacity(prefix.len());
                for ty in prefix.iter() {
                    let reduced_ty = ty.reduce(ctx)?.into_dispatcher();
                    new_prefix.push(reduced_ty);
                }
                let reduced_tail = tail.reduce(ctx)?.into_dispatcher();
                Ok(Sequence::new_cons(new_prefix, reduced_tail, self.source_info.clone()))
            }
            SequenceType::Phantom(_) => unreachable!(),
        }
    }

    fn invoke(&self, _ctx: InvokeContext<Type<T>, T>) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.clone().dispatch().into()))
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

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Sequence<Type<T>, T> {
    fn represent(
        &self,
        path: &mut FastCycleDetector<TaggedPtr<()>>,
        depth: usize,
        max_depth: usize,
    ) -> String {
        // 处理 offset，只显示视图中可见的部分
        if let Some(start_idx) = self.seek_prefix() {
            let prefix = self.physical_prefix();
            let mut parts = Vec::new();
            for ty in prefix.iter().skip(start_idx) {
                parts.push(ty.represent(path, depth + 1, max_depth));
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
                SequenceType::Phantom(_) => unreachable!(),
            }
        } else {
            // offset 已经超出 prefix，或者是 Unit
            match &self.ty {
                SequenceType::Unit => "()".to_string(),
                SequenceType::Repeat(_, tail) => {
                    // prefix 已经全部被跳过，只剩下 repeat 部分
                    format!("(!..{})", tail.represent(path, depth + 1, max_depth))
                }
                SequenceType::Cons(_, tail) => {
                    // prefix 已经全部被跳过，只剩下 cons 的 tail
                    format!("(!~{})", tail.represent(path, depth + 1, max_depth))
                }
                SequenceType::NonEmptyTuple(_) => {
                    // offset 超出了 tuple 的长度，这应该是空的
                    "()".to_string()
                }
                SequenceType::Phantom(_) => unreachable!(),
            }
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Sequence<Type<T>, T> {
    pub fn new_repeat<U: AsDispatcher<Type<T>, T>, V: AsDispatcher<Type<T>, T>>(
        prefix: impl IntoIterator<Item = U>,
        tail: V,

        source_info: Option<Arc<SourceLocation>>,
    ) -> Type<T> {
        let prefix_iter = prefix.into_iter().map(|ty| ty.into_dispatcher());
        let prefix = Arc::from_iter(prefix_iter);
        let tail = tail.into_dispatcher();
        let rootless = tail.rootless() & prefix.iter().all(|ty| ty.rootless());
        Self { ty: SequenceType::Repeat(prefix, Arc::new(tail)), rootless, source_info, offset: 0 }
            .dispatch()
    }

    pub fn new_cons<U: AsDispatcher<Type<T>, T>, V: AsDispatcher<Type<T>, T>>(
        prefix: impl IntoIterator<Item = U>,
        tail: V,

        source_info: Option<Arc<SourceLocation>>,
    ) -> Type<T> {
        let prefix_iter = prefix.into_iter().map(|ty| ty.into_dispatcher());
        let prefix = Arc::from_iter(prefix_iter);
        let tail = tail.into_dispatcher();
        let rootless = tail.rootless() & prefix.iter().all(|ty| ty.rootless());
        Self { ty: SequenceType::Cons(prefix, Arc::new(tail)), rootless, source_info, offset: 0 }
            .dispatch()
    }

    pub fn new_tuple<U: AsDispatcher<Type<T>, T>>(
        prefix: impl IntoIterator<Item = U>,

        source_info: Option<Arc<SourceLocation>>,
    ) -> Type<T> {
        let prefix_iter = prefix.into_iter().map(|ty| ty.into_dispatcher());
        if prefix_iter.size_hint().0 == 0 {
            return Self::unit(source_info);
        }
        let prefix = Arc::from_iter(prefix_iter);
        let rootless = prefix.iter().all(|ty| ty.rootless());
        Self { ty: SequenceType::NonEmptyTuple(prefix), rootless, source_info, offset: 0 }
            .dispatch()
    }

    pub fn unit(source_info: Option<Arc<SourceLocation>>) -> Type<T> {
        Self { ty: SequenceType::Unit, source_info, offset: 0, rootless: true }.dispatch()
    }

    pub fn unit_seq(source_info: Option<Arc<SourceLocation>>) -> Sequence<Type<T>, T> {
        Self { ty: SequenceType::Unit, source_info, offset: 0, rootless: true }
    }

    pub fn nature_number<V: AsDispatcher<Type<T>, T>>(
        num: usize,
        ty: V,
        source_info: Option<Arc<SourceLocation>>,
    ) -> Type<T> {
        if num == 0 {
            Self::unit(source_info)
        } else {
            let ty = ty.into_dispatcher();
            let rootless = ty.rootless();
            let prefix = Arc::from_iter(std::iter::repeat_n(ty, num));
            Self { ty: SequenceType::NonEmptyTuple(prefix), rootless, source_info, offset: 0 }
                .dispatch()
        }
    }

    #[allow(clippy::type_complexity)]
    pub fn check_prefix(
        &self,
        other: &Sequence<Type<T>, T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<(ThreeValuedLogic, Option<usize>, Option<usize>), TypeError<Type<T>, T>> {
        let mut self_seek = self.seek_prefix();
        let mut other_seek = other.seek_prefix();
        let mut all = ThreeValuedLogic::True;

        while let (Some(cursor_self), Some(cursor_other)) = (self_seek, other_seek) {
            let ty_self = &self.physical_prefix()[cursor_self];
            let ty_other = &other.physical_prefix()[cursor_other];
            all &= ty_self.check(ty_other.as_ref_dispatcher(), ctx)?;
            if let ThreeValuedLogic::False = all {
                return Ok((ThreeValuedLogic::False, self_seek, other_seek));
            }

            self_seek = self.next_index(cursor_self);
            other_seek = other.next_index(cursor_other);
        }

        Ok((all, self_seek, other_seek))
    }

    #[allow(clippy::type_complexity)]
    pub fn subof_prefix(
        &self,
        other: &Sequence<Type<T>, T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<(ThreeValuedLogic, Option<usize>, Option<usize>), TypeError<Type<T>, T>> {
        let mut self_seek = self.seek_prefix();
        let mut other_seek = other.seek_prefix();
        let mut all = ThreeValuedLogic::True;

        while let (Some(cursor_self), Some(cursor_other)) = (self_seek, other_seek) {
            let ty_self = &self.physical_prefix()[cursor_self];
            let ty_other = &other.physical_prefix()[cursor_other];
            all &= ty_self.subof(ty_other.as_ref_dispatcher(), ctx)?;
            if let ThreeValuedLogic::False = all {
                return Ok((ThreeValuedLogic::False, self_seek, other_seek));
            }

            self_seek = self.next_index(cursor_self);
            other_seek = other.next_index(cursor_other);
        }

        Ok((all, self_seek, other_seek))
    }

    // 返回起始索引，如果 offset 超出了 prefix，返回 None
    fn seek_prefix(&self) -> Option<usize> {
        if self.offset < self.physical_prefix().len() { Some(self.offset) } else { None }
    }

    fn next_index(&self, current_idx: usize) -> Option<usize> {
        let next_idx = current_idx + 1;
        if next_idx < self.physical_prefix().len() { Some(next_idx) } else { None }
    }

    pub fn physical_prefix(&self) -> &[Type<T>] {
        match &self.ty {
            SequenceType::Repeat(prefix, _) => prefix.as_ref(),
            SequenceType::Cons(prefix, _) => prefix.as_ref(),
            SequenceType::NonEmptyTuple(prefix) => prefix.as_ref(),
            SequenceType::Unit => &[],
            SequenceType::Phantom(_) => unreachable!(),
        }
    }

    pub fn physical_prefix_len(&self) -> usize {
        self.physical_prefix().len()
    }

    pub fn offset(&self) -> usize {
        self.offset
    }

    pub fn is_prefix_empty(&self) -> bool {
        self.physical_prefix_len() <= self.offset
    }

    pub fn get_prefix_value(&self, index: usize) -> Option<&Type<T>> {
        self.physical_prefix().get(self.offset + index)
    }

    pub fn view(&self, offset: usize) -> Option<Sequence<Type<T>, T>> {
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
                        rootless: self.rootless,
                    })
                } else {
                    // 视图仍然有元素
                    Some(Sequence {
                        ty: self.ty.clone(),
                        source_info: self.source_info.clone(),
                        offset: self.offset + offset,
                        rootless: self.rootless,
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
                        rootless: self.rootless,
                    })
                }
            }
            SequenceType::Repeat(_, _) => {
                let len = self.physical_prefix_len();
                Some(Sequence {
                    ty: self.ty.clone(),
                    source_info: self.source_info.clone(),
                    offset: if self.offset + offset >= len { len } else { self.offset + offset },
                    rootless: self.rootless,
                })
            }
            SequenceType::Unit => {
                if offset == 0 {
                    Some(Sequence {
                        ty: self.ty.clone(),
                        source_info: self.source_info.clone(),
                        offset: 0,
                        rootless: self.rootless,
                    })
                } else {
                    None
                }
            }
            SequenceType::Phantom(_) => unreachable!(),
        }
    }

    pub fn add(
        &self,
        other: &Sequence<Type<T>, T>,
    ) -> Result<Sequence<Type<T>, T>, TypeError<Type<T>, T>> {
        // 朴素拼接：直接拼接 Vec，不做任何合并或展开。
        // 对于 LHS，不复用其原始块，必须通过 seek 切割后重新拷贝。
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
            _ => {}
        }

        let self_phys = self.physical_prefix();
        let other_phys = other.physical_prefix();

        let self_seek = self.seek_prefix();
        let other_seek = other.seek_prefix();

        let self_remaining = self_seek.map(|idx| self_phys.len().saturating_sub(idx)).unwrap_or(0);
        let other_remaining =
            other_seek.map(|idx| other_phys.len().saturating_sub(idx)).unwrap_or(0);

        let mut new_prefix = Vec::with_capacity(self_remaining + other_remaining);

        if let Some(idx) = self_seek {
            new_prefix.extend(self_phys[idx..].iter().cloned());
        }
        if let Some(idx) = other_seek {
            new_prefix.extend(other_phys[idx..].iter().cloned());
        }

        let total = new_prefix.len();
        let mut iter = new_prefix.into_iter();
        let new_prefix_arc = Arc::from_iter(iter.by_ref().take(total));
        let new_ty = match &other.ty {
            SequenceType::Unit | SequenceType::NonEmptyTuple(_) => {
                if new_prefix_arc.is_empty() {
                    SequenceType::Unit
                } else {
                    SequenceType::NonEmptyTuple(new_prefix_arc)
                }
            }
            SequenceType::Repeat(_, tail) => SequenceType::Repeat(new_prefix_arc, tail.clone()),
            SequenceType::Cons(_, tail) => SequenceType::Cons(new_prefix_arc, tail.clone()),
            SequenceType::Phantom(_) => unreachable!(),
        };

        Ok(Sequence {
            ty: new_ty,
            source_info: self.source_info.clone(),
            offset: 0,
            rootless: self.rootless & other.rootless,
        })
    }

    pub fn is_tuple(&self) -> bool {
        matches!(self.ty, SequenceType::NonEmptyTuple(_) | SequenceType::Unit)
    }

    pub fn len(&self) -> usize {
        match &self.ty {
            SequenceType::Unit => 0,
            SequenceType::NonEmptyTuple(prefix) => prefix.len().saturating_sub(self.offset),
            SequenceType::Repeat(_, _) => {
                panic!("Cannot get length of Repeat sequence");
            }
            SequenceType::Cons(_, _) => {
                panic!("Cannot get length of Cons sequence");
            }
            SequenceType::Phantom(_) => unreachable!(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn is_unit(&self) -> bool {
        matches!(self.ty, SequenceType::Unit)
    }
}
