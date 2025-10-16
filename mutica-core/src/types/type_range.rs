use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, Representable, Type,
        TypeCheckContext, TypeRef, type_bound::TypeBound,
    },
    util::{collector::Collector, rootstack::Rootable},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeRangeTag {
    LClosedRClosed,
    LClosedROpen,
    LOpenRClosed,
    LOpenROpen,
}

impl TypeRangeTag {
    pub fn is_left_closed(&self) -> bool {
        matches!(
            self,
            TypeRangeTag::LClosedRClosed | TypeRangeTag::LClosedROpen
        )
    }

    pub fn is_right_closed(&self) -> bool {
        matches!(
            self,
            TypeRangeTag::LClosedRClosed | TypeRangeTag::LOpenRClosed
        )
    }
}

pub struct TypeRange<T: GcAllocObject<T, Inner = Type<T>>> {
    range: Arc<(Type<T>, Type<T>)>,
    tag: TypeRangeTag,
    is_nf: bool,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for TypeRange<T> {
    fn clone(&self) -> Self {
        Self {
            range: self.range.clone(),
            tag: self.tag.clone(),
            is_nf: self.is_nf,
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for TypeRange<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        self.range.0.collect(queue);
        self.range.1.collect(queue);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GcAllocObject<T> for TypeRange<T> {
    type Inner = Type<T>;
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for TypeRange<T> {
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        self.range.0.upgrade(collected);
        self.range.1.upgrade(collected);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for TypeRange<T> {
    fn represent(
        &self,
        path: &mut crate::util::cycle_detector::FastCycleDetector<*const ()>,
    ) -> String {
        let left_bracket = if self.tag.is_left_closed() { "[" } else { "(" };
        let right_bracket = if self.tag.is_right_closed() { "]" } else { ")" };
        format!(
            "Range{}{}, {}{}",
            left_bracket,
            self.range.0.represent(path),
            self.range.1.represent(path),
            right_bracket
        )
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for TypeRange<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;

    fn into_dispatcher(self) -> Type<T> {
        Type::Range(self)
    }

    fn as_ref_dispatcher<'a>(&'a self) -> Self::RefDispatcher<'a> {
        TypeRef::Range(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for TypeRange<T> {
    fn is_normal_form(&self) -> bool {
        self.is_nf
    }

    fn reduce(
        self,
        ctx: &mut super::ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, super::TypeError<Type<T>, T>> {
        let left_reduced = self.range.0.clone().reduce(ctx)?;
        let right_reduced = self.range.1.clone().reduce(ctx)?;
        Ok(Self::new((left_reduced, right_reduced), self.tag))
    }

    fn invoke(
        &self,
        _ctx: &mut super::InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, super::TypeError<Type<T>, T>> {
        Err(super::TypeError::NonApplicableType(
            self.clone().dispatch().into(),
        ))
    }

    fn is(
        &self,
        other: TypeRef<T>,
        ctx: &mut super::TypeCheckContext<Type<T>, T>,
    ) -> Result<Option<()>, super::TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env);
            match other {
                TypeRef::Range(v) => {
                    // for range(A, B) <: range(C, D)
                    // Left: Closed & Closed => C <= A
                    // Left: Closed & Open   => C < A (C <= A && A </: C)
                    // Left: Open   & Closed => C <= A
                    // Left: Open   & Open   => C <= A

                    // Right: Closed & Closed => B <= D
                    // Right: Closed & Open   => B <  D (B <= D && D </: B)
                    // Right: Open   & Closed => B <=  D
                    // Right: Open   & Open   => B <=  D
                    let r_cmp = self
                        .range
                        .1
                        .is(v.range.1.as_ref_dispatcher(), &mut inner_ctx)?; // B <= D
                    if r_cmp.is_none() {
                        return Ok(None);
                    }

                    let mut pattern_env_disabled = Collector::new_disabled();

                    let mut inner_ctx = TypeCheckContext::new(
                        ctx.assumptions,
                        (ctx.closure_env.1, ctx.closure_env.0),
                        &mut pattern_env_disabled,
                    );

                    let l_cmp = v
                        .range
                        .0
                        .is(self.range.0.as_ref_dispatcher(), &mut inner_ctx)?; // C <= A
                    if l_cmp.is_none() {
                        return Ok(None);
                    }

                    if self.tag.is_left_closed() || !v.tag.is_left_closed() {
                        let mut inner_ctx = TypeCheckContext::new(
                            ctx.assumptions,
                            ctx.closure_env,
                            &mut pattern_env_disabled,
                        );
                        if self
                            .range
                            .0
                            .is(v.range.0.as_ref_dispatcher(), &mut inner_ctx)?
                            .is_some()
                        {
                            // not A </: C then check fail
                            return Ok(None);
                        }
                    }

                    let mut inner_ctx = TypeCheckContext::new(
                        ctx.assumptions,
                        (ctx.closure_env.1, ctx.closure_env.0),
                        &mut pattern_env_disabled,
                    );

                    if self.tag.is_right_closed() || !v.tag.is_right_closed() {
                        if v.range
                            .1
                            .is(self.range.1.as_ref_dispatcher(), &mut inner_ctx)?
                            .is_some()
                        {
                            // not D </: B then check fail
                            return Ok(None);
                        }
                    }
                    Ok(Some(()))
                }
                TypeRef::Bound(TypeBound::Top) => Ok(Some(())),
                TypeRef::Generalize(v) => v.has(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Specialize(v) => v.has(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.has(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.has(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.has(self.as_ref_dispatcher(), &mut inner_ctx),
                _ => Ok(None),
            }
        })
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveTypeWithAny<Type<T>, T> for TypeRange<T> {
    fn has(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<Option<()>, super::TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env);
            let r_cmp = other.is(self.range.1.as_ref_dispatcher(), &mut inner_ctx)?; // X <= D
            if r_cmp.is_none() {
                return Ok(None);
            }

            // 下面的所有匹配都不允许模式的解构
            let mut new_pattern_env = Collector::new_disabled();
            let mut inner_ctx = TypeCheckContext::new(
                ctx.assumptions,
                (ctx.closure_env.1, ctx.closure_env.0),
                &mut new_pattern_env,
            );

            let l_cmp = self.range.0.is(other.clone(), &mut inner_ctx)?; // C <= X
            if l_cmp.is_none() {
                return Ok(None);
            }

            if !self.tag.is_left_closed() {
                // (C, D] or (C, D)
                let mut inner_ctx =
                    TypeCheckContext::new(ctx.assumptions, ctx.closure_env, &mut new_pattern_env);
                if other
                    .is(self.range.0.as_ref_dispatcher(), &mut inner_ctx)?
                    .is_some()
                // when X <= C then check fail
                {
                    // not X </: C then check fail
                    return Ok(None);
                }
            }

            if !self.tag.is_right_closed() {
                // [C, D) or (C, D)
                let mut inner_ctx = TypeCheckContext::new(
                    ctx.assumptions,
                    (ctx.closure_env.1, ctx.closure_env.0),
                    &mut new_pattern_env,
                );
                if self.range.1.is(other, &mut inner_ctx)?.is_some()
                // when D <= X then check fail
                {
                    // not D </: X then check fail
                    return Ok(None);
                }
            }

            Ok(Some(()))
        })
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> TypeRange<T> {
    pub fn new<U: AsDispatcher<Type<T>, T>, V: AsDispatcher<Type<T>, T>>(
        range: (U, V),
        tag: TypeRangeTag,
    ) -> Type<T> {
        let l = range.0.into_dispatcher();
        let r = range.1.into_dispatcher();
        let is_nf = l.is_normal_form() && r.is_normal_form();
        Self {
            range: Arc::new((l, r)),
            tag,
            is_nf,
        }
        .dispatch()
    }

    pub fn get_range(&self) -> &(Type<T>, Type<T>) {
        &self.range
    }

    pub fn get_tag(&self) -> &TypeRangeTag {
        &self.tag
    }
}
