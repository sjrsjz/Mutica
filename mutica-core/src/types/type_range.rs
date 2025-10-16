use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, Representable, Type, TypeCheckContext,
        TypeRef, fixpoint::FixPointInner, type_bound::TypeBound,
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

#[derive(Clone)]
pub struct TypeRange {
    range: Arc<(Type, Type)>,
    tag: TypeRangeTag,
    is_nf: bool,
}

impl GCTraceable<FixPointInner> for TypeRange {
    fn collect(
        &self,
        queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<FixPointInner>>,
    ) {
        self.range.0.collect(queue);
        self.range.1.collect(queue);
    }
}

impl Rootable<FixPointInner>for TypeRange {
    fn upgrade(&self, collected: &mut Vec<GCArc<FixPointInner>>) {
        self.range.0.upgrade(collected);
        self.range.1.upgrade(collected);
    }
}

impl Representable for TypeRange {
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

impl CoinductiveType<Type> for TypeRange {
    type RefDispatcher<'a>
        = TypeRef<'a>
    where
        Self: 'a;
    fn dispatch(self) -> Type {
        Type::Range(self)
    }

    fn dispatch_ref<'a>(&'a self) -> Self::RefDispatcher<'a> {
        TypeRef::Range(self)
    }

    fn is_normal_form(&self) -> bool {
        self.is_nf
    }

    fn reduce(self, ctx: &mut super::ReductionContext) -> Result<Type, super::TypeError<Type>> {
        let left_reduced = self.range.0.clone().reduce(ctx)?;
        let right_reduced = self.range.1.clone().reduce(ctx)?;
        Ok(Self::new((left_reduced, right_reduced), self.tag))
    }

    fn invoke(&self, _ctx: &mut super::InvokeContext) -> Result<Type, super::TypeError<Type>> {
        Err(super::TypeError::NonApplicableType(
            self.clone().dispatch().into(),
        ))
    }

    fn is(
        &self,
        other: &Type,
        ctx: &mut super::TypeCheckContext,
    ) -> Result<Option<()>, super::TypeError<Type>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env);
            match other {
                Type::Range(v) => {
                    // for range(A, B) <: range(C, D)
                    // Left: Closed & Closed => C <= A
                    // Left: Closed & Open   => C < A (C <= A && A </: C)
                    // Left: Open   & Closed => C <= A
                    // Left: Open   & Open   => C <= A

                    // Right: Closed & Closed => B <= D
                    // Right: Closed & Open   => B <  D (B <= D && D </: B)
                    // Right: Open   & Closed => B <=  D
                    // Right: Open   & Open   => B <=  D
                    let r_cmp = self.range.1.is(&v.range.1, &mut inner_ctx)?; // B <= D
                    if r_cmp.is_none() {
                        return Ok(None);
                    }

                    let mut pattern_env_disabled = Collector::new_disabled();

                    let mut inner_ctx = TypeCheckContext::new(
                        ctx.assumptions,
                        (ctx.closure_env.1, ctx.closure_env.0),
                        &mut pattern_env_disabled,
                    );

                    let l_cmp = v.range.0.is(&self.range.0, &mut inner_ctx)?; // C <= A
                    if l_cmp.is_none() {
                        return Ok(None);
                    }

                    if self.tag.is_left_closed() || !v.tag.is_left_closed() {
                        let mut inner_ctx = TypeCheckContext::new(
                            ctx.assumptions,
                            ctx.closure_env,
                            &mut pattern_env_disabled,
                        );
                        if self.range.0.is(&v.range.0, &mut inner_ctx)?.is_some() {
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
                        if v.range.1.is(&self.range.1, &mut inner_ctx)?.is_some() {
                            // not D </: B then check fail
                            return Ok(None);
                        }
                    }
                    Ok(Some(()))
                }
                Type::Bound(TypeBound::Top) => Ok(Some(())),
                Type::Generalize(v) => v.has(self, &mut inner_ctx),
                Type::Specialize(v) => v.has(self, &mut inner_ctx),
                Type::FixPoint(v) => v.has(self, &mut inner_ctx),
                Type::Pattern(v) => v.has(self, &mut inner_ctx),
                Type::Variable(v) => v.has(self, &mut inner_ctx),
                _ => Ok(None),
            }
        })
    }
}

impl CoinductiveTypeWithAny<Type> for TypeRange {
    fn has<V: CoinductiveType<Type>>(
        &self,
        other: &V,
        ctx: &mut TypeCheckContext,
    ) -> Result<Option<()>, super::TypeError<Type>> {
        ctx.pattern_env.collect(|_| {
            let mut new_pattern_env = Collector::new_disabled();
            let mut inner_ctx = TypeCheckContext::new(
                ctx.assumptions,
                (ctx.closure_env.1, ctx.closure_env.0),
                &mut new_pattern_env,
            );

            let l_cmp = self.range.0.is(&other.clone().dispatch(), &mut inner_ctx)?; // C <= X
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, &mut new_pattern_env);

            let r_cmp = other.is(&self.range.1, &mut inner_ctx)?; // X <= D
            if l_cmp.is_some() && r_cmp.is_some() {
                Ok(Some(()))
            } else {
                Ok(None)
            }
        })
    }
}

impl TypeRange {
    pub fn new<U: AsDispatcher, V: AsDispatcher>(range: (U, V), tag: TypeRangeTag) -> Type {
        let is_nf =
            range.0.as_ref_dispatcher().is_normal_form() && range.1.as_ref_dispatcher().is_normal_form();
        Self {
            range: Arc::new((range.0.into_dispatcher(), range.1.into_dispatcher())),
            tag,
            is_nf,
        }
        .dispatch()
    }

    pub fn get_range(&self) -> &(Type, Type) {
        &self.range
    }

    pub fn get_tag(&self) -> &TypeRangeTag {
        &self.tag
    }
}
