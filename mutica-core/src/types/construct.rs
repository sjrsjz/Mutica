use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::{
    test_true,
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, Representable,
        Rootable, TaggedPtr, Type, TypeCheckContext, TypeRef,
    },
    util::{arc_opt::ArcOpt, source_info::SourceLocation, three_valued_logic::ThreeValuedLogic},
};

pub struct Construct<T: GcAllocObject<T, Inner = Type<T>>> {
    inner: ArcOpt<(Vec<Type<T>>, Type<T>)>,
    source_info: Option<Arc<SourceLocation>>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Construct<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
            source_info: self.source_info.clone(),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Construct<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        for v in self.prefix() {
            v.collect(queue);
        }
        self.tail().collect(queue);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Construct<T> {
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        for v in self.prefix() {
            v.upgrade(collected);
        }
        self.tail().upgrade(collected);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Construct<T> {
    fn represent(
        &self,
        path: &mut crate::util::cycle_detector::FastCycleDetector<TaggedPtr<()>>,
        depth: usize,
        max_depth: usize,
    ) -> String {
        if depth > max_depth {
            return "...".to_string();
        }
        let mut repr = "Cons<(".to_string();
        for (i, v) in self.prefix().iter().enumerate() {
            repr.push_str(&v.represent(path, depth + 1, max_depth));
            if self.prefix().len() != 1 && i != self.prefix().len() - 1 {
                repr.push_str(", ");
            }
        }
        repr.push_str("), ");
        repr.push_str(&self.tail().represent(path, depth + 1, max_depth));
        repr.push_str(">");
        repr
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for Construct<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;

    fn into_dispatcher(self) -> Type<T> {
        Type::Construct(self)
    }

    fn as_ref_dispatcher<'a>(&'a self) -> Self::RefDispatcher<'a> {
        TypeRef::Construct(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for Construct<T> {
    fn check(
        &self,
        other: TypeRef<T>,
        ctx: &mut super::TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, super::TypeError<Type<T>, T>> {
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
                TypeRef::Construct(v) => {
                    if self.prefix().len() != v.prefix().len() {
                        return Ok(ThreeValuedLogic::False);
                    }
                    let mut all = ThreeValuedLogic::True;
                    for (a, b) in self.prefix().iter().zip(v.prefix().iter()) {
                        all &= test_true!(a.check(b.as_ref_dispatcher(), &mut inner_ctx)?)
                    }
                    Ok(all
                        & self
                            .tail()
                            .check(v.tail().as_ref_dispatcher(), &mut inner_ctx)?)
                }
                TypeRef::Tuple(v) => {
                    if v.len() < self.prefix().len() {
                        return Ok(ThreeValuedLogic::False);
                    }
                    let mut all = ThreeValuedLogic::True;
                    for (i, x) in self.prefix().iter().enumerate() {
                        all &= test_true!(
                            x.check(v.get(i).unwrap().as_ref_dispatcher(), &mut inner_ctx)?
                        )
                    }
                    let tail = v.view(self.prefix().len());
                    Ok(all
                        & self
                            .tail()
                            .check(tail.as_ref_dispatcher(), &mut inner_ctx)?)
                }
                TypeRef::NatureNumber(v) => match v.view(self.prefix().len()) {
                    Some(tail) => {
                        let mut all = ThreeValuedLogic::True;
                        for x in self.prefix() {
                            all &= test_true!(x.check(v.ty().as_ref_dispatcher(), &mut inner_ctx)?)
                        }
                        Ok(all
                            & self
                                .tail()
                                .check(tail.as_ref_dispatcher(), &mut inner_ctx)?)
                    }
                    None => Ok(ThreeValuedLogic::False),
                },
                TypeRef::Range(v) => match v.view(self.prefix().len()) {
                    Some(tail) => {
                        let mut all = ThreeValuedLogic::True;
                        for x in self.prefix() {
                            all &= test_true!(x.check(v.ty().as_ref_dispatcher(), &mut inner_ctx)?)
                        }
                        Ok(all
                            & self
                                .tail()
                                .check(tail.as_ref_dispatcher(), &mut inner_ctx)?)
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
    ) -> Result<ThreeValuedLogic, super::TypeError<Type<T>, T>> {
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
                TypeRef::Construct(v) => {
                    if self.prefix().len() != v.prefix().len() {
                        return Ok(ThreeValuedLogic::False);
                    }
                    let mut all = ThreeValuedLogic::True;
                    for (a, b) in self.prefix().iter().zip(v.prefix().iter()) {
                        all &= test_true!(a.subof(b.as_ref_dispatcher(), &mut inner_ctx)?)
                    }
                    Ok(all
                        & self
                            .tail()
                            .subof(v.tail().as_ref_dispatcher(), &mut inner_ctx)?)
                }
                TypeRef::Tuple(v) => {
                    if v.len() < self.prefix().len() {
                        return Ok(ThreeValuedLogic::False);
                    }
                    let mut all = ThreeValuedLogic::True;
                    for (i, x) in self.prefix().iter().enumerate() {
                        all &= test_true!(
                            x.subof(v.get(i).unwrap().as_ref_dispatcher(), &mut inner_ctx)?
                        )
                    }
                    let tail = v.view(self.prefix().len());
                    Ok(all
                        & self
                            .tail()
                            .subof(tail.as_ref_dispatcher(), &mut inner_ctx)?)
                }
                TypeRef::NatureNumber(v) => match v.view(self.prefix().len()) {
                    Some(tail) => {
                        let mut all = ThreeValuedLogic::True;
                        for x in self.prefix() {
                            all &= test_true!(x.subof(v.ty().as_ref_dispatcher(), &mut inner_ctx)?)
                        }
                        Ok(all
                            & self
                                .tail()
                                .subof(tail.as_ref_dispatcher(), &mut inner_ctx)?)
                    }
                    None => Ok(ThreeValuedLogic::False),
                },
                TypeRef::Range(v) => match v.view(self.prefix().len()) {
                    Some(tail) => {
                        let mut all = ThreeValuedLogic::True;
                        for x in self.prefix() {
                            all &= test_true!(x.subof(v.ty().as_ref_dispatcher(), &mut inner_ctx)?)
                        }
                        Ok(all
                            & self
                                .tail()
                                .subof(tail.as_ref_dispatcher(), &mut inner_ctx)?)
                    }
                    None => Ok(ThreeValuedLogic::False),
                },
                _ => Ok(ThreeValuedLogic::False),
            }
        })
    }

    fn reduce(
        mut self,
        ctx: &mut super::ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, super::TypeError<Type<T>, T>> {
        match self.inner.modify(|(prefix, tail)| {
            let new_prefix = prefix
                .into_iter()
                .map(|v| v.reduce(ctx))
                .collect::<Result<Vec<_>, _>>()?;
            let new_tail = tail.reduce(ctx)?;
            Ok((new_prefix, new_tail))
        })? {
            Some(()) => Ok(self.dispatch()),
            None => {
                let (prefix, tail) = self.inner.as_ref();
                let new_prefix = prefix
                    .iter()
                    .map(|v| v.clone().reduce(ctx))
                    .collect::<Result<Vec<_>, _>>()?;
                let new_tail = tail.clone().reduce(ctx)?;
                Ok(Self::new(new_prefix, new_tail, self.source_info.clone()))
            }
        }
    }

    fn invoke(
        self,
        _ctx: super::InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, super::TypeError<Type<T>, T>> {
        Err(super::TypeError::NonApplicableType(self.dispatch().into()))
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        self.source_info.as_ref()
    }

    fn report_source_info(&self) -> crate::types::TypeReport {
        if let Some(loc) = self.source_info() {
            let span = loc.span().clone();
            let filepath = loc.source().filepath().to_string();
            ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                .with_message(format!("Constructor type at {}", filepath))
                .with_label(
                    ariadne::Label::new((filepath, span)).with_message("Constructor defined here"),
                )
                .finish()
        } else {
            ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                .with_message("Constructor type has no source location")
                .with_label(
                    ariadne::Label::new(("<unknown>".to_string(), 0..0))
                        .with_message("Location unknown"),
                )
                .finish()
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Construct<T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new<
        U: IntoIterator<Item = W>,
        V: AsDispatcher<Type<T>, T>,
        W: AsDispatcher<Type<T>, T>,
    >(
        head: U,
        tail: V,
        source_info: Option<Arc<SourceLocation>>,
    ) -> Type<T> {
        Self {
            inner: ArcOpt::new((
                head.into_iter().map(|v| v.into_dispatcher()).collect(),
                tail.into_dispatcher(),
            )),
            source_info,
        }
        .dispatch()
    }

    pub fn prefix(&self) -> &[Type<T>] {
        &self.inner.as_ref().0
    }

    pub fn tail(&self) -> &Type<T> {
        &self.inner.as_ref().1
    }
}
