use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeRef, CoinductiveTypeWithAny, GcAllocObject,
        InvokeContext, ReductionContext, Representable, Rootable, TaggedPtr, Type,
        TypeCheckContext, TypeError, TypeRef,
    },
    util::{source_info::SourceLocation, three_valued_logic::ThreeValuedLogic},
};

pub struct Pattern<T: GcAllocObject<T, Inner = Type<T>>> {
    bind_name: Arc<str>,
    bind_layer: usize, // 从栈底开始数的，相当于嵌套了第几层
    source_info: Option<Arc<SourceLocation>>,
    _phantom: std::marker::PhantomData<T>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Pattern<T> {
    fn clone(&self) -> Self {
        Self {
            bind_name: self.bind_name.clone(),
            bind_layer: self.bind_layer,
            source_info: self.source_info.clone(),
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Pattern<T> {
    fn collect(&self, _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {}
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Pattern<T> {
    fn upgrade(&self, _collected: &mut Vec<GCArc<T>>) {}
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Pattern<T> {
    fn represent(
        &self,
        _path: &mut crate::util::cycle_detector::FastCycleDetector<TaggedPtr<()>>,
        _depth: usize,
        _max_depth: usize,
    ) -> String {
        format!("T_{}.{}", self.bind_layer, self.bind_name)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for Pattern<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;

    fn into_dispatcher(self) -> Type<T> {
        Type::Pattern(self)
    }

    fn as_ref_dispatcher<'a>(&'a self) -> Self::RefDispatcher<'a> {
        TypeRef::Pattern(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for Pattern<T> {
    fn check(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        (match ctx.collected.lookup(&self.bind_name) {
            Some(existing) => existing.clone(),
            None => {
                return ctx.pattern_collector.collect(|pattern_env| {
                    let mut inner_ctx = TypeCheckContext::new(
                        ctx.assumptions,
                        pattern_env,
                        ctx.lhs_env,
                        ctx.rhs_env,
                        ctx.collected,
                    );
                    match other {
                        TypeRef::Any(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                        TypeRef::All(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                        TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                        TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                        TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                        TypeRef::SubOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                        TypeRef::EqOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                        TypeRef::Bound(v)
                            if matches!(&v.kind, crate::types::type_bound::TypeBoundKind::Top) =>
                        {
                            Ok(ThreeValuedLogic::True)
                        }
                        // Pattern 无法直接匹配其他类型
                        _ => Ok(ThreeValuedLogic::False),
                    }
                });
            }
        })
        .check(other, ctx)
    }

    fn subof(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        (match ctx.collected.lookup(&self.bind_name) {
            Some(existing) => existing.clone(),
            None => {
                return ctx.pattern_collector.collect(|pattern_env| {
                    let mut inner_ctx = TypeCheckContext::new(
                        ctx.assumptions,
                        pattern_env,
                        ctx.lhs_env,
                        ctx.rhs_env,
                        ctx.collected,
                    );
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
                        _ => Ok(ThreeValuedLogic::False),
                    }
                });
            }
        })
        .subof(other, ctx)
    }

    fn invoke(self, _ctx: InvokeContext<Type<T>, T>) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.into_dispatcher().into()))
    }

    fn reduce(
        self,
        _ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Ok(self.into_dispatcher())
    }

    fn tagged_ptr(&self) -> super::TaggedPtr<()> {
        super::TaggedPtr::new_unique(&self as *const _ as *const ())
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        self.source_info.as_ref()
    }

    fn report_source_info(&self) -> crate::types::TypeReport {
        if let Some(loc) = self.source_info.as_ref() {
            let span = loc.span().clone();
            let filepath = loc.source().filepath().to_string();
            ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                .with_message(format!("Pattern type at {}", filepath))
                .with_label(
                    ariadne::Label::new((filepath, span)).with_message("Pattern defined here"),
                )
                .finish()
        } else {
            ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                .with_message("Pattern type has no source location")
                .with_label(
                    ariadne::Label::new(("<unknown>".to_string(), 0..0))
                        .with_message("Location unknown"),
                )
                .finish()
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveTypeWithAny<Type<T>, T> for Pattern<T> {
    #[stacksafe::stacksafe]
    fn accept(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        other.check(
            (if self.bind_layer < ctx.collected.layers() {
                match ctx.collected.lookup(&self.bind_name) {
                    Some(existing) => existing.clone(),
                    None => {
                        return Ok(ThreeValuedLogic::Unknown); // 未绑定
                    }
                }
            } else if self.bind_layer == ctx.collected.layers() {
                // 绑定当前层
                ctx.pattern_collector.push((self.bind_name.clone(), other.clone_data()));
                return Ok(ThreeValuedLogic::True);
            } else {
                return Err(TypeError::GenericLayerOverflow(other.clone_data().into()));
            })
            .as_ref_dispatcher(),
            ctx,
        )
    }

    #[stacksafe::stacksafe]
    fn superof(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        other.subof(
            (if self.bind_layer < ctx.collected.layers() {
                match ctx.collected.lookup(&self.bind_name) {
                    Some(existing) => existing.clone(),
                    None => {
                        return Ok(ThreeValuedLogic::Unknown); // 未绑定
                    }
                }
            } else if self.bind_layer == ctx.collected.layers() {
                // 绑定当前层
                ctx.pattern_collector.push((self.bind_name.clone(), other.clone_data()));
                return Ok(ThreeValuedLogic::True);
            } else {
                return Err(TypeError::GenericLayerOverflow(other.clone_data().into()));
            })
            .as_ref_dispatcher(),
            ctx,
        )
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Pattern<T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new<S: Into<Arc<str>>>(
        bind_name: S,
        bind_layer: usize,
        source_info: Option<Arc<SourceLocation>>,
    ) -> Self {
        Self {
            bind_name: bind_name.into(),
            bind_layer,
            source_info,
            _phantom: std::marker::PhantomData,
        }
    }

    pub fn bind_name(&self) -> &Arc<str> {
        &self.bind_name
    }

    pub fn bind_layer(&self) -> usize {
        self.bind_layer
    }
}
