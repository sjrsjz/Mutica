use std::sync::Arc;

use crate::types::CoinductiveTypeRef;
use crate::util::source_info::SourceLocation;
use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, TaggedPtr, Type, TypeCheckContext, TypeError,
        TypeRef,
    },
    util::{cycle_detector::FastCycleDetector, three_valued_logic::ThreeValuedLogic},
};
use arc_gc::traceable::GCTraceable;

pub struct Variable<T: GcAllocObject<T, Inner = Type<T>>> {
    debruijn_index: isize,
    source_info: Option<Arc<SourceLocation>>,
    _phantom: std::marker::PhantomData<T>,
}
impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Variable<T> {
    fn clone(&self) -> Self {
        Self {
            debruijn_index: self.debruijn_index,
            source_info: self.source_info.clone(),
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Variable<T> {
    fn collect(&self, _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {}
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Variable<T> {}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for Variable<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;

    fn into_dispatcher(self) -> Type<T> {
        Type::Variable(self)
    }

    fn as_ref_dispatcher<'a>(&'a self) -> Self::RefDispatcher<'a> {
        TypeRef::Variable(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for Variable<T> {
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
                TypeRef::EqOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::SubOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Variable(v) => {
                    let self_idx = self.debruijn_index;
                    let v_idx = v.debruijn_index;
                    if self_idx >= 0 || v_idx >= 0 {
                        return Ok(ThreeValuedLogic::Unknown);
                    }
                    // 如果都是负数,说明都是闭包内的变量
                    // 需要从闭包环境中取出对应的类型进行比较
                    let l = (-1 - self_idx) as usize;
                    let r = (-1 - v_idx) as usize;

                    let value_l = ctx.closure_env.0.get(l)?;
                    let value_r = ctx.closure_env.1.get(r)?;
                    value_l.check(value_r.as_ref_dispatcher(), &mut inner_ctx)
                }
                _ => {
                    if self.debruijn_index >= 0 {
                        // 如果是正数,说明是参数变量,无法确定
                        // 实际上新模型允许通过上下文推导出参数变量的类型，进而使用Eq来判断
                        return Ok(ThreeValuedLogic::Unknown);
                    }
                    let r = (-1 - self.debruijn_index) as usize;
                    let value = ctx.closure_env.1.get(r)?;
                    value.check(other, &mut inner_ctx)
                }
            }
        })
    }

    fn subof(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.rhs);
            match other {
                TypeRef::FixPoint(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Bound(v)
                    if matches!(&v.kind, crate::types::type_bound::TypeBoundKind::Top) =>
                {
                    Ok(ThreeValuedLogic::True)
                }
                TypeRef::Variable(v) => {
                    let self_idx = self.debruijn_index;
                    let v_idx = v.debruijn_index;
                    if self_idx >= 0 || v_idx >= 0 {
                        return Ok(ThreeValuedLogic::Unknown);
                    }
                    // 如果都是负数,说明都是闭包内的变量
                    // 需要从闭包环境中取出对应的类型进行比较
                    let l = (-1 - self_idx) as usize;
                    let r = (-1 - v_idx) as usize;

                    let value_l = ctx.closure_env.0.get(l)?;
                    let value_r = ctx.closure_env.1.get(r)?;
                    value_l.subof(value_r.as_ref_dispatcher(), &mut inner_ctx)
                }
                _ => {
                    if self.debruijn_index >= 0 {
                        // 如果是正数,说明是参数变量,无法确定
                        // 实际上新模型允许通过上下文推导出参数变量的类型，进而使用Eq来判断
                        return Ok(ThreeValuedLogic::Unknown);
                    }
                    let r = (-1 - self.debruijn_index) as usize;
                    let value = ctx.closure_env.1.get(r)?;
                    value.subof(other, &mut inner_ctx)
                }
            }
        })
    }

    fn reduce(
        self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        let idx = self.debruijn_index;
        if idx >= 0 {
            ctx.param_env.get(idx as usize).cloned()
        } else {
            ctx.closure_env.get((-1 - idx) as usize).cloned()
        }
    }

    fn invoke(self, _ctx: InvokeContext<Type<T>, T>) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.clone().dispatch().into()))
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        self.source_info.as_ref()
    }

    fn report_source_info(&self) -> crate::types::TypeReport {
        if let Some(loc) = &self.source_info {
            let span = loc.span().clone();
            let filepath = loc.source().filepath().to_string();
            ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                .with_message(format!("Type variable at {}", filepath))
                .with_label(
                    ariadne::Label::new((filepath, span))
                        .with_message("Type variable defined here"),
                )
                .finish()
        } else {
            ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                .with_message("Type variable has no source location")
                .with_label(
                    ariadne::Label::new(("<unknown>".to_string(), 0..0))
                        .with_message("Location unknown"),
                )
                .finish()
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveTypeWithAny<Type<T>, T> for Variable<T> {
    #[stacksafe::stacksafe]
    fn accept(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            if self.debruijn_index >= 0 {
                Ok(ThreeValuedLogic::Unknown)
            } else {
                let r = (-1 - self.debruijn_index) as usize;
                let value = ctx.closure_env.1.get(r)?;
                let mut inner_ctx =
                    TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.rhs);
                other.check(value.as_ref_dispatcher(), &mut inner_ctx)
            }
        })
    }

    #[stacksafe::stacksafe]
    fn superof(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            if self.debruijn_index >= 0 {
                Ok(ThreeValuedLogic::Unknown)
            } else {
                let r = (-1 - self.debruijn_index) as usize;
                let value = ctx.closure_env.1.get(r)?;
                let mut inner_ctx =
                    TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.rhs);
                other.subof(value.as_ref_dispatcher(), &mut inner_ctx)
            }
        })
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Variable<T> {
    fn represent(
        &self,
        _path: &mut FastCycleDetector<TaggedPtr<()>>,
        _depth: usize,
        _max_depth: usize,
    ) -> String {
        format!("λ.{}", self.debruijn_index)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Variable<T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new(debruijn_index: isize, source_info: Option<Arc<SourceLocation>>) -> Type<T> {
        Variable {
            debruijn_index,
            source_info,
            _phantom: std::marker::PhantomData,
        }
        .dispatch()
    }

    pub fn debruijn_index(&self) -> isize {
        self.debruijn_index
    }
}
