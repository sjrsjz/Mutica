use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, TaggedPtr, Type, TypeCheckContext, TypeError,
        TypeRef, type_bound::TypeBound,
    },
    util::{cycle_detector::FastCycleDetector, three_valued_logic::ThreeValuedLogic},
};
use arc_gc::traceable::GCTraceable;

pub struct Variable<T: GcAllocObject<T, Inner = Type<T>>> {
    debruijn_index: isize,
    _phantom: std::marker::PhantomData<T>,
}
impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Variable<T> {
    fn clone(&self) -> Self {
        Self {
            debruijn_index: self.debruijn_index,
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Variable<T> {
    fn collect(&self, _queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {}
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GcAllocObject<T> for Variable<T> {
    type Inner = Type<T>;
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
    fn fulfill(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<Option<()>, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env);
            match other {
                TypeRef::Generalize(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Specialize(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Variable(v) => {
                    let self_idx = self.debruijn_index;
                    let v_idx = v.debruijn_index;
                    if self_idx >= 0 || v_idx >= 0 {
                        return Ok(if self_idx == v_idx { Some(()) } else { None });
                    }
                    // 如果都是负数,说明都是闭包内的变量
                    // 需要从闭包环境中取出对应的类型进行比较
                    let l = (-1 - self_idx) as usize;
                    let r = (-1 - v_idx) as usize;

                    let value_l = ctx.closure_env.0.get(l)?;
                    let value_r = ctx.closure_env.1.get(r)?;
                    value_l.fulfill(value_r.as_ref_dispatcher(), &mut inner_ctx)
                }
                _ => {
                    if self.debruijn_index >= 0 {
                        // 如果是正数,说明是全局变量,无法确定类型
                        return Ok(None);
                    }
                    let r = (-1 - self.debruijn_index) as usize;
                    let value = ctx.closure_env.1.get(r)?;
                    value.fulfill(other, &mut inner_ctx)
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
            Ok(ctx
                .param_env
                .get(idx as usize).cloned()
                .unwrap_or(TypeBound::bottom()))
        } else {
            ctx.closure_env.get((-1 - idx) as usize).cloned()
        }
    }

    fn invoke(
        &self,
        _ctx: &mut InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.clone().dispatch().into()))
    }

    fn is_normal_form(&self) -> ThreeValuedLogic {
        ThreeValuedLogic::False
    }

    fn recalculate_normal_form(&self, _: &mut FastCycleDetector<TaggedPtr<()>>) {}
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveTypeWithAny<Type<T>, T> for Variable<T> {
    #[stacksafe::stacksafe]
    fn accept(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<Option<()>, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            if self.debruijn_index >= 0 {
                Ok(None)
            } else {
                let r = (-1 - self.debruijn_index) as usize;
                let value = ctx.closure_env.1.get(r)?;
                let mut inner_ctx =
                    TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env);
                other.fullfill(value.as_ref_dispatcher(), &mut inner_ctx)
            }
        })
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Variable<T> {
    fn represent(&self, _path: &mut FastCycleDetector<TaggedPtr<()>>) -> String {
        format!("λ.{}", self.debruijn_index)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Variable<T> {
    pub fn new_debruijn(debruijn_index: isize) -> Type<T> {
        Variable {
            debruijn_index,
            _phantom: std::marker::PhantomData,
        }
        .dispatch()
    }

    pub fn debruijn_index(&self) -> isize {
        self.debruijn_index
    }
}
