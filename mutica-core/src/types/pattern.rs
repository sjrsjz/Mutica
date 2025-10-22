use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, Type, TypeCheckContext, TypeError, TypeRef,
    },
    util::{cycle_detector::FastCycleDetector, three_valued_logic::ThreeValuedLogic},
};

// 理论上来说应当把 debruijn_index 直接和 Type 绑定起来（因为Pattern只是一个附加信息）
// 但是为了实现的简洁性，这里就先分开了
pub struct Pattern<T: GcAllocObject<T, Inner = Type<T>>> {
    is_nf: ThreeValuedLogic,
    debruijn_index: usize,
    expr: Arc<Type<T>>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Pattern<T> {
    fn clone(&self) -> Self {
        Self {
            is_nf: self.is_nf,
            debruijn_index: self.debruijn_index,
            expr: self.expr.clone(),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Pattern<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        self.expr.collect(queue);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Pattern<T> {
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        self.expr.upgrade(collected);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Pattern<T> {
    fn represent(
        &self,
        path: &mut crate::util::cycle_detector::FastCycleDetector<*const ()>,
    ) -> String {
        format!("λ.{} : {}", self.debruijn_index, self.expr.represent(path))
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GcAllocObject<T> for Pattern<T> {
    type Inner = Type<T>;
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
    fn fulfill(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<Option<()>, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let enabled = pattern_env.is_enabled();
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env);
            if enabled {
                // 模式泄露到了待匹配的对象而非模式中
                // 这通常意味着编译器的bug
                // 理想情况是直接 panic
                panic!(
                    "CRITICAL: Pattern variable leaked to non-pattern context: {:?}",
                    self.represent(&mut FastCycleDetector::new())
                )
            }
            // // 由于Pattern的特殊性，非模式匹配下的Pattern只能和Pattern进行比较，否则可能破坏alpha等价性
            // match other {
            //     TypeRef::Pattern(v) => {
            //         if self.debruijn_index == v.debruijn_index {
            //             self.expr.is(v.expr.as_ref_dispatcher(), &mut inner_ctx)
            //         } else {
            //             Ok(None)
            //         }
            //     }
            //     _ => Ok(None),
            // }

            // 虽然使用Pattern进行类型检查可以判定alpha等价性
            // 但是它会导致TypeBound的反向子类型关系失效
            // 因此这里直接透过Pattern进行比较
            self.expr.fulfill(other, &mut inner_ctx)
        })
    }

    fn invoke(
        &self,
        _ctx: &mut InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.clone().dispatch().into()))
    }

    fn reduce(
        self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, TypeError<Type<T>, T>> {
        Ok(Self::new(
            self.debruijn_index,
            self.expr.as_ref().clone().reduce(ctx)?,
        ))
    }

    fn tagged_ptr(&self) -> super::TaggedPtr<()> {
        super::TaggedPtr::new_unique(&self as *const _ as *const ())
    }

    fn is_normal_form(&self) -> ThreeValuedLogic {
        self.is_nf
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveTypeWithAny<Type<T>, T> for Pattern<T> {
    #[stacksafe::stacksafe]
    fn accept(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<Option<()>, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            if !pattern_env.is_enabled() {
                // 模式泄露到了非模式匹配的上下文中
                panic!(
                    "CRITICAL: Pattern variable leaked to non-pattern context: {:?}",
                    self.represent(&mut FastCycleDetector::new())
                )
            }
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env);
            if other
                .fullfill(self.expr.as_ref_dispatcher(), &mut inner_ctx)?
                .is_some()
            {
                pattern_env.push((self.debruijn_index, other.clone_data()));
                Ok(Some(()))
            } else {
                Ok(None)
            }
        })
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Pattern<T> {
    pub fn new<X: AsDispatcher<Type<T>, T>>(debruijn_index: usize, expr: X) -> Type<T> {
        let expr = expr.into_dispatcher();
        let is_nf = expr.is_normal_form();
        Self {
            is_nf,
            debruijn_index,
            expr: Arc::new(expr),
        }
        .dispatch()
    }
    pub fn debruijn_index(&self) -> usize {
        self.debruijn_index
    }

    pub fn expr(&self) -> &Type<T> {
        &self.expr
    }
}
