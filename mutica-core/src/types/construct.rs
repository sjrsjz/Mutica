use std::sync::{Arc, RwLock};

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, Representable,
        Rootable, TaggedPtr, Type, TypeCheckContext, TypeRef, type_bound::TypeBound,
    },
    util::{cycle_detector::FastCycleDetector, three_valued_logic::ThreeValuedLogic},
};

pub struct Construct<T: GcAllocObject<T, Inner = Type<T>>> {
    head: Arc<Type<T>>,
    tail: Arc<Type<T>>,
    is_nf: Arc<RwLock<ThreeValuedLogic>>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Construct<T> {
    fn clone(&self) -> Self {
        Self {
            head: self.head.clone(),
            tail: self.tail.clone(),
            is_nf: self.is_nf.clone(),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Construct<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        self.head.collect(queue);
        self.tail.collect(queue);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Construct<T> {
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        self.head.upgrade(collected);
        self.tail.upgrade(collected);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Construct<T> {
    fn represent(
        &self,
        path: &mut crate::util::cycle_detector::FastCycleDetector<TaggedPtr<()>>,
    ) -> String {
        format!(
            "Cons<{}, {}>",
            self.head.represent(path),
            self.tail.represent(path)
        )
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
    ) -> Result<bool, super::TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.rhs);
            match other {
                TypeRef::Generalize(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Specialize(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Bound(TypeBound::Top) => Ok(true),
                TypeRef::Construct(v) => Ok(self
                    .head
                    .check(v.head.as_ref_dispatcher(), &mut inner_ctx)?
                    && self
                        .tail
                        .check(v.tail.as_ref_dispatcher(), &mut inner_ctx)?),
                TypeRef::Tuple(v) => {
                    if v.is_empty() {
                        // 空元组无法匹配任何构造
                        return Ok(false);
                    }
                    let head = v.get(0).unwrap();
                    let tail = v.tail().unwrap();
                    Ok(self.head.check(head.as_ref_dispatcher(), &mut inner_ctx)?
                        && self.tail.check(tail.as_ref_dispatcher(), &mut inner_ctx)?)
                }
                _ => Ok(false),
            }
        })
    }

    fn equals(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut super::TypeCheckContext<Type<T>, T>,
    ) -> Result<bool, super::TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.rhs);
            match other {
                TypeRef::FixPoint(v) => v.equals_any(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.equals_any(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.equals_any(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::EqType(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Construct(v) => Ok(self
                    .head
                    .equals(v.head.as_ref_dispatcher(), &mut inner_ctx)?
                    && self
                        .tail
                        .equals(v.tail.as_ref_dispatcher(), &mut inner_ctx)?),
                TypeRef::Tuple(v) => {
                    if v.is_empty() {
                        // 空元组无法匹配任何构造
                        return Ok(false);
                    }
                    let head = v.get(0).unwrap();
                    let tail = v.tail().unwrap();
                    Ok(self.head.equals(head.as_ref_dispatcher(), &mut inner_ctx)?
                        && self.tail.equals(tail.as_ref_dispatcher(), &mut inner_ctx)?)
                }
                _ => Ok(false),
            }
        })
    }

    fn reduce(
        self,
        ctx: &mut super::ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, super::TypeError<Type<T>, T>> {
        let head = self.head.as_ref().clone().reduce(ctx)?;
        let tail = self.tail.as_ref().clone().reduce(ctx)?;
        Ok(Self::new(head, tail))
    }

    fn invoke(
        &self,
        _ctx: &mut super::InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, super::TypeError<Type<T>, T>> {
        Err(super::TypeError::NonApplicableType(
            self.clone().dispatch().into(),
        ))
    }

    fn is_normal_form(&self) -> ThreeValuedLogic {
        match self.is_nf.read() {
            Ok(v) => *v,
            Err(_) => ThreeValuedLogic::False,
        }
    }

    fn recalculate_normal_form(&self, cycle_detector: &mut FastCycleDetector<TaggedPtr<()>>) {
        self.head.recalculate_normal_form(cycle_detector);
        self.tail.recalculate_normal_form(cycle_detector);
        let new_nf = self.head.is_normal_form() & self.tail.is_normal_form();
        if let Ok(mut nf_lock) = self.is_nf.write() {
            *nf_lock = new_nf;
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Construct<T> {
    #[allow(clippy::new_ret_no_self)]
    pub fn new<U: AsDispatcher<Type<T>, T>, V: AsDispatcher<Type<T>, T>>(
        head: U,
        tail: V,
    ) -> Type<T> {
        let head = head.into_dispatcher();
        let tail = tail.into_dispatcher();
        let is_nf = head.is_normal_form() & tail.is_normal_form();
        Self {
            head: Arc::new(head),
            tail: Arc::new(tail),
            is_nf: Arc::new(RwLock::new(is_nf)),
        }
        .dispatch()
    }

    pub fn head(&self) -> &Type<T> {
        &self.head
    }

    pub fn tail(&self) -> &Type<T> {
        &self.tail
    }
}
