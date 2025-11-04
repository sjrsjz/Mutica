use std::sync::RwLock;

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, Representable,
        Rootable, TaggedPtr, Type, TypeCheckContext, TypeRef, type_bound::TypeBound,
    },
    util::{
        arc_opt::ArcOpt, cycle_detector::FastCycleDetector, three_valued_logic::ThreeValuedLogic,
    },
};

pub struct Construct<T: GcAllocObject<T, Inner = Type<T>>> {
    inner: ArcOpt<(Type<T>, Type<T>, RwLock<ThreeValuedLogic>)>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Construct<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Construct<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        let (head, tail, _) = self.inner.as_ref();
        head.collect(queue);
        tail.collect(queue);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Construct<T> {
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        let (head, tail, _) = self.inner.as_ref();
        head.upgrade(collected);
        tail.upgrade(collected);
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Construct<T> {
    fn represent(
        &self,
        path: &mut crate::util::cycle_detector::FastCycleDetector<TaggedPtr<()>>,
    ) -> String {
        let (head, tail, _) = self.inner.as_ref();
        format!("Cons<{}, {}>", head.represent(path), tail.represent(path))
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
                TypeRef::EqType(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Bound(TypeBound::Top) => Ok(true),
                TypeRef::Construct(v) => {
                    let (head, tail, _) = self.inner.as_ref();
                    let (v_head, v_tail, _) = v.inner.as_ref();
                    Ok(head.check(v_head.as_ref_dispatcher(), &mut inner_ctx)?
                        && tail.check(v_tail.as_ref_dispatcher(), &mut inner_ctx)?)
                }
                TypeRef::Tuple(v) => {
                    if v.is_empty() {
                        // 空元组无法匹配任何构造
                        return Ok(false);
                    }
                    let head = v.get(0).unwrap();
                    let tail = v.tail().unwrap();
                    let (self_head, self_tail, _) = self.inner.as_ref();
                    Ok(self_head.check(head.as_ref_dispatcher(), &mut inner_ctx)?
                        && self_tail.check(tail.as_ref_dispatcher(), &mut inner_ctx)?)
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

                TypeRef::Construct(v) => {
                    let (head, tail, _) = self.inner.as_ref();
                    let (v_head, v_tail, _) = v.inner.as_ref();
                    Ok(head.equals(v_head.as_ref_dispatcher(), &mut inner_ctx)?
                        && tail.equals(v_tail.as_ref_dispatcher(), &mut inner_ctx)?)
                }
                TypeRef::Tuple(v) => {
                    if v.is_empty() {
                        // 空元组无法匹配任何构造
                        return Ok(false);
                    }
                    let head = v.get(0).unwrap();
                    let tail = v.tail().unwrap();
                    let (self_head, self_tail, _) = self.inner.as_ref();
                    Ok(self_head.equals(head.as_ref_dispatcher(), &mut inner_ctx)?
                        && self_tail.equals(tail.as_ref_dispatcher(), &mut inner_ctx)?)
                }
                _ => Ok(false),
            }
        })
    }

    fn reduce(
        mut self,
        ctx: &mut super::ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, super::TypeError<Type<T>, T>> {
        match self.inner.modify(|(head, tail, is_nf)| {
            let new_head = head.reduce(ctx)?;
            let new_tail = tail.reduce(ctx)?;
            let new_is_nf = new_head.is_normal_form() & new_tail.is_normal_form();
            if let Ok(mut nf_lock) = is_nf.write() {
                *nf_lock = new_is_nf;
            }
            Ok((new_head, new_tail, is_nf))
        })? {
            Some(()) => Ok(self.dispatch()),
            None => {
                let (head, tail, _) = self.inner.as_ref();
                let new_head = head.clone().reduce(ctx)?;
                let new_tail = tail.clone().reduce(ctx)?;
                Ok(Self::new(new_head, new_tail))
            }
        }
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
        match self.inner.as_ref().2.read() {
            Ok(v) => *v,
            Err(_) => ThreeValuedLogic::False,
        }
    }

    fn recalculate_normal_form(&self, cycle_detector: &mut FastCycleDetector<TaggedPtr<()>>) {
        let (head, tail, is_nf) = self.inner.as_ref();
        head.recalculate_normal_form(cycle_detector);
        tail.recalculate_normal_form(cycle_detector);
        let new_nf = head.is_normal_form() & tail.is_normal_form();
        if let Ok(mut nf_lock) = is_nf.write() {
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
            inner: ArcOpt::new((head, tail, RwLock::new(is_nf))),
        }
        .dispatch()
    }

    pub fn head(&self) -> &Type<T> {
        &self.inner.as_ref().0
    }

    pub fn tail(&self) -> &Type<T> {
        &self.inner.as_ref().1
    }
}
