use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::types::{
    AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
    ReductionContext, Representable, Rootable, TaggedPtr, Type, TypeCheckContext, TypeError,
    TypeRef, type_bound::TypeBound,
};

// 抽象链表类型，实际实现为 Vec<T>
// 逻辑等价为 (T_1, (T_2, (T_3, ...)))
pub struct List<T: GcAllocObject<T, Inner = Type<T>>> {
    elements: Arc<Vec<Type<T>>>,
    head: usize,
    is_nf: bool,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for List<T> {
    fn clone(&self) -> Self {
        Self {
            elements: self.elements.clone(),
            head: self.head,
            is_nf: self.is_nf,
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for List<T> {
    fn represent(
        &self,
        path: &mut crate::util::cycle_detector::FastCycleDetector<*const ()>,
    ) -> String {
        let mut repr = String::from("@(");
        for (i, element) in self.iter().enumerate() {
            if i != 0 {
                repr.push_str(", ");
            }
            repr.push_str(&element.represent(path));
        }
        repr.push(')');
        repr
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for List<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        for element in self.iter() {
            // 我们不关心 head 之前的元素，他们对于本类型是不可达的
            element.collect(queue);
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for List<T> {
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        for element in self.iter() {
            // 我们不关心 head 之前的元素，他们对于本类型是不可达的
            element.upgrade(collected);
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GcAllocObject<T> for List<T> {
    type Inner = Type<T>;
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for List<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;

    fn into_dispatcher(self) -> Type<T> {
        Type::List(self)
    }

    fn as_ref_dispatcher<'a>(&'a self) -> Self::RefDispatcher<'a> {
        TypeRef::List(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for List<T> {
    fn fulfill(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<Option<()>, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env);
            match other {
                TypeRef::Specialize(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Generalize(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Neg(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Rot(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Bound(TypeBound::Top) => Ok(Some(())),
                TypeRef::List(v) => {
                    if self.len() != v.len() {
                        return Ok(None);
                    }
                    for (a, b) in self.iter().zip(v.iter()) {
                        if !a.fulfill(b.as_ref_dispatcher(), &mut inner_ctx)?.is_some() {
                            return Ok(None);
                        }
                    }
                    Ok(Some(()))
                }
                TypeRef::Tuple(v) => {
                    if self.len() == 0 && v.is_empty() {
                        return Ok(Some(()));
                    }
                    if self.len() == 0 || v.len() != 2 {
                        return Ok(None);
                    }
                    let head = self.head().unwrap();
                    let first = &v.types()[0];
                    if !head
                        .fulfill(first.as_ref_dispatcher(), &mut inner_ctx)?
                        .is_some()
                    {
                        return Ok(None);
                    }
                    let view = self.view(1);
                    let second = &v.types()[1];
                    view.fulfill(second.as_ref_dispatcher(), &mut inner_ctx)
                }
                _ => Ok(None),
            }
        })
    }

    fn reduce(
        self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, super::TypeError<Type<T>, T>> {
        let mut reduced_elements = Vec::with_capacity(self.len());
        for element in self.iter() {
            reduced_elements.push(element.clone().reduce(ctx)?);
        }
        Ok(Self::new(reduced_elements))
    }

    fn invoke(
        &self,
        ctx: &mut InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, super::TypeError<Type<T>, T>> {
        match ctx.arg {
            Type::IntegerValue(iv) => match iv.value() {
                0 => self.head().map(|t| t.clone()).ok_or_else(|| {
                    TypeError::TupleIndexOutOfBounds(Box::new((
                        self.clone().dispatch(),
                        ctx.arg.clone(),
                    )))
                }),
                1 => self.tail().ok_or_else(|| {
                    TypeError::TupleIndexOutOfBounds(Box::new((
                        self.clone().dispatch(),
                        ctx.arg.clone(),
                    )))
                }),
                _ => Err(TypeError::TupleIndexOutOfBounds(Box::new((
                    self.clone().dispatch(),
                    ctx.arg.clone(),
                )))),
            },
            _ => Ok(TypeBound::bottom()),
        }
    }

    fn tagged_ptr(&self) -> TaggedPtr<()> {
        TaggedPtr::new(self.elements.as_ref().as_ptr() as *const (), self.head)
    }

    fn is_normal_form(&self) -> bool {
        self.is_nf
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> List<T> {
    pub fn len(&self) -> usize {
        self.elements.len() - self.head
    }

    pub fn iter(&self) -> impl Iterator<Item = &Type<T>> {
        self.elements.iter().skip(self.head)
    }

    pub fn get(&self, index: usize) -> Option<&Type<T>> {
        if index >= self.len() {
            return None;
        }
        self.elements.get(self.head + index)
    }

    pub fn new<I, X>(types: I) -> Type<T>
    where
        I: IntoIterator<Item = X>,
        X: AsDispatcher<Type<T>, T>,
    {
        let elements: Vec<Type<T>> = types.into_iter().map(|t| t.into_dispatcher()).collect();
        let is_nf = elements.iter().all(|t| t.is_normal_form());
        Self {
            elements: Arc::from(elements),
            head: 0,
            is_nf,
        }
        .dispatch()
    }

    pub fn view(&self, start: usize) -> Type<T> {
        if start > self.len() {
            panic!("List view start index out of bounds");
        }
        let is_nf = self.is_nf || self.iter().skip(start).all(|e| e.is_normal_form());
        Self {
            elements: self.elements.clone(),
            head: self.head + start,
            is_nf,
        }
        .dispatch()
    }

    pub fn head(&self) -> Option<&Type<T>> {
        self.iter().next()
    }

    pub fn tail(&self) -> Option<Type<T>> {
        if self.len() == 0 {
            return None;
        }
        Some(self.view(1))
    }
}
