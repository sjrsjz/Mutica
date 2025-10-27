use std::sync::{Arc, RwLock};

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, TaggedPtr, Type, TypeCheckContext, TypeError,
        TypeRef, type_bound::TypeBound,
    },
    util::{cycle_detector::FastCycleDetector, three_valued_logic::ThreeValuedLogic},
};

// 元组类型，但是其允许通过cons类型解构
pub struct Tuple<T: GcAllocObject<T, Inner = Type<T>>> {
    elements: Arc<Vec<Type<T>>>,
    is_nf: Arc<RwLock<ThreeValuedLogic>>,
    head: usize,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Tuple<T> {
    fn clone(&self) -> Self {
        Self {
            elements: self.elements.clone(),
            is_nf: self.is_nf.clone(),
            head: self.head,
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Tuple<T> {
    fn represent(
        &self,
        path: &mut crate::util::cycle_detector::FastCycleDetector<TaggedPtr<()>>,
    ) -> String {
        let mut repr = String::from("(");
        for (i, element) in self.iter().enumerate() {
            repr.push_str(&element.represent(path));
            if i != self.len() - 1 {
                repr.push_str(", ");
            }
            if self.len() == 1 {
                repr.push(',');
            }
        }
        repr.push(')');
        repr
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Tuple<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        for element in self.iter() {
            // 我们不关心 head 之前的元素，他们对于本类型是不可达的
            element.collect(queue);
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Tuple<T> {
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        for element in self.iter() {
            // 我们不关心 head 之前的元素，他们对于本类型是不可达的
            element.upgrade(collected);
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GcAllocObject<T> for Tuple<T> {
    type Inner = Type<T>;
}

impl<T: GcAllocObject<T, Inner = Type<T>>> AsDispatcher<Type<T>, T> for Tuple<T> {
    type RefDispatcher<'a>
        = TypeRef<'a, T>
    where
        Self: 'a;

    fn into_dispatcher(self) -> Type<T> {
        Type::Tuple(self)
    }

    fn as_ref_dispatcher<'a>(&'a self) -> Self::RefDispatcher<'a> {
        TypeRef::Tuple(self)
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> CoinductiveType<Type<T>, T> for Tuple<T> {
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

                TypeRef::Bound(TypeBound::Top) => Ok(Some(())),
                TypeRef::Tuple(v) => {
                    if self.len() != v.len() {
                        return Ok(None);
                    }
                    for (a, b) in self.iter().zip(v.iter()) {
                        if a.fulfill(b.as_ref_dispatcher(), &mut inner_ctx)?.is_none() {
                            return Ok(None);
                        }
                    }
                    Ok(Some(()))
                }
                TypeRef::Construct(cons) => {
                    if self.is_empty() {
                        // 空元组无法匹配任何构造
                        return Ok(None);
                    }
                    let head = cons.head();
                    let tail = cons.tail();
                    // 多元素元组匹配构造
                    let head_result = self
                        .head()
                        .unwrap()
                        .fulfill(head.as_ref_dispatcher(), &mut inner_ctx)?;
                    if head_result.is_none() {
                        return Ok(None);
                    }
                    self.tail()
                        .unwrap()
                        .fulfill(tail.as_ref_dispatcher(), &mut inner_ctx)
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
        ctx.arg
            .map(&mut FastCycleDetector::new(), |_, arg| match arg {
                TypeRef::IntegerValue(iv) => {
                    let index = iv.value() as usize;
                    match self.get(index) {
                        Some(t) => Ok(t.clone()),
                        None => Err(super::TypeError::TupleIndexOutOfBounds(
                            (self.clone().dispatch(), ctx.arg.clone()).into(),
                        )),
                    }
                }
                _ => Ok(TypeBound::bottom()),
            })?
    }

    fn tagged_ptr(&self) -> TaggedPtr<()> {
        TaggedPtr::new(self.elements.as_ref().as_ptr() as *const (), self.head)
    }

    fn is_normal_form(&self) -> ThreeValuedLogic {
        match self.is_nf.read() {
            Ok(v) => *v,
            Err(_) => ThreeValuedLogic::False,
        }
    }

    fn recalculate_normal_form(&self, cycle_detector: &mut FastCycleDetector<TaggedPtr<()>>) {
        let mut new_nf = ThreeValuedLogic::True;
        for element in self.iter() {
            element.recalculate_normal_form(cycle_detector);
            new_nf &= element.is_normal_form();
        }
        if let Ok(mut nf_lock) = self.is_nf.write() {
            *nf_lock = new_nf;
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Tuple<T> {
    pub fn len(&self) -> usize {
        self.elements.len() - self.head
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn iter(&self) -> impl Iterator<Item = &Type<T>> {
        self.elements.iter().skip(self.head)
    }

    pub fn types(&self) -> &[Type<T>] {
        &self.elements[self.head..]
    }

    pub fn get(&self, index: usize) -> Option<&Type<T>> {
        if index >= self.len() {
            return None;
        }
        self.elements.get(self.head + index)
    }

    #[allow(clippy::new_ret_no_self)]
    pub fn new<I, X>(types: I) -> Type<T>
    where
        I: IntoIterator<Item = X>,
        X: AsDispatcher<Type<T>, T>,
    {
        let elements: Vec<Type<T>> = types.into_iter().map(|t| t.into_dispatcher()).collect();
        let mut is_nf = ThreeValuedLogic::True;
        for element in &elements {
            is_nf &= element.is_normal_form();
        }
        Self {
            elements: Arc::from(elements),
            head: 0,
            is_nf: Arc::new(RwLock::new(is_nf)),
        }
        .dispatch()
    }

    pub fn view(&self, start: usize) -> Type<T> {
        if start > self.len() {
            panic!("List view start index out of bounds");
        }
        let is_nf = if self.is_normal_form() == ThreeValuedLogic::True {
            ThreeValuedLogic::True // 删掉这个会导致大部分算法从O(1)变成O(n)，进而严重影响性能
        } else {
            let mut is_nf = ThreeValuedLogic::True;
            for element in self.elements.iter().skip(self.head + start) {
                is_nf &= element.is_normal_form();
            }
            is_nf
        };
        Self {
            elements: self.elements.clone(),
            head: self.head + start,
            is_nf: Arc::new(RwLock::new(is_nf)),
        }
        .dispatch()
    }

    pub fn head(&self) -> Option<&Type<T>> {
        self.iter().next()
    }

    pub fn tail(&self) -> Option<Type<T>> {
        if self.is_empty() {
            return None;
        }
        Some(self.view(1))
    }

    pub fn concat(&self, other: &Tuple<T>) -> Type<T> {
        let mut new_elements = Vec::with_capacity(self.len() + other.len());
        new_elements.extend_from_slice(self.types());
        new_elements.extend_from_slice(other.types());
        let is_nf = self.is_normal_form() & other.is_normal_form();
        Self {
            elements: Arc::new(new_elements),
            head: 0,
            is_nf: Arc::new(RwLock::new(is_nf)),
        }
        .dispatch()
    }
}
