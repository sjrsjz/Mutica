use std::sync::Arc;

use arc_gc::traceable::GCTraceable;

use crate::types::{
        AsTypeRef, CoinductiveType, CoinductiveTypeWithAny, Representable, TypeCheckContext, ReductionContext, InvokeContext, Rootable,
        StabilizedType, TaggedPtr, Type, TypeError,
        fixpoint::FixPointInner,
        type_bound::TypeBound,
    };

// 抽象链表类型，实际实现为 Vec<T>
// 逻辑等价为 (T_1, (T_2, (T_3, ...)))
#[derive(Clone)]
pub struct List {
    elements: Arc<Vec<Type>>,
    head: usize,
}

impl Representable for List {
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

impl GCTraceable<FixPointInner> for List {
    fn collect(
        &self,
        queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<FixPointInner>>,
    ) {
        for element in self.iter() {
            // 我们不关心 head 之前的元素，他们对于本类型是不可达的
            element.collect(queue);
        }
    }
}

impl Rootable for List {
    fn upgrade(&self, collected: &mut smallvec::SmallVec<[arc_gc::arc::GCArc<FixPointInner>; 8]>) {
        for element in self.iter() {
            // 我们不关心 head 之前的元素，他们对于本类型是不可达的
            element.upgrade(collected);
        }
    }
}

impl CoinductiveType<Type, StabilizedType> for List {
    fn dispatch(self) -> Type {
        Type::List(self)
    }

    fn is(&self, other: &Type, ctx: &mut TypeCheckContext) -> Result<Option<()>, TypeError> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.pattern_mode);
            match other {
                Type::List(v) => {
                    if self.len() != v.len() {
                        return Ok(None);
                    }
                    for (a, b) in self.iter().zip(v.iter()) {
                        if !a.is(b, &mut inner_ctx)?.is_some() {
                            return Ok(None);
                        }
                    }
                    Ok(Some(()))
                }
                Type::Tuple(v) => {
                    if self.len() == 0 && v.is_empty() {
                        return Ok(Some(()));
                    }
                    if self.len() == 0 || v.len() != 2 {
                        return Ok(None);
                    }
                    let head = self.head().unwrap();
                    let first = &v.types()[0];
                    if !head.is(first, &mut inner_ctx)?.is_some() {
                        return Ok(None);
                    }
                    let view = self.view(1);
                    let second = &v.types()[1];
                    view.is(second, &mut inner_ctx)
                }
                Type::Bound(TypeBound::Top) => Ok(Some(())),
                Type::Specialize(v) => v.has(self, &mut inner_ctx),
                Type::Generalize(v) => v.has(self, &mut inner_ctx),
                Type::FixPoint(v) => v.has(self, &mut inner_ctx),
                Type::Pattern(v) => v.has(self, &mut inner_ctx),
                Type::Variable(v) => v.has(self, &mut inner_ctx),
                _ => Ok(None),
            }
        })
    }

    fn reduce(&self, ctx: &mut ReductionContext) -> Result<StabilizedType, super::TypeError> {
        let mut reduced_elements = Vec::with_capacity(self.len());
        for element in self.iter() {
            reduced_elements.push(element.reduce(ctx)?);
        }
        Ok(Self::new(reduced_elements))
    }

    fn invoke(&self, ctx: &mut InvokeContext) -> Result<StabilizedType, super::TypeError> {
        match ctx.arg {
            Type::IntegerValue(iv) => match iv.value() {
                0 => self.head().map(|t| t.clone().stabilize()).ok_or_else(|| {
                    TypeError::TupleIndexOutOfBounds(Box::new((
                        self.clone().dispatch().stabilize(),
                        ctx.arg.clone().stabilize(),
                    )))
                }),
                1 => self.tail().ok_or_else(|| {
                    TypeError::TupleIndexOutOfBounds(Box::new((
                        self.clone().dispatch().stabilize(),
                        ctx.arg.clone().stabilize(),
                    )))
                }),
                _ => Err(TypeError::TupleIndexOutOfBounds(Box::new((
                    self.clone().dispatch().stabilize(),
                    ctx.arg.clone().stabilize(),
                )))),
            },
            _ => Ok(TypeBound::bottom()),
        }
    }

    fn tagged_ptr(&self) -> TaggedPtr<()> {
        TaggedPtr::new(self.elements.as_ref().as_ptr() as *const (), self.head)
    }
}

impl List {
    pub fn len(&self) -> usize {
        self.elements.len() - self.head
    }

    pub fn iter(&self) -> impl Iterator<Item = &Type> {
        self.elements.iter().skip(self.head)
    }

    pub fn get(&self, index: usize) -> Option<&Type> {
        if index >= self.len() {
            return None;
        }
        self.elements.get(self.head + index)
    }

    pub fn new<I, T>(types: I) -> StabilizedType
    where
        I: IntoIterator<Item = T>,
        T: AsTypeRef,
    {
        let elements: Vec<Type> = types.into_iter().map(|t| t.into_type()).collect();
        Self {
            elements: Arc::new(elements),
            head: 0,
        }
        .dispatch()
        .stabilize()
    }

    pub fn view(&self, start: usize) -> Type {
        if start > self.len() {
            panic!("List view start index out of bounds");
        }
        Self {
            elements: self.elements.clone(),
            head: self.head + start,
        }
        .dispatch()
    }

    pub fn head(&self) -> Option<&Type> {
        self.iter().next()
    }

    pub fn tail(&self) -> Option<StabilizedType> {
        if self.len() == 0 {
            return None;
        }
        Some(
            Self {
                elements: self.elements.clone(),
                head: self.head + 1,
            }
            .dispatch()
            .stabilize(),
        )
    }
}
