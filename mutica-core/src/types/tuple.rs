use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::{
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, Type, TypeCheckContext, TypeError, TypeRef,
        type_bound::TypeBound,
    },
    util::cycle_detector::FastCycleDetector,
};

pub struct Tuple<T: GcAllocObject<T>> {
    types: Arc<[Type<T>]>,
    is_nf: bool,
}

impl<T: GcAllocObject<T>> Clone for Tuple<T> {
    fn clone(&self) -> Self {
        Self {
            types: self.types.clone(),
            is_nf: self.is_nf,
        }
    }
}

impl<T: GcAllocObject<T>> GCTraceable<T> for Tuple<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        for v in self.types.iter() {
            v.collect(queue);
        }
    }
}

impl<T: GcAllocObject<T>> GcAllocObject<T> for Tuple<T> {}

impl<T: GcAllocObject<T>> AsDispatcher<Type<T>, T> for Tuple<T> {
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

impl<T: GcAllocObject<T>> CoinductiveType<Type<T>, T> for Tuple<T> {
    fn is(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<Option<()>, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env);
            match other {
                TypeRef::Bound(TypeBound::Top) => Ok(Some(())),
                TypeRef::Tuple(other_types) => {
                    if self.types.len() != other_types.len() {
                        return Ok(None);
                    }
                    for (self_type, other_type) in self.types.iter().zip(other_types.types.iter()) {
                        if self_type.is(other_type, &mut inner_ctx)?.is_none() {
                            return Ok(None);
                        }
                    }
                    Ok(Some(()))
                }
                TypeRef::List(v) => {
                    if self.is_empty() && v.len() == 0 {
                        return Ok(Some(()));
                    }
                    if self.len() != 2 || v.len() == 0 {
                        return Ok(None);
                    }
                    let first = &self.types[0];
                    let head = v.head().unwrap();
                    if first.is(head, &mut inner_ctx)?.is_none() {
                        return Ok(None);
                    }
                    self.types[1].is(&v.view(1), &mut inner_ctx)
                }
                TypeRef::Specialize(v) => v.has(self, &mut inner_ctx),
                TypeRef::Generalize(v) => v.has(self, &mut inner_ctx),
                TypeRef::FixPoint(v) => v.has(self, &mut inner_ctx),
                TypeRef::Pattern(v) => v.has(self, &mut inner_ctx),
                TypeRef::Variable(v) => v.has(self, &mut inner_ctx),
                _ => Ok(None),
            }
        })
    }

    fn reduce(
        self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, super::TypeError<Type<T>, T>> {
        let mut result = smallvec::SmallVec::<[Type<T>; 8]>::new();
        for sub in self.types.into_iter() {
            result.push(sub.clone().reduce(ctx)?);
        }

        Ok(Self::new(&result))
    }

    fn invoke(
        &self,
        ctx: &mut InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, super::TypeError<Type<T>, T>> {
        ctx.arg
            .map(&mut FastCycleDetector::new(), |_, arg| match arg {
                Type::IntegerValue(iv) => {
                    if self.types.is_empty() {
                        return Err(super::TypeError::TupleIndexOutOfBounds(Box::new((
                            self.clone().dispatch(),
                            ctx.arg.clone(),
                        ))));
                    }
                    let index = iv.value() as usize;
                    if index >= self.types.len() {
                        return Err(super::TypeError::TupleIndexOutOfBounds(Box::new((
                            self.clone().dispatch(),
                            ctx.arg.clone(),
                        ))));
                    }
                    Ok(self.types[index].clone())
                }
                _ => Err(super::TypeError::TypeMismatch(
                    (ctx.arg.clone(), "IntegerValue".into()).into(),
                )),
            })?
    }

    fn is_normal_form(&self) -> bool {
        self.is_nf
    }
}

impl<T: GcAllocObject<T>> Rootable<T> for Tuple<T> {
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        for ty in self.types.iter() {
            ty.upgrade(collected);
        }
    }
}

impl<T: GcAllocObject<T>> Representable for Tuple<T> {
    fn represent(
        &self,
        path: &mut crate::util::cycle_detector::FastCycleDetector<*const ()>,
    ) -> String {
        let mut result = String::new();
        result.push('(');
        for (i, ty) in self.types.iter().enumerate() {
            result.push_str(&ty.represent(path));
            if self.types.len() - 1 != i || self.types.len() == 1 {
                result.push_str(", ");
            }
        }
        result.push(')');
        result
    }
}

impl<T: GcAllocObject<T>> Tuple<T> {
    pub fn new<I, U>(types: I) -> Type<T>
    where
        I: IntoIterator<Item = U>,
        U: AsDispatcher<Type<T>, T>,
    {
        let types = types
            .into_iter()
            .map(|t| t.into_dispatcher())
            .collect::<Arc<[Type<T>]>>();
        let is_nf = types.iter().all(|t| t.is_normal_form());
        Self { types, is_nf }.dispatch()
    }

    pub fn types(&self) -> &[Type<T>] {
        &self.types
    }

    pub fn len(&self) -> usize {
        self.types.len()
    }

    pub fn is_empty(&self) -> bool {
        self.types.is_empty()
    }
}
