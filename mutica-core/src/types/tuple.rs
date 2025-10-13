use std::sync::Arc;

use arc_gc::traceable::GCTraceable;

use crate::{
    types::{
        AsTypeRef, CoinductiveType, CoinductiveTypeWithAny, InvokeContext, ReductionContext,
        Representable, Rootable, StabilizedType, Type, TypeCheckContext, TypeError,
        fixpoint::FixPointInner, type_bound::TypeBound,
    },
    util::cycle_detector::FastCycleDetector,
};

#[derive(Clone)]
pub struct Tuple {
    types: Arc<[Type]>,
}

impl GCTraceable<FixPointInner> for Tuple {
    fn collect(
        &self,
        queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<FixPointInner>>,
    ) {
        for v in self.types.iter() {
            v.collect(queue);
        }
    }
}

impl CoinductiveType<Type, StabilizedType> for Tuple {
    fn dispatch(self) -> Type {
        Type::Tuple(self)
    }

    fn is(&self, other: &Type, ctx: &mut TypeCheckContext) -> Result<Option<()>, TypeError> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx = TypeCheckContext::new(
                ctx.assumptions,
                ctx.closure_env,
                pattern_env,
                ctx.pattern_mode,
            );
            match other {
                Type::Bound(TypeBound::Top) => Ok(Some(())),
                Type::Tuple(other_types) => {
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
                Type::List(v) => {
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
        let mut result = smallvec::SmallVec::<[StabilizedType; 8]>::new();
        for sub in self.types.iter() {
            result.push(sub.reduce(ctx)?);
        }

        Ok(Self::new(&result))
    }

    fn invoke(&self, ctx: &mut InvokeContext) -> Result<StabilizedType, super::TypeError> {
        ctx.arg
            .map(&mut FastCycleDetector::new(), |_, arg| match arg {
                Type::IntegerValue(iv) => {
                    if self.types.is_empty() {
                        return Err(super::TypeError::TupleIndexOutOfBounds(Box::new((
                            self.clone().dispatch().stabilize(),
                            ctx.arg.clone().stabilize(),
                        ))));
                    }
                    let index = iv.value() as usize;
                    if index >= self.types.len() {
                        return Err(super::TypeError::TupleIndexOutOfBounds(Box::new((
                            self.clone().dispatch().stabilize(),
                            ctx.arg.clone().stabilize(),
                        ))));
                    }
                    Ok(self.types[index].clone().stabilize())
                }
                _ => Err(super::TypeError::TypeMismatch(
                    Box::new(ctx.arg.clone().stabilize()),
                    "IntegerValue".to_string(),
                )),
            })?
    }
}

impl Rootable for Tuple {
    fn upgrade(&self, collected: &mut smallvec::SmallVec<[arc_gc::arc::GCArc<FixPointInner>; 8]>) {
        for ty in self.types.iter() {
            ty.upgrade(collected);
        }
    }
}

impl Representable for Tuple {
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

impl Tuple {
    pub fn new<I, T>(types: I) -> StabilizedType
    where
        I: IntoIterator<Item = T>,
        T: AsTypeRef,
    {
        let types = types
            .into_iter()
            .map(|t| t.into_type())
            .collect::<Arc<[Type]>>();
        Tuple { types }.dispatch().stabilize()
    }

    pub fn types(&self) -> &Arc<[Type]> {
        &self.types
    }

    pub fn len(&self) -> usize {
        self.types.len()
    }

    pub fn is_empty(&self) -> bool {
        self.types.is_empty()
    }
}
