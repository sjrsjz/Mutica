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

pub struct Tuple<T: GcAllocObject<T, Inner = Type<T>>> {
    types: Arc<[Type<T>]>,
    is_nf: Arc<RwLock<ThreeValuedLogic>>,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Tuple<T> {
    fn clone(&self) -> Self {
        Self {
            types: self.types.clone(),
            is_nf: self.is_nf.clone(),
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> GCTraceable<T> for Tuple<T> {
    fn collect(&self, queue: &mut std::collections::VecDeque<arc_gc::arc::GCArcWeak<T>>) {
        for v in self.types.iter() {
            v.collect(queue);
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
                TypeRef::Tuple(other_types) => {
                    if self.types.len() != other_types.len() {
                        return Ok(None);
                    }
                    for (self_type, other_type) in self.types.iter().zip(other_types.types.iter()) {
                        if self_type
                            .fulfill(other_type.as_ref_dispatcher(), &mut inner_ctx)?
                            .is_none()
                        {
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
                    if first
                        .fulfill(head.as_ref_dispatcher(), &mut inner_ctx)?
                        .is_none()
                    {
                        return Ok(None);
                    }
                    self.types[1].fulfill(v.view(1).as_ref_dispatcher(), &mut inner_ctx)
                }
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
                TypeRef::IntegerValue(iv) => {
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

    fn is_normal_form(&self) -> ThreeValuedLogic {
        match self.is_nf.read() {
            Ok(v) => v.clone(),
            Err(_) => ThreeValuedLogic::False,
        }
    }

    fn recalculate_normal_form(&self, cycle_detector: &mut FastCycleDetector<TaggedPtr<()>>) {
        let mut new_nf = ThreeValuedLogic::True;
        for ty in self.types.iter() {
            ty.recalculate_normal_form(cycle_detector);
            new_nf &= ty.is_normal_form();
        }
        if let Ok(mut nf_lock) = self.is_nf.write() {
            *nf_lock = new_nf;
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Tuple<T> {
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        for ty in self.types.iter() {
            ty.upgrade(collected);
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Tuple<T> {
    fn represent(
        &self,
        path: &mut crate::util::cycle_detector::FastCycleDetector<TaggedPtr<()>>,
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

impl<T: GcAllocObject<T, Inner = Type<T>>> Tuple<T> {
    pub fn new<I, U>(types: I) -> Type<T>
    where
        I: IntoIterator<Item = U>,
        U: AsDispatcher<Type<T>, T>,
    {
        let types = types
            .into_iter()
            .map(|t| t.into_dispatcher())
            .collect::<Arc<[Type<T>]>>();
        let mut is_nf = ThreeValuedLogic::True;
        for ty in types.iter() {
            is_nf &= ty.is_normal_form();
        }
        Self {
            types,
            is_nf: Arc::new(RwLock::new(is_nf)),
        }
        .dispatch()
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
