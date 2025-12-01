use core::panic;
use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::{
    test_true,
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, TaggedPtr, Type, TypeCheckContext, TypeError,
        TypeRef, type_bound::TypeBound,
    },
    util::{
        arc_opt::ArcOpt, cycle_detector::FastCycleDetector, source_info::SourceLocation,
        three_valued_logic::ThreeValuedLogic,
    },
};

// 元组类型，但是其允许通过cons类型解构
pub struct Tuple<T: GcAllocObject<T, Inner = Type<T>>> {
    elements: ArcOpt<Vec<Type<T>>>,
    source_info: Option<Arc<SourceLocation>>,
    head: usize,
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Tuple<T> {
    fn clone(&self) -> Self {
        Self {
            elements: self.elements.clone(),
            source_info: self.source_info.clone(),
            head: self.head,
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Representable for Tuple<T> {
    fn represent(
        &self,
        path: &mut crate::util::cycle_detector::FastCycleDetector<TaggedPtr<()>>,
        depth: usize,
        max_depth: usize,
    ) -> String {
        if depth > max_depth {
            return "...".to_string();
        }
        let mut repr = String::from("(");
        for (i, element) in self.iter().enumerate() {
            repr.push_str(&element.represent(path, depth + 1, max_depth));
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
    fn check(
        &self,
        other: TypeRef<T>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.rhs);
            match other {
                TypeRef::All(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Any(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::EqOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::SubOf(v) => v.accept(self.as_ref_dispatcher(), &mut inner_ctx),

                TypeRef::Bound(v)
                    if matches!(&v.kind, crate::types::type_bound::TypeBoundKind::Top) =>
                {
                    Ok(ThreeValuedLogic::True)
                }
                TypeRef::Tuple(v) => {
                    if self.len() != v.len() {
                        return Ok(ThreeValuedLogic::False);
                    }
                    let mut all = ThreeValuedLogic::True;
                    for (a, b) in self.iter().zip(v.iter()) {
                        all &= test_true!(a.check(b.as_ref_dispatcher(), &mut inner_ctx)?)
                    }
                    Ok(all)
                }
                TypeRef::Construct(cons) => {
                    if self.is_empty() {
                        // 空元组无法匹配任何构造
                        return Ok(ThreeValuedLogic::False);
                    }
                    let head = cons.head();
                    let tail = cons.tail();
                    // 多元素元组匹配构造
                    Ok(test_true!(
                        self.head()
                            .unwrap()
                            .check(head.as_ref_dispatcher(), &mut inner_ctx)?
                    ) & test_true!(
                        self.tail()
                            .unwrap()
                            .check(tail.as_ref_dispatcher(), &mut inner_ctx)?
                    ))
                }
                _ => Ok(ThreeValuedLogic::False),
            }
        })
    }

    fn subof(
        &self,
        other: Self::RefDispatcher<'_>,
        ctx: &mut TypeCheckContext<Type<T>, T>,
    ) -> Result<ThreeValuedLogic, TypeError<Type<T>, T>> {
        ctx.pattern_env.collect(|pattern_env| {
            let mut inner_ctx =
                TypeCheckContext::new(ctx.assumptions, ctx.closure_env, pattern_env, ctx.rhs);
            match other {
                TypeRef::Any(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::All(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::FixPoint(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Pattern(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Variable(v) => v.superof(self.as_ref_dispatcher(), &mut inner_ctx),
                TypeRef::Bound(v)
                    if matches!(&v.kind, crate::types::type_bound::TypeBoundKind::Top) =>
                {
                    Ok(ThreeValuedLogic::True)
                }
                TypeRef::Tuple(v) => {
                    if self.len() != v.len() {
                        return Ok(ThreeValuedLogic::False);
                    }
                    let mut all = ThreeValuedLogic::True;
                    for (a, b) in self.iter().zip(v.iter()) {
                        all &= test_true!(a.subof(b.as_ref_dispatcher(), &mut inner_ctx)?);
                    }
                    Ok(all)
                }
                TypeRef::Construct(cons) => {
                    if self.is_empty() {
                        // 空元组无法匹配任何构造
                        return Ok(ThreeValuedLogic::False);
                    }
                    let head = cons.head();
                    let tail = cons.tail();
                    // 多元素元组匹配构造
                    Ok(test_true!(
                        self.head()
                            .unwrap()
                            .subof(head.as_ref_dispatcher(), &mut inner_ctx)?
                    ) & test_true!(
                        self.tail()
                            .unwrap()
                            .subof(tail.as_ref_dispatcher(), &mut inner_ctx)?
                    ))
                }
                _ => Ok(ThreeValuedLogic::False),
            }
        })
    }

    fn reduce(
        mut self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, super::TypeError<Type<T>, T>> {
        match self.elements.modify(|mut elements| {
            for element in elements.iter_mut().skip(self.head) {
                // 手动提供一个占位符值，然后换出旧值
                let owned_element = std::mem::replace(element, TypeBound::bottom(None));
                let reduced = owned_element.reduce(ctx)?;
                // 将计算结果写回
                *element = reduced;
            }
            Ok(elements)
        })? {
            Some(()) => Ok(self.dispatch()),
            None => {
                let new_elements = self
                    .types()
                    .iter()
                    .map(|t| t.clone().reduce(ctx))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Self::new(new_elements, self.source_info.clone()))
            }
        }
    }

    fn invoke(
        self,
        ctx: InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, super::TypeError<Type<T>, T>> {
        ctx.arg
            .take(&mut FastCycleDetector::new(), |_, arg| match arg {
                Type::NatureNumber(iv) => {
                    let index = iv.value();
                    match self.get(index) {
                        Some(t) => Ok(t.clone()),
                        None => Err(super::TypeError::TupleIndexOutOfBounds(
                            (self.dispatch(), iv.dispatch()).into(),
                        )),
                    }
                }
                _ => Ok(TypeBound::bottom(ctx.source_info.cloned())),
            })?
            .unwrap_or(Err(TypeError::UnresolvableType(
                "Could not resolve argument".into(),
            )))
    }

    fn tagged_ptr(&self) -> TaggedPtr<()> {
        TaggedPtr::new(self.elements.as_ref() as *const _ as *const (), self.head)
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        self.source_info.as_ref()
    }

    fn report_source_info(&self) -> crate::types::TypeReport {
        if let Some(loc) = &self.source_info {
            let span = loc.span().clone();
            let filepath = loc.source().filepath().to_string();
            ariadne::Report::build(ariadne::ReportKind::Error, filepath.clone(), span.start)
                .with_message(format!("Tuple type at {}", filepath))
                .with_label(
                    ariadne::Label::new((filepath, span)).with_message("Tuple defined here"),
                )
                .finish()
        } else {
            ariadne::Report::build(ariadne::ReportKind::Error, "<unknown>".to_string(), 0)
                .with_message("Tuple type has no source location")
                .with_label(
                    ariadne::Label::new(("<unknown>".to_string(), 0..0))
                        .with_message("Location unknown"),
                )
                .finish()
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Tuple<T> {
    pub fn len(&self) -> usize {
        self.elements.as_ref().len() - self.head
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn iter(&self) -> impl Iterator<Item = &Type<T>> {
        self.elements.as_ref().iter().skip(self.head)
    }

    pub fn types(&self) -> &[Type<T>] {
        self.elements.as_ref()[self.head..].as_ref()
    }

    pub fn get(&self, index: usize) -> Option<&Type<T>> {
        if index >= self.len() {
            return None;
        }
        self.elements.as_ref().get(self.head + index)
    }

    #[allow(clippy::new_ret_no_self)]
    pub fn new<I, X>(types: I, source_info: Option<Arc<SourceLocation>>) -> Type<T>
    where
        I: IntoIterator<Item = X>,
        X: AsDispatcher<Type<T>, T>,
    {
        let elements: Vec<Type<T>> = types.into_iter().map(|t| t.into_dispatcher()).collect();
        Self {
            elements: ArcOpt::new(elements),
            head: 0,
            source_info,
        }
        .dispatch()
    }

    pub fn view(&self, start: usize) -> Type<T> {
        if start > self.len() {
            panic!("List view start index out of bounds");
        }
        Self {
            elements: self.elements.clone(),
            head: self.head + start,
            source_info: self.source_info.clone(),
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

    pub fn concat(self, other: Tuple<T>, source_info: Option<Arc<SourceLocation>>) -> Type<T> {
        let mut new_elements = self.take();
        new_elements.extend(other.take());
        Self {
            elements: ArcOpt::new(new_elements),
            head: 0,
            source_info,
        }
        .dispatch()
    }

    pub fn take(self) -> Vec<Type<T>> {
        match self.elements.take() {
            Ok(mut vec) => {
                // 拥有唯一所有权,直接 drain 前面的元素
                vec.drain(..self.head);
                vec
            }
            Err(arc) => {
                // 共享引用,只克隆需要的部分
                arc.as_ref()[self.head..].to_vec()
            }
        }
    }

    pub fn unit() -> Type<T> {
        Self::new(std::iter::empty::<Type<T>>(), None)
    }
}
