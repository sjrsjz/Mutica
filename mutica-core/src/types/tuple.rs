use std::sync::Arc;

use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::{
    test_true,
    types::{
        AsDispatcher, CoinductiveType, CoinductiveTypeWithAny, GcAllocObject, InvokeContext,
        ReductionContext, Representable, Rootable, TaggedPtr, Type, TypeCheckContext, TypeError,
        TypeRef, sequence::SequenceTail, type_bound::TypeBound,
    },
    util::{arc_opt::ArcOpt, source_info::SourceLocation, three_valued_logic::ThreeValuedLogic},
};

// 元组类型，但是其允许通过cons类型解构
// 使用枚举来优化空元组的情况，避免不必要的内存分配
pub enum Tuple<T: GcAllocObject<T, Inner = Type<T>>> {
    /// 空元组 ()，不需要分配任何内存
    Unit {
        source_info: Option<Arc<SourceLocation>>,
    },
    /// 非空元组，包含至少一个元素
    NonEmpty {
        elements: ArcOpt<Vec<Type<T>>>,
        source_info: Option<Arc<SourceLocation>>,
        head: usize,
    },
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Clone for Tuple<T> {
    fn clone(&self) -> Self {
        match self {
            Self::Unit { source_info } => Self::Unit {
                source_info: source_info.clone(),
            },
            Self::NonEmpty {
                elements,
                source_info,
                head,
            } => Self::NonEmpty {
                elements: elements.clone(),
                source_info: source_info.clone(),
                head: *head,
            },
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
        match self {
            Self::Unit { .. } => {
                // 空元组没有元素需要收集
            }
            Self::NonEmpty { elements, head, .. } => {
                for element in elements.as_ref().iter().skip(*head) {
                    // 我们不关心 head 之前的元素，他们对于本类型是不可达的
                    element.collect(queue);
                }
            }
        }
    }
}

impl<T: GcAllocObject<T, Inner = Type<T>>> Rootable<T> for Tuple<T> {
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {
        match self {
            Self::Unit { .. } => {
                // 空元组没有元素需要升级
            }
            Self::NonEmpty { elements, head, .. } => {
                for element in elements.as_ref().iter().skip(*head) {
                    // 我们不关心 head 之前的元素，他们对于本类型是不可达的
                    element.upgrade(collected);
                }
            }
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

                TypeRef::Sequence(v) => {
                    let mut all = ThreeValuedLogic::True;
                    let mut cursor = 0;
                    for (prefix, repeat) in v.prefix() {
                        for _ in 0..repeat.get() {
                            if cursor >= self.len() {
                                return Ok(ThreeValuedLogic::False);
                            }
                            let elem = &self.iter().nth(cursor).unwrap();
                            all &=
                                test_true!(elem.check(prefix.as_ref_dispatcher(), &mut inner_ctx)?);
                            cursor += 1;
                        }
                    }
                    let suffix_len = self.len().saturating_sub(cursor);
                    match v.tail() {
                        SequenceTail::Nothing => Ok(if suffix_len == 0 {
                            all
                        } else {
                            ThreeValuedLogic::False
                        }),
                        SequenceTail::Repeat(ty) => {
                            for _ in 0..suffix_len {
                                let elem = &self.iter().nth(cursor).unwrap();
                                all &=
                                    test_true!(elem.check(ty.0.as_ref_dispatcher(), &mut inner_ctx)?);
                                cursor += 1;
                            }
                            Ok(all)
                        }
                        SequenceTail::Cons(ty) => {
                            let viewed = self.view(cursor).ok_or(TypeError::UnresolvableType(
                                "Could not view tuple suffix".into(),
                            ))?;
                            all &=
                                test_true!(viewed.check(ty.as_ref_dispatcher(), &mut inner_ctx)?);
                            Ok(all)
                        }
                    }
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

                TypeRef::Sequence(v) => {
                    let mut all = ThreeValuedLogic::True;
                    let mut cursor = 0;
                    for (prefix, repeat) in v.prefix() {
                        for _ in 0..repeat.get() {
                            if cursor >= self.len() {
                                return Ok(ThreeValuedLogic::False);
                            }
                            let elem = &self.iter().nth(cursor).unwrap();
                            all &=
                                test_true!(elem.subof(prefix.as_ref_dispatcher(), &mut inner_ctx)?);
                            cursor += 1;
                        }
                    }
                    let suffix_len = self.len().saturating_sub(cursor);
                    match v.tail() {
                        SequenceTail::Nothing => Ok(if suffix_len == 0 {
                            all
                        } else {
                            ThreeValuedLogic::False
                        }),
                        SequenceTail::Repeat(ty) => {
                            for _ in 0..suffix_len {
                                let elem = &self.iter().nth(cursor).unwrap();
                                all &=
                                    test_true!(elem.subof(ty.0.as_ref_dispatcher(), &mut inner_ctx)?);
                                cursor += 1;
                            }
                            Ok(all)
                        }
                        SequenceTail::Cons(ty) => {
                            let viewed = self.view(cursor).ok_or(TypeError::UnresolvableType(
                                "Could not view tuple suffix".into(),
                            ))?;
                            all &=
                                test_true!(viewed.subof(ty.as_ref_dispatcher(), &mut inner_ctx)?);
                            Ok(all)
                        }
                    }
                }
                _ => Ok(ThreeValuedLogic::False),
            }
        })
    }

    fn reduce(
        mut self,
        ctx: &mut ReductionContext<Type<T>, T>,
    ) -> Result<Type<T>, super::TypeError<Type<T>, T>> {
        match &mut self {
            Self::Unit { .. } => Ok(self.dispatch()),
            Self::NonEmpty {
                elements,
                head,
                source_info,
            } => {
                let head_val = *head;
                let source_info_clone = source_info.clone();
                match elements.modify(|mut elems| {
                    for element in elems.iter_mut().skip(head_val) {
                        // 手动提供一个占位符值，然后换出旧值
                        let owned_element = std::mem::replace(element, TypeBound::bottom(None));
                        let reduced = owned_element.reduce(ctx)?;
                        // 将计算结果写回
                        *element = reduced;
                    }
                    Ok(elems)
                })? {
                    Some(()) => Ok(self.dispatch()),
                    None => {
                        let new_elements = self
                            .types()
                            .iter()
                            .map(|t| t.clone().reduce(ctx))
                            .collect::<Result<Vec<_>, _>>()?;
                        Ok(Self::new(new_elements, source_info_clone))
                    }
                }
            }
        }
    }

    fn invoke(
        self,
        _ctx: InvokeContext<Type<T>, T>,
    ) -> Result<Type<T>, super::TypeError<Type<T>, T>> {
        Err(TypeError::NonApplicableType(self.into_dispatcher().into()))
    }

    fn tagged_ptr(&self) -> TaggedPtr<()> {
        match self {
            Self::Unit { .. } => TaggedPtr::new(std::ptr::null(), 0),
            Self::NonEmpty { elements, head, .. } => {
                TaggedPtr::new(elements.as_ref() as *const _ as *const (), *head)
            }
        }
    }

    fn source_info(&self) -> Option<&Arc<SourceLocation>> {
        match self {
            Self::Unit { source_info } => source_info.as_ref(),
            Self::NonEmpty { source_info, .. } => source_info.as_ref(),
        }
    }

    fn report_source_info(&self) -> crate::types::TypeReport {
        if let Some(loc) = self.source_info() {
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
        match self {
            Self::Unit { .. } => 0,
            Self::NonEmpty { elements, head, .. } => elements.as_ref().len() - head,
        }
    }

    pub fn is_empty(&self) -> bool {
        matches!(self, Self::Unit { .. })
    }

    pub fn iter(&self) -> impl Iterator<Item = &Type<T>> {
        match self {
            Self::Unit { .. } => [].iter(),
            Self::NonEmpty { elements, head, .. } => {
                // 使用切片的 iter 来避免生命周期问题
                let slice = &elements.as_ref()[*head..];
                slice.iter()
            }
        }
    }

    pub fn types(&self) -> &[Type<T>] {
        match self {
            Self::Unit { .. } => &[],
            Self::NonEmpty { elements, head, .. } => &elements.as_ref()[*head..],
        }
    }

    pub fn get(&self, index: usize) -> Option<&Type<T>> {
        match self {
            Self::Unit { .. } => None,
            Self::NonEmpty { elements, head, .. } => {
                if index >= self.len() {
                    return None;
                }
                elements.as_ref().get(*head + index)
            }
        }
    }

    #[allow(clippy::new_ret_no_self)]
    pub fn new<I, X>(types: I, source_info: Option<Arc<SourceLocation>>) -> Type<T>
    where
        I: IntoIterator<Item = X>,
        X: AsDispatcher<Type<T>, T>,
    {
        let elements: Vec<Type<T>> = types.into_iter().map(|t| t.into_dispatcher()).collect();
        if elements.is_empty() {
            Self::Unit { source_info }.dispatch()
        } else {
            Self::NonEmpty {
                elements: ArcOpt::new(elements),
                head: 0,
                source_info,
            }
            .dispatch()
        }
    }

    pub fn view(&self, start: usize) -> Option<Type<T>> {
        if start > self.len() {
            return None;
        }
        Some(match self {
            Self::Unit { source_info } => Self::Unit {
                source_info: source_info.clone(),
            }
            .dispatch(),
            Self::NonEmpty {
                elements,
                head,
                source_info,
            } => {
                let new_head = head + start;
                if new_head >= elements.as_ref().len() {
                    Self::Unit {
                        source_info: source_info.clone(),
                    }
                    .dispatch()
                } else {
                    Self::NonEmpty {
                        elements: elements.clone(),
                        head: new_head,
                        source_info: source_info.clone(),
                    }
                    .dispatch()
                }
            }
        })
    }

    pub fn head(&self) -> Option<&Type<T>> {
        match self {
            Self::Unit { .. } => None,
            Self::NonEmpty { elements, head, .. } => elements.as_ref().get(*head),
        }
    }

    pub fn tail(&self) -> Option<Type<T>> {
        self.view(1)
    }

    pub fn concat(self, other: Tuple<T>, source_info: Option<Arc<SourceLocation>>) -> Type<T> {
        let mut new_elements = self.take();
        new_elements.extend(other.take());
        if new_elements.is_empty() {
            Self::Unit { source_info }.dispatch()
        } else {
            Self::NonEmpty {
                elements: ArcOpt::new(new_elements),
                head: 0,
                source_info,
            }
            .dispatch()
        }
    }

    pub fn take(self) -> Vec<Type<T>> {
        match self {
            Self::Unit { .. } => Vec::new(),
            Self::NonEmpty { elements, head, .. } => match elements.take() {
                Ok(mut vec) => {
                    // 拥有唯一所有权,直接 drain 前面的元素
                    vec.drain(..head);
                    vec
                }
                Err(arc) => {
                    // 共享引用,只克隆需要的部分
                    arc.as_ref()[head..].to_vec()
                }
            },
        }
    }

    pub fn unit() -> Type<T> {
        Self::Unit { source_info: None }.dispatch()
    }
}
