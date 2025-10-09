use mutica_compiler::parser::{
    SourceFile, WithLocation,
    ast::{FlowedMetaData, LinearTypeAst},
};

pub struct SourceMapping<'ast> {
    mapping: Vec<Option<&'ast WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>>, // 按字节偏移存储对应的 AST 节点
}

impl<'ast> SourceMapping<'ast> {
    /// 从 AST 构建字节偏移到 AST 节点的映射
    pub fn from_ast(
        ast: &'ast WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>,
        source_file: &SourceFile,
    ) -> Self {
        let mut mapping = Vec::new();
        Self::build_mapping(ast, &mut mapping, source_file);
        SourceMapping { mapping }
    }

    fn build_mapping(
        node: &'ast WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>,
        mapping: &mut Vec<Option<&'ast WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>>,
        source_file: &SourceFile,
    ) {
        // 我们自顶而下的按照字节偏移标记对应的 AST 节点
        if let Some(loc) = node.location() {
            if loc.source() == source_file {
                let start = loc.span().start;
                let end = loc.span().end;
                if mapping.len() < end {
                    mapping.resize(end, None);
                }
                for i in start..end {
                    mapping[i] = Some(node);
                }
            }
        }

        // 递归处理子节点
        match node.value() {
            LinearTypeAst::Int => (),
            LinearTypeAst::Char => (),
            LinearTypeAst::Top => (),
            LinearTypeAst::Bottom => (),
            LinearTypeAst::IntLiteral(_) => (),
            LinearTypeAst::CharLiteral(_) => (),
            LinearTypeAst::Variable(_) => (),
            LinearTypeAst::Tuple(items) => {
                for item in items {
                    Self::build_mapping(item, mapping, source_file);
                }
            }
            LinearTypeAst::List(items) => {
                for item in items {
                    Self::build_mapping(item, mapping, source_file);
                }
            }
            LinearTypeAst::Generalize(items) => {
                for item in items {
                    Self::build_mapping(item, mapping, source_file);
                }
            }
            LinearTypeAst::Specialize(items) => {
                for item in items {
                    Self::build_mapping(item, mapping, source_file);
                }
            }
            LinearTypeAst::Closure {
                pattern,
                body,
                fail_branch,
                ..
            } => {
                Self::build_mapping(pattern, mapping, source_file);
                Self::build_mapping(body, mapping, source_file);
                if let Some(fail_branch) = fail_branch {
                    Self::build_mapping(fail_branch, mapping, source_file);
                }
            }
            LinearTypeAst::Invoke {
                func,
                arg,
                continuation,
            } => {
                Self::build_mapping(func, mapping, source_file);
                Self::build_mapping(arg, mapping, source_file);
                Self::build_mapping(continuation, mapping, source_file);
            }
            LinearTypeAst::AtomicOpcode(_) => (),
            LinearTypeAst::FixPoint { expr, .. } => {
                Self::build_mapping(expr, mapping, source_file);
            }
            LinearTypeAst::Namespace { expr, .. } => {
                Self::build_mapping(expr, mapping, source_file);
            }
            LinearTypeAst::Pattern { expr, .. } => {
                Self::build_mapping(expr, mapping, source_file);
            }
            LinearTypeAst::Literal(_) => todo!(),
        }
    }
}

impl<'ast> SourceMapping<'ast> {
    /// 根据字节偏移获取对应的 AST 节点
    pub fn at(
        &self,
        byte_offset: usize,
    ) -> Option<&'ast WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>> {
        if byte_offset < self.mapping.len() {
            self.mapping[byte_offset]
        } else {
            None
        }
    }

    pub fn get_reference(&self, byte_offset: usize) -> Option<WithLocation<&LinearTypeAst<'ast>>> {
        self.at(byte_offset)
            .map(|node| node.payload().reference())
            .and_then(|r| r.cloned())
    }
}
