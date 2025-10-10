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

        // 按照字节偏移标记对应的 AST 节点
        if let Some(loc) = node.location() {
            if loc.source() == source_file {
                let start = loc.span().start;
                let end = loc.span().end;
                if mapping.len() < end {
                    mapping.resize(end, None);
                }
                for i in start..end {
                    if mapping[i].is_none() {
                        mapping[i] = Some(node);
                    }
                }
            }
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

    pub fn get_reference(
        &self,
        byte_offset: usize,
    ) -> Option<WithLocation<Option<&LinearTypeAst<'ast>>>> {
        self.at(byte_offset)
            .map(|node| node.payload().reference())
            .and_then(|r| r.cloned())
    }

    pub fn get_variable_context(&self, byte_offset: usize) -> Option<&[WithLocation<String>]> {
        // 获取变量上下文
        // 由于存在大量的空白区域，直接通过字节偏移获取可能会失败
        // 我们使用二分查找向前查找最后一个有效的映射

        if self.mapping.is_empty() {
            return None;
        }

        // 确定搜索的结束位置
        let end_pos = byte_offset.min(self.mapping.len() - 1);

        // 先尝试直接获取
        if let Some(node) = self.mapping[end_pos] {
            return Some(node.payload().variable_context().as_slice());
        }

        // 使用二分查找找到最近的有效节点
        // 我们要找的是 <= end_pos 的最大索引，且该位置有有效节点
        let mut left = 0;
        let mut right = end_pos;
        let mut result = None;

        while left <= right {
            let mid = left + (right - left) / 2;

            if let Some(node) = self.mapping[mid] {
                // 找到一个有效节点，记录它并尝试找更靠右的
                result = Some(node.payload().variable_context().as_slice());
                if mid == end_pos {
                    break;
                }
                left = mid + 1;
            } else {
                // 当前位置无效，向左搜索
                if mid == 0 {
                    break;
                }
                right = mid - 1;
            }
        }

        result
    }

    pub fn mapping(
        &self,
    ) -> &Vec<Option<&'ast WithLocation<LinearTypeAst<'ast>, FlowedMetaData<'ast>>>> {
        &self.mapping
    }
}

#[cfg(test)]
mod test {
    use std::sync::Arc;

    use mutica_compiler::{
        SyntaxError, ariadne,
        grammar::TypeParser,
        logos::{Logos, Source},
        parser::{ParseContext, SourceFile, ast::LinearizeContext, lexer::LexerToken},
    };

    use crate::semantic::SourceMapping;

    pub fn parse_and_test_mapping(expr: &str, byte_offsets: Vec<usize>) {
        let source_file = Arc::new(SourceFile::new(None, expr.into()));

        #[cfg(debug_assertions)]
        println!("Parsing expression:\n{}\n", expr);
        let lexer = LexerToken::lexer(expr);
        let spanned_lexer = lexer.spanned().map(|(token_result, span)| {
            let token = token_result?;
            Ok((span.start, token, span.end))
        });

        let parser = TypeParser::new();
        let parsed = parser.parse(&source_file, spanned_lexer);
        match parsed {
            Ok(ast) => {
                let mut errors = Vec::new();
                ast.collect_errors(&mut errors);
                if !errors.is_empty() {
                    for e in errors {
                        mutica_compiler::parser::report_error_recovery(&e, "<input>", expr);
                    }
                    return;
                }
                let basic = ast.into_basic(ast.location());
                // println!("Basic AST: {:?}", basic);
                let linearized = basic
                    .linearize(&mut LinearizeContext::new(), basic.location())
                    .finalize();
                // println!("Linearized AST: {:?}", linearized);
                let flow_result =
                    linearized.flow(&mut ParseContext::new(), false, linearized.location());
                let flowed = match &flow_result {
                    Ok(result) => result.ty().clone(),
                    Err(e) => {
                        // 获取源文件信息用于错误报告
                        let (filename, source_content) =
                            if let Some(location) = linearized.location() {
                                let source = location.source();
                                (source.filename(), source.content().to_string())
                            } else {
                                ("<input>".to_string(), expr.to_string())
                            };
                        e.report()
                            .eprint((filename, ariadne::Source::from(source_content)))
                            .ok();
                        return;
                    }
                };

                let mapping = SourceMapping::from_ast(&flowed, &source_file);

                for byte_offset in byte_offsets {
                    //
                    // 测试字节偏移到 AST 节点的映射
                    if let Some(node) = mapping.at(byte_offset) {
                        // println!(
                        //     "Byte offset {} maps to location {:?}\nSource: {:?}\n",
                        //     byte_offset,
                        //     node.location(),
                        //     node.location()
                        //         .map(|loc| loc.source().content().slice(loc.span().clone()))
                        // );
                        if let Some(reference) = node.payload().reference() {
                            println!(
                                "Byte offset {} maps at location {:?} reference to {:?}, context: {:?}",
                                byte_offset,
                                node.location().map(|loc| loc.span().clone()),
                                reference
                                    .location()
                                    .map(|loc| loc.source().content().slice(loc.span().clone())),
                                node.payload().variable_context()
                            );
                        }
                    } else {
                        println!("Byte offset {} does not map to any AST node", byte_offset);
                    }
                }
            }
            Err(e) => {
                let syntax_error = SyntaxError::new(e);
                let filename = "<input>".to_string();
                let report = syntax_error.report(filename.clone(), expr);
                report.eprint((filename, ariadne::Source::from(expr))).ok();
            }
        }
    }

    #[test]
    fn test_source_mapping() {
        let expr = r#"
let Option: any = T: any |-> (Some::T | None::());
let println: any = x: any |-> {
    discard print x;
    discard print '\n';
};
discard println[Option(1)];
discard println[Option(2)];
discard println[Option(int)];
Option(1), Option(2), Option(int), Option(1) <: Option(int), Option(2) <: Option(int), Option(1) <: Option(2)
        "#;
        // 测试不同的字节偏移
        let mut byte_offsets = vec![];
        for i in 0..expr.len() {
            byte_offsets.push(i);
        }
        parse_and_test_mapping(expr, byte_offsets);
    }
}
