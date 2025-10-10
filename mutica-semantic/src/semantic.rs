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
        // 按照字节偏移标记对应的 AST 节点
        if let Some(loc) = node.location() {
            if loc.source() == source_file {
                let start = loc.span().start;
                let end = loc.span().end;
                if mapping.len() < end {
                    mapping.resize(end, None);
                }
                for i in start..end {
                    //if mapping[i].is_none() {
                    mapping[i] = Some(node);
                    //}
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
                Self::build_mapping(continuation, mapping, source_file);
                Self::build_mapping(func, mapping, source_file);
                Self::build_mapping(arg, mapping, source_file);
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
        parser::{
            ParseContext, SourceFile,
            ast::{LinearTypeAst, LinearizeContext},
            lexer::LexerToken,
        },
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

    /// 根据 AST 节点类型获取颜色名称和样式
    fn get_color_for_ast(ast: &LinearTypeAst) -> &'static str {
        match ast {
            LinearTypeAst::Variable(_) => "green",
            LinearTypeAst::IntLiteral(_) => "yellow",
            LinearTypeAst::CharLiteral(_) => "yellow",
            LinearTypeAst::Int => "blue",
            LinearTypeAst::Char => "blue",
            LinearTypeAst::Top => "bright blue",
            LinearTypeAst::Bottom => "bright blue",
            LinearTypeAst::Closure { .. } => "red",
            LinearTypeAst::Invoke { .. } => "magenta",
            LinearTypeAst::Tuple(_) => "cyan",
            LinearTypeAst::List(_) => "bright cyan",
            LinearTypeAst::Generalize(_) => "bright green",
            LinearTypeAst::Specialize(_) => "bright yellow",
            LinearTypeAst::AtomicOpcode(_) => "bright magenta",
            LinearTypeAst::FixPoint { .. } => "bright red",
            LinearTypeAst::Namespace { .. } => "white",
            LinearTypeAst::Pattern { .. } => "bright white",
            LinearTypeAst::Literal(_) => "yellow",
        }
    }

    /// 打印带颜色的源代码映射
    pub fn print_colored_mapping(expr: &str) {
        use colored::Colorize;

        let source_file = Arc::new(SourceFile::new(None, expr.into()));

        println!(
            "\n{}\n",
            "=== Parsing and building mapping ===".bright_white().bold()
        );
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
                let linearized = basic
                    .linearize(&mut LinearizeContext::new(), basic.location())
                    .finalize();
                let flow_result =
                    linearized.flow(&mut ParseContext::new(), false, linearized.location());
                let flowed = match &flow_result {
                    Ok(result) => result.ty().clone(),
                    Err(e) => {
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

                println!("{}\n", "=== Colored Source Code ===".bright_white().bold());

                // 遍历源代码的每个字符
                let mut current_color = None;
                let mut buffer = String::new();

                for (byte_offset, ch) in expr.char_indices() {
                    let color = if let Some(node) = mapping.at(byte_offset) {
                        Some(get_color_for_ast(node.value()))
                    } else {
                        None
                    };

                    // 如果颜色改变，先输出缓冲区的内容
                    if color != current_color {
                        if !buffer.is_empty() {
                            match current_color {
                                Some("green") => print!("{}", buffer.green()),
                                Some("yellow") => print!("{}", buffer.yellow()),
                                Some("blue") => print!("{}", buffer.blue()),
                                Some("bright blue") => print!("{}", buffer.bright_blue()),
                                Some("red") => print!("{}", buffer.red()),
                                Some("magenta") => print!("{}", buffer.magenta()),
                                Some("cyan") => print!("{}", buffer.cyan()),
                                Some("bright cyan") => print!("{}", buffer.bright_cyan()),
                                Some("bright green") => print!("{}", buffer.bright_green()),
                                Some("bright yellow") => print!("{}", buffer.bright_yellow()),
                                Some("bright magenta") => print!("{}", buffer.bright_magenta()),
                                Some("bright red") => print!("{}", buffer.bright_red()),
                                Some("white") => print!("{}", buffer.white()),
                                Some("bright white") => print!("{}", buffer.bright_white()),
                                None => print!("{}", buffer.dimmed()),
                                _ => print!("{}", buffer),
                            }
                            buffer.clear();
                        }
                        current_color = color;
                    }

                    buffer.push(ch);
                }

                // 输出最后的缓冲区内容
                if !buffer.is_empty() {
                    match current_color {
                        Some("green") => print!("{}", buffer.green()),
                        Some("yellow") => print!("{}", buffer.yellow()),
                        Some("blue") => print!("{}", buffer.blue()),
                        Some("bright blue") => print!("{}", buffer.bright_blue()),
                        Some("red") => print!("{}", buffer.red()),
                        Some("magenta") => print!("{}", buffer.magenta()),
                        Some("cyan") => print!("{}", buffer.cyan()),
                        Some("bright cyan") => print!("{}", buffer.bright_cyan()),
                        Some("bright green") => print!("{}", buffer.bright_green()),
                        Some("bright yellow") => print!("{}", buffer.bright_yellow()),
                        Some("bright magenta") => print!("{}", buffer.bright_magenta()),
                        Some("bright red") => print!("{}", buffer.bright_red()),
                        Some("white") => print!("{}", buffer.white()),
                        Some("bright white") => print!("{}", buffer.bright_white()),
                        None => print!("{}", buffer.dimmed()),
                        _ => print!("{}", buffer),
                    }
                }

                println!("\n\n{}", "=== Color Legend ===".bright_white().bold());
                println!("{}: Variable", "green".green());
                println!("{}: Literal (Int/Char)", "yellow".yellow());
                println!("{}: Type (Int/Char)", "blue".blue());
                println!("{}: Top/Bottom", "bright blue".bright_blue());
                println!("{}: Closure", "red".red());
                println!("{}: Invoke", "magenta".magenta());
                println!("{}: Tuple", "cyan".cyan());
                println!("{}: List", "bright cyan".bright_cyan());
                println!("{}: Generalize", "bright green".bright_green());
                println!("{}: Specialize", "bright yellow".bright_yellow());
                println!("{}: AtomicOpcode", "bright magenta".bright_magenta());
                println!("{}: FixPoint", "bright red".bright_red());
                println!("{}: Namespace", "white".white());
                println!("{}: Pattern", "bright white".bright_white());
                println!("{}: No mapping", "dimmed".dimmed());
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
    fn test_colored_source_mapping() {
        let expr = r#"
let Just: any = T: any |-> Just::T;
let Nothing: any = Nothing::();
// let Maybe: any = T: any |-> (Just T | Nothing);
let map: any = v: (Nothing::() | Just::any) |-> f: any |-> 
    match v
        | Just::(x: any) => Just(f(x))
        | Nothing::() => Nothing
        | panic;
let v1: any = Just(41);
let v2: any = Nothing;
map(v1)(x: int |-> x + 1), map(v2)(x: int |-> x + 1)
        "#;

        print_colored_mapping(expr);

        let expr = r#"
let int_list: any = rec list: (() | (int, list));
let append: any = rec append: (list1: any, list2: any) |->
    match list1 // this is a comment
        | () => list2
        | (head: int, tail: any) => (head, append(tail, list2))
        | panic;
let lst1: any = @(1, 2, 3);
let lst2: any = @(4, 5, 6);
let lst3: any = append(lst1, lst2);
lst3, lst3 <: int_list
        "#;

        print_colored_mapping(expr);
    }
}
