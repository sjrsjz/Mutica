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
            LinearTypeAst::Float => (),
            LinearTypeAst::Char => (),
            LinearTypeAst::Top => (),
            LinearTypeAst::Bottom => (),
            LinearTypeAst::IntLiteral(_) => (),
            LinearTypeAst::FloatLiteral(_) => (),
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
            LinearTypeAst::Match { branches, .. } => {
                for (pattern, expr) in branches {
                    Self::build_mapping(pattern, mapping, source_file);
                    Self::build_mapping(expr, mapping, source_file);
                }
            }
            LinearTypeAst::Invoke {
                func,
                arg,
                continuation,
                perform_handler,
            } => {
                if let Some(perform_handler) = perform_handler {
                    Self::build_mapping(perform_handler, mapping, source_file);
                }
                if let Some(continuation) = continuation {
                    Self::build_mapping(continuation, mapping, source_file);
                }
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
            LinearTypeAst::Literal(expr) => {
                Self::build_mapping(expr, mapping, source_file);
            }
            LinearTypeAst::Rot { value } => {
                Self::build_mapping(value, mapping, source_file);
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
    use mutica_compiler::{
        ariadne,
        logos::Source,
        parser::{ParseContext, ast::LinearizeContext, colorize::TokenColor},
    };

    use crate::semantic::SourceMapping;

    pub fn parse_and_test_mapping(expr: &str, byte_offsets: Vec<usize>) {
        use mutica_compiler::parser::{MultiFileBuilder, MultiFileBuilderError, SyntaxError};
        use std::collections::HashMap;
        use std::path::PathBuf;

        #[cfg(debug_assertions)]
        println!("Parsing expression:\n{}\n", expr);

        let path = PathBuf::from("<test>");

        // 使用 MultiFileBuilder 来构建整个项目
        let mut imported_ast = HashMap::new();
        let mut cycle_detector = mutica_core::util::cycle_detector::FastCycleDetector::new();
        let mut builder_errors = Vec::new();
        let mut multifile_builder =
            MultiFileBuilder::new(&mut imported_ast, &mut cycle_detector, &mut builder_errors);
        let (ast, source) = multifile_builder.build(path.clone(), expr.to_string());
        // 直接使用 MultiFileBuilder 构建
        let basic = match ast {
            Some(ast) => ast,
            None => {
                // 报告构建错误
                for error_with_loc in &builder_errors {
                    let (filepath, source_content) =
                        if let Some(location) = error_with_loc.location() {
                            let source = location.source();
                            (source.filepath(), source.content().to_string())
                        } else {
                            (path.to_string_lossy().to_string(), expr.to_string())
                        };

                    match error_with_loc.value() {
                        MultiFileBuilderError::SyntaxError(e) => {
                            let syntax_error = SyntaxError::new(e.clone());
                            let report = syntax_error.report(filepath.clone(), &source_content);
                            report
                                .eprint((filepath, ariadne::Source::from(source_content)))
                                .ok();
                        }
                        MultiFileBuilderError::RecoveryError(e) => {
                            let report = mutica_compiler::parser::report_error_recovery(
                                e,
                                &filepath,
                                &source_content,
                            );
                            report
                                .eprint((filepath.as_str(), ariadne::Source::from(source_content)))
                                .ok();
                        }
                        MultiFileBuilderError::IOError(e) => {
                            eprintln!("IO Error: {}", e);
                        }
                    }
                }
                return;
            }
        };

        let linearized = basic
            .0
            .linearize(&mut LinearizeContext::new(), basic.0.location())
            .finalize();

        let mut flow_errors = Vec::new();
        let flowed = linearized.flow(
            &mut ParseContext::new(),
            false,
            linearized.location(),
            &mut flow_errors,
        );

        if !flow_errors.is_empty() {
            // 获取源文件信息用于错误报告
            let filepath = source.filepath();
            let source_content = source.content().to_string();
            // 报告所有错误
            let mut has_error = false;
            for e in &flow_errors {
                e.report()
                    .eprint((
                        filepath.clone(),
                        ariadne::Source::from(source_content.clone()),
                    ))
                    .ok();
                if !e.is_warning() {
                    has_error = true;
                }
            }
            if has_error {
                return;
            }
        }

        let flowed = flowed.ty().clone();

        // 获取 source_file 用于构建映射
        let source_file = source;

        let mapping = SourceMapping::from_ast(&flowed, &source_file);

        for byte_offset in byte_offsets {
            // 测试字节偏移到 AST 节点的映射
            if let Some(node) = mapping.at(byte_offset) {
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
    /// 打印带颜色的源代码映射
    pub fn print_colored_mapping(expr: &str) {
        use colored::Colorize;
        use mutica_compiler::parser::{MultiFileBuilder, MultiFileBuilderError, SyntaxError};
        use std::collections::HashMap;
        use std::path::PathBuf;

        let path = PathBuf::from("<test>");

        println!(
            "\n{}\n",
            "=== Parsing and building mapping ===".bright_white().bold()
        );

        // 使用 MultiFileBuilder 来构建整个项目
        let mut imported_ast = HashMap::new();
        let mut cycle_detector = mutica_core::util::cycle_detector::FastCycleDetector::new();
        let mut builder_errors = Vec::new();
        let mut multifile_builder =
            MultiFileBuilder::new(&mut imported_ast, &mut cycle_detector, &mut builder_errors);
        let (ast, source) = multifile_builder.build(path.clone(), expr.to_string());
        // 直接使用 MultiFileBuilder 构建
        let basic = match ast {
            Some(ast) => ast,
            None => {
                // 报告构建错误
                for error_with_loc in &builder_errors {
                    let filepath = source.filepath();
                    let source_content = source.content().to_string();
                    match error_with_loc.value() {
                        MultiFileBuilderError::SyntaxError(e) => {
                            let syntax_error = SyntaxError::new(e.clone());
                            let report = syntax_error.report(filepath.clone(), &source_content);
                            report
                                .eprint((filepath, ariadne::Source::from(source_content)))
                                .ok();
                        }
                        MultiFileBuilderError::RecoveryError(e) => {
                            let report = mutica_compiler::parser::report_error_recovery(
                                e,
                                &filepath,
                                &source_content,
                            );
                            report
                                .eprint((filepath.as_str(), ariadne::Source::from(source_content)))
                                .ok();
                        }
                        MultiFileBuilderError::IOError(e) => {
                            eprintln!("IO Error: {}", e);
                        }
                    }
                }
                return;
            }
        };

        let color_buffer = basic.1.color_mapping();
        // 按照字节偏移打印带颜色的源代码
        for (i, ch) in expr.char_indices() {
            if i < color_buffer.len() {
                let color = color_buffer[i];
                let color_name = match color {
                    TokenColor::UnSpecified => "dimmed",
                    TokenColor::Keyword => "bright blue",
                    TokenColor::Declaration => "underlined bright blue",
                    TokenColor::Namespace => "underlined bright yellow",
                    TokenColor::Identifier => "green",
                    TokenColor::Literal => "yellow",
                    TokenColor::Operator => "magenta",
                    TokenColor::Comment => "bright black",
                    TokenColor::Whitespace => "dimmed",
                    TokenColor::Punctuation => "cyan",
                    TokenColor::Function => "bright green",
                    TokenColor::Type => "blue",
                    TokenColor::Attribute => "bright yellow",
                    TokenColor::Macro => "bright magenta",
                    TokenColor::Number => "yellow",
                    TokenColor::String => "yellow",
                    TokenColor::Boolean => "bright blue",
                    TokenColor::Error => "red",
                };
                let colored_char = match color_name {
                    "dimmed" => ch.to_string().dimmed(),
                    "bright blue" => ch.to_string().bright_blue().bold(),
                    "green" => ch.to_string().green(),
                    "yellow" => ch.to_string().yellow(),
                    "magenta" => ch.to_string().magenta(),
                    "bright black" => ch.to_string().bright_black().italic(),
                    "cyan" => ch.to_string().cyan(),
                    "bright green" => ch.to_string().bright_green().bold(),
                    "blue" => ch.to_string().blue(),
                    "bright yellow" => ch.to_string().bright_yellow().bold(),
                    "bright magenta" => ch.to_string().bright_magenta().bold(),
                    "red" => ch.to_string().red().bold(),
                    "underlined bright blue" => ch.to_string().bright_blue().underline().bold(),
                    "underlined bright yellow" => ch.to_string().bright_yellow().underline().bold(),
                    _ => ch.to_string().normal(),
                };
                print!("{}", colored_char);
            } else {
                print!("{}", ch);
            }
        }
        println!();

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

    #[test]
    fn test_colored_source_mapping() {
        let expr = r#"
let maybe_pkg: any = import "maybe.mu";
let List: any = T: any |-> rec list: (() | (T, list));
let Nil: any = ();
let cons: any = (head: any, tail: any) |-> (head, tail);
let head: any = match
    | (h: any, _) => h
    | panic;
let tail: any = match
    | (_, t: any) => t
    | panic;
let is_nil: any = match
    | () => true
    | _ => false
    | panic;
let iter: any = lst: List(any) |-> f: any |-> {
    let loop: any = rec go: match
        | () => ()
        | (h: any, t: any) => {
            discard f(h);
            go(t)
        }
        | panic;
    loop(lst)
};
let map: any = lst: List(any) |-> f: any |-> {
    let loop: any = rec go: match
        | () => ()
        | (h: any, t: any) => cons(f(h), go(t))
        | panic;
    loop(lst)
};
let len: any = lst: List(any) |-> {
    let loop: any = rec go: match
        | () => 0
        | (_, t: any) => 1 + go(t)
        | panic;
    loop(lst)
};
let filter: any = lst: List(any) |-> pred: any |-> {
    let loop: any = rec go: match
        | () => ()
        | (h: any, t: any) => match pred(h)
            | false => go(t)
            | true => cons(h, go(t))
            | panic
        | panic;
    loop(lst)
};
let fold: any = lst: List(any) |-> acc: any |-> f: any |-> {
    let loop: any = rec go: match
        | ((), a: any) => a
        | ((h: any, t: any), a: any) => go(t, f(a, h))
        | panic;
    loop(lst, acc)
};
let foldr: any = lst: List(any) |-> acc: any |-> f: any |-> {
    let loop: any = rec go: match
        | () => acc
        | (h: any, t: any) => f(h, go(t))
        | panic;
    loop(lst)
};
let append: any = lst1: List(any) |-> lst2: List(any) |-> {
    let loop: any = rec go: match
        | () => lst2
        | (h: any, t: any) => cons(h, go(t))
        | panic;
    loop(lst1)
};
let reverse: any = lst: List(any) |-> {
    let loop: any = rec go: match
        | ((), acc: any) => acc
        | ((h: any, t: any), acc: any) => go(t, cons(h, acc))
        | panic;
    loop(lst, ())
};
let nth: any = lst: List(any) |-> n: int |-> {
    let loop: any = rec go: match
        | ((h: any, _), 0) => h
        | ((_, t: any), i: any) => go(t, i - 1)
        | panic;
    loop(lst, n)
};
let take: any = lst: List(any) |-> n: int |-> {
    let loop: any = rec go: match
        | ((), _) => ()
        | (_, 0) => ()
        | ((h: any, t: any), i: any) => cons(h, go(t, i - 1))
        | panic;
    loop(lst, n)
};
let drop: any = lst: List(any) |-> n: int |-> {
    let loop: any = rec go: match
        | ((), _) => ()
        | (l: any, 0) => l
        | ((_, t: any), i: any) => go(t, i - 1)
        | panic;
    loop(lst, n)
};
let find: any = lst: List(any) |-> pred: any |-> {
    let loop: any = rec go: match
        | () => maybe_pkg.Nothing
        | (h: any, t: any) => match pred(h)
            | false => go(t)
            | true => maybe_pkg.Just(h)
            | panic
        | panic;
    loop(lst)
};
let list_all: any = lst: List(any) |-> pred: any |-> {
    let loop: any = rec go: match
        | () => true
        | (h: any, t: any) => match pred(h)
            | false => false
            | true => go(t)
            | panic
        | panic;
    loop(lst)
};
let list_any: any = lst: List(any) |-> pred: any |-> {
    let loop: any = rec go: match
        | () => false
        | (h: any, t: any) => match pred(h)
            | false => go(t)
            | true => true
            | panic
        | panic;
    loop(lst)
};

List::List &
Nil::Nil &
cons::cons &
head::head &
tail::tail &
is_nil::is_nil &
iter::iter &
map::map &
len::len &
filter::filter &
fold::fold &
foldr::foldr &
append::append &
reverse::reverse &
nth::nth &
take::take &
drop::drop &
find::find &
list_all::list_all &
list_any::list_any
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
