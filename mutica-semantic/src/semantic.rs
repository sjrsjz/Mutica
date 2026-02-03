use mutica_compiler::parser::{
    WithLocation,
    ast::{FlowedMetaData, LinearTypeAst},
};
use mutica_core::util::source_info::SourceFile;

pub struct SourceMapping<'ast> {
    mapping: Vec<Option<&'ast WithLocation<LinearTypeAst, FlowedMetaData>>>, // 按字节偏移存储对应的 AST 节点
}

impl<'ast> SourceMapping<'ast> {
    /// 从 AST 构建字节偏移到 AST 节点的映射
    pub fn from_ast(
        ast: &'ast WithLocation<LinearTypeAst, FlowedMetaData>,
        source_file: &SourceFile,
    ) -> Self {
        let mut mapping = Vec::new();
        Self::build_mapping(ast, &mut mapping, source_file);
        SourceMapping { mapping }
    }

    fn build_mapping(
        node: &'ast WithLocation<LinearTypeAst, FlowedMetaData>,
        mapping: &mut Vec<Option<&'ast WithLocation<LinearTypeAst, FlowedMetaData>>>,
        source_file: &SourceFile,
    ) {
        // 按照字节偏移标记对应的 AST 节点
        if let Some(loc) = node.location()
            && loc.source() == source_file
        {
            let start = loc.span().start;
            let end = loc.span().end;
            if mapping.len() < end {
                mapping.resize(end, None);
            }
            // Fill the slice with the node reference to avoid indexing warnings.
            for slot in &mut mapping[start..end] {
                *slot = Some(node);
            }
        }
        // 递归处理子节点
        match node.value() {
            LinearTypeAst::Range { ty, .. } => {
                Self::build_mapping(ty, mapping, source_file);
            }
            LinearTypeAst::NaturalNumberSet => (),
            LinearTypeAst::Float => (),
            LinearTypeAst::Char => (),
            LinearTypeAst::NaturalNumberLiteral(_) => (),
            LinearTypeAst::FloatLiteral(_) => (),
            LinearTypeAst::CharLiteral(_) => (),
            LinearTypeAst::Variable(_) => (),
            LinearTypeAst::Tuple(items) => {
                for (item, _count) in items {
                    Self::build_mapping(item, mapping, source_file);
                }
            }
            LinearTypeAst::List { head, tail } => {
                for (h, _count) in head {
                    Self::build_mapping(h, mapping, source_file);
                }
                Self::build_mapping(tail, mapping, source_file);
            }
            LinearTypeAst::Cons { head, tail } => {
                for (h, _count) in head {
                    Self::build_mapping(h, mapping, source_file);
                }
                Self::build_mapping(tail, mapping, source_file);
            }
            LinearTypeAst::AnyOf(items) => {
                for item in items {
                    Self::build_mapping(item, mapping, source_file);
                }
            }
            LinearTypeAst::AllOf(items) => {
                for item in items {
                    Self::build_mapping(item, mapping, source_file);
                }
            }
            LinearTypeAst::Match { branches, .. } => {
                for (pattern, constraint, expr) in branches {
                    Self::build_mapping(pattern, mapping, source_file);
                    for (_, expr) in constraint {
                        Self::build_mapping(expr, mapping, source_file);
                    }
                    Self::build_mapping(expr, mapping, source_file);
                }
            }
            LinearTypeAst::Lambda { patterns, .. } => {
                for (pattern, constraint) in patterns {
                    Self::build_mapping(pattern, mapping, source_file);
                    for (_, expr) in constraint {
                        Self::build_mapping(expr, mapping, source_file);
                    }
                }
            }
            LinearTypeAst::Invoke { func, arg, continuation, perform_handler } => {
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
            LinearTypeAst::Namespace { expr, .. } => {
                Self::build_mapping(expr, mapping, source_file);
            }
            LinearTypeAst::Generic { expr, constraint, .. } => {
                for (_, expr) in constraint {
                    Self::build_mapping(expr, mapping, source_file);
                }
                Self::build_mapping(expr, mapping, source_file);
            }
            LinearTypeAst::Lazy(expr) => {
                Self::build_mapping(expr, mapping, source_file);
            }
            LinearTypeAst::SubOf { value } => {
                Self::build_mapping(value, mapping, source_file);
            }
            LinearTypeAst::Mutable { value } => {
                Self::build_mapping(value, mapping, source_file);
            }
            LinearTypeAst::StaticFixPoint { expr, .. } => {
                Self::build_mapping(expr, mapping, source_file);
            }
        }
    }
}

impl<'ast> SourceMapping<'ast> {
    /// 根据字节偏移获取对应的 AST 节点
    pub fn at(
        &self,
        byte_offset: usize,
    ) -> Option<&'ast WithLocation<LinearTypeAst, FlowedMetaData>> {
        if byte_offset < self.mapping.len() { self.mapping[byte_offset] } else { None }
    }

    pub fn get_reference(&self, byte_offset: usize) -> Option<WithLocation<()>> {
        self.at(byte_offset).map(|node| node.payload().reference()).and_then(|r| r.cloned())
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

    pub fn mapping(&self) -> &Vec<Option<&'ast WithLocation<LinearTypeAst, FlowedMetaData>>> {
        &self.mapping
    }
}

#[cfg(test)]
mod test {
    use mutica_compiler::{
        ariadne,
        logos::Source,
        parser::{ParseContext, WithLocation, ast::LinearizeContext, inject_std_library},
    };
    use mutica_core::util::{colorize::TokenColor, source_info::SourceLocation};

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
        let (mut ast, source) = multifile_builder.build(path.clone(), expr.to_string());

        if let Some((ast, _)) = &mut ast {
            *ast = inject_std_library(ast.clone(), &mut builder_errors)
        }
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
                            let report =
                                syntax_error.report(filepath.clone(), &source_content, None);
                            report.eprint((filepath, ariadne::Source::from(source_content))).ok();
                        }
                        MultiFileBuilderError::RecoveryError(e) => {
                            let report = mutica_compiler::parser::report_error_recovery(
                                e,
                                filepath.clone(),
                                &source_content,
                            );
                            report
                                .eprint((filepath.clone(), ariadne::Source::from(source_content)))
                                .ok();
                        }
                        MultiFileBuilderError::IOError(e) => {
                            eprintln!("IO Error: {}", e);
                        }
                        MultiFileBuilderError::TopLevelBindError(v) => {
                            let range = v.location().map(|r| r.span().clone()).unwrap_or(0..0);
                            ariadne::Report::build(
                                ariadne::ReportKind::Error,
                                filepath.as_str(),
                                range.start,
                            )
                            .with_label(
                                ariadne::Label::new((filepath.as_str(), range)).with_message(
                                    format!(
                                        "Unexpected bind for variable '{}' at top level",
                                        v.value()
                                    ),
                                ),
                            )
                            .with_message("Invalid bind pattern")
                            .finish()
                            .eprint((filepath.as_str(), ariadne::Source::from(source_content)))
                            .ok();
                        }
                    }
                }
                return;
            }
        };

        // 执行 auto_bind 转换，将 AutoBind 模式转换为 Standard 模式
        let (desugared, leftover_binds) = basic.0.auto_bind();

        // 检查是否有未处理的绑定（这通常表示顶层有 Bind 节点，这是不应该出现的）
        if !leftover_binds.is_empty() {
            for (var, _) in leftover_binds {
                builder_errors.push(WithLocation::new(
                    MultiFileBuilderError::TopLevelBindError(var),
                    None::<&SourceLocation>,
                ));
            }
            // 报告错误
            for error_with_loc in &builder_errors {
                let (filepath, source_content) = if let Some(location) = error_with_loc.location() {
                    let source = location.source();
                    (source.filepath(), source.content().to_string())
                } else {
                    (path.to_string_lossy().to_string(), expr.to_string())
                };

                match error_with_loc.value() {
                    MultiFileBuilderError::SyntaxError(e) => {
                        let syntax_error = SyntaxError::new(e.clone());
                        let report = syntax_error.report(filepath.clone(), &source_content, None);
                        report.eprint((filepath, ariadne::Source::from(source_content))).ok();
                    }
                    MultiFileBuilderError::RecoveryError(e) => {
                        let report = mutica_compiler::parser::report_error_recovery(
                            e,
                            filepath.clone(),
                            &source_content,
                        );
                        report
                            .eprint((filepath.clone(), ariadne::Source::from(source_content)))
                            .ok();
                    }
                    MultiFileBuilderError::IOError(e) => {
                        eprintln!("IO Error: {}", e);
                    }
                    MultiFileBuilderError::TopLevelBindError(var) => {
                        let range = var.location().map(|r| r.span().clone()).unwrap_or(0..0);
                        ariadne::Report::build(
                            ariadne::ReportKind::Error,
                            filepath.as_str(),
                            range.start,
                        )
                        .with_label(ariadne::Label::new((filepath.as_str(), range)).with_message(
                            format!("Unexpected bind for variable '{}' at top level", var.value()),
                        ))
                        .with_message("Invalid bind pattern")
                        .finish()
                        .eprint((filepath.as_str(), ariadne::Source::from(source_content)))
                        .ok();
                    }
                }
            }
            return;
        }

        let mut flow_errors = Vec::new();

        let linearized = desugared
            .linearize(&mut LinearizeContext::new(), &mut flow_errors, desugared.location())
            .finalize();

        let flowed =
            linearized.flow(&mut ParseContext::new(), linearized.location(), &mut flow_errors);

        if !flow_errors.is_empty() {
            // 获取源文件信息用于错误报告
            let filepath = source.filepath();
            let source_content = source.content().to_string();
            // 报告所有错误
            let mut has_error = false;
            for e in &flow_errors {
                e.report()
                    .eprint((filepath.clone(), ariadne::Source::from(source_content.clone())))
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
// 阶乘和斐波那契数列示例

// 普通递归阶乘
let constraint factorial: any = 
    dyn_rec fact: match
        | assert 0 => 1
        | assert 1 => 1
        | constraint n: nat => n * fact(n - 1)
        | panic;

// 尾递归阶乘
let constraint factorial_tail: any = constraint n: nat => [
        let constraint helper: any = dyn_rec h: constraint acc: nat => match 
            | assert 0 => acc
            | assert 1 => acc
            | constraint n: nat => h(acc * n)(n - 1)
            | panic;
        helper(1)(n)
    ];

// 斐波那契数列
let constraint fibonacci: any = 
    dyn_rec fib: match 
        | assert 0 => 0
        | assert 1 => 1
        | constraint n: nat => fib(n - 1) + fib(n - 2)
        | panic;

// 尾递归斐波那契
let constraint fibonacci_tail: any = constraint n: nat => [
    let constraint helper: any = dyn_rec helper: constraint a: nat => constraint b: nat => match
            | assert 0 => a
            | constraint n: nat => helper(b)(a + b)(n - 1)
            | panic;
        helper(0)(1)(n)
    ];

// 测试
factorial(5), factorial_tail(5), fibonacci(7), fibonacci_tail(7)

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

        println!("\n{}\n", "=== Parsing and building mapping ===".bright_white().bold());

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
                            let report =
                                syntax_error.report(filepath.clone(), &source_content, None);
                            report.eprint((filepath, ariadne::Source::from(source_content))).ok();
                        }
                        MultiFileBuilderError::RecoveryError(e) => {
                            let report = mutica_compiler::parser::report_error_recovery(
                                e,
                                filepath.clone(),
                                &source_content,
                            );
                            report.eprint((filepath, ariadne::Source::from(source_content))).ok();
                        }
                        MultiFileBuilderError::IOError(e) => {
                            eprintln!("IO Error: {}", e);
                        }
                        MultiFileBuilderError::TopLevelBindError(v) => {
                            let range = v.location().map(|r| r.span().clone()).unwrap_or(0..0);
                            ariadne::Report::build(
                                ariadne::ReportKind::Error,
                                filepath.as_str(),
                                range.start,
                            )
                            .with_label(
                                ariadne::Label::new((filepath.as_str(), range)).with_message(
                                    format!(
                                        "Unexpected bind for variable '{}' at top level",
                                        v.value()
                                    ),
                                ),
                            )
                            .with_message("Invalid bind pattern")
                            .finish()
                            .eprint((filepath.as_str(), ariadne::Source::from(source_content)))
                            .ok();
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
        println!("{}: AnyOf", "bright green".bright_green());
        println!("{}: AllOf", "bright yellow".bright_yellow());
        println!("{}: AtomicOpcode", "bright magenta".bright_magenta());
        println!("{}: FixPoint", "bright red".bright_red());
        println!("{}: Namespace", "white".white());
        println!("{}: Pattern", "bright white".bright_white());
        println!("{}: No mapping", "dimmed".dimmed());
    }

    #[test]
    fn test_colored_source_mapping() {
        let expr = r#"
// 阶乘和斐波那契数列示例

// 普通递归阶乘
let constraint factorial: any = 
    dyn_rec fact: match
        | assert 0 => 1
        | assert 1 => 1
        | constraint n: nat => n * fact(n - 1)
        | panic;

// 尾递归阶乘
let constraint factorial_tail: any = constraint n: nat => [
        let constraint helper: any = dyn_rec h: constraint acc: nat => match 
            | assert 0 => acc
            | assert 1 => acc
            | constraint n: nat => h(acc * n)(n - 1)
            | panic;
        helper(1)(n)
    ];

// 斐波那契数列
let constraint fibonacci: any = 
    dyn_rec fib: match 
        | assert 0 => 0
        | assert 1 => 1
        | constraint n: nat => fib(n - 1) + fib(n - 2)
        | panic;

// 尾递归斐波那契
let constraint fibonacci_tail: any = constraint n: nat => [
    let constraint helper: any = dyn_rec helper: constraint a: nat => constraint b: nat => match
            | assert 0 => a
            | constraint n: nat => helper(b)(a + b)(n - 1)
            | panic;
        helper(0)(1)(n)
    ];

// 测试
factorial(5), factorial_tail(5), fibonacci(7), fibonacci_tail(7)

        "#;

        print_colored_mapping(expr);

        let expr = r#"
let constraint vec3: any = (float, float, float);

extend $"op#add": constraint (A: vec3, B: vec3) => {
    let constraint (x1: float, y1: float, z1: float) = A;
    let constraint (x2: float, y2: float, z2: float) = B;
    (x1 + x2, y1 + y2, z1 + z2)
};

extend $"op#sub": constraint (A: vec3, B: vec3) => {
    let constraint (x1: float, y1: float, z1: float) = A;
    let constraint (x2: float, y2: float, z2: float) = B;
    (x1 - x2, y1 - y2, z1 - z2)
};

extend $"op#mul": constraint (A: vec3, B: vec3) => {
    let constraint (x1: float, y1: float, z1: float) = A;
    let constraint (x2: float, y2: float, z2: float) = B;
    (x1 * x2, y1 * y2, z1 * z2)
};

extend $"op#mul": constraint (A: vec3, scale: float) => {
    let constraint (x1: float, y1: float, z1: float) = A;
    (x1 * scale, y1 * scale, z1 * scale)
};

extend $"op#div": constraint (A: vec3, scale: float) => {
    let constraint (x1: float, y1: float, z1: float) = A;
    (x1 / scale, y1 / scale, z1 / scale)
};

let constraint dot: any = constraint (A: vec3, B: vec3) => {
    let constraint (x1: float, y1: float, z1: float) = A;
    let constraint (x2: float, y2: float, z2: float) = B;
    x1 * x2 + y1 * y2 + z1 * z2
};

let constraint cross: any = constraint (A: vec3, B: vec3) => {
    let constraint (x1: float, y1: float, z1: float) = A;
    let constraint (x2: float, y2: float, z2: float) = B;
    (
        y1 * z2 - z1 * y2,
        z1 * x2 - x1 * z2,
        x1 * y2 - y1 * x2
    )
};

let constraint A: vec3 = (1.0, 2.0, 3.0);
let constraint B: vec3 = (4.0, 5.0, 6.0);
println!(A + B);
println!(A - B);
println!(A * B);
println!(A / 2.0);
println!(dot(A, B));
println!(cross(A, B));
        "#;

        print_colored_mapping(expr);
    }
}
