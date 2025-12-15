use clap::{Parser, Subcommand};
use std::{collections::HashMap, fs, path::PathBuf, process, sync::OnceLock};

use mutica_compiler::{
    ariadne,
    parser::{
        BuildContext, MultiFileBuilder, MultiFileBuilderError, ParseContext, SyntaxError,
        WithLocation, ast::LinearizeContext, inject_std_library,
    },
};
use mutica_core::{
    arc_gc::{arc::GCArcWeak, gc::GC, traceable::GCTraceable},
    scheduler::{self, ContinuationOrHandler, stack::Stack},
    stacksafe::{set_minimum_stack_size, set_stack_allocation_size},
    tokio,
    types::{AsDispatcher, GcAllocObject, Representable, TaggedPtr, Type, TypeError, TypeRef},
    util::{cycle_detector::FastCycleDetector, rootstack::RootStack},
};

// 定义一个用于GC堆分配的类型
pub struct TypeGcOnceLock {
    inner: OnceLock<Type<TypeGcOnceLock>>,
}

impl GcAllocObject<TypeGcOnceLock> for TypeGcOnceLock {
    /// 创建未初始化的不动点占位符
    ///
    /// 这是递归类型定义的第一步：创建一个"洞"，稍后填充.
    type Inner = Type<TypeGcOnceLock>;

    fn new_placeholder() -> Self {
        TypeGcOnceLock { inner: OnceLock::new() }
    }

    fn get_value(&self) -> Option<&Self::Inner> {
        self.inner.get()
    }

    fn map_value<F, R>(&self, path: &mut FastCycleDetector<TaggedPtr<()>>, f: F) -> Option<R>
    where
        F: FnOnce(
            &mut FastCycleDetector<TaggedPtr<()>>,
            <Self::Inner as AsDispatcher<Self::Inner, TypeGcOnceLock>>::RefDispatcher<'_>,
        ) -> R,
    {
        self.get_value().map(|inner| f(path, inner.as_ref_dispatcher()))
    }

    fn set_value(&self, _value: Self::Inner) -> Result<(), TypeError<Self::Inner, TypeGcOnceLock>> {
        self.inner.set(_value).map_err(|_| TypeError::RedeclaredType)
    }

    fn take_value<F, R>(&self, path: &mut FastCycleDetector<TaggedPtr<()>>, f: F) -> Option<R>
    where
        F: FnOnce(&mut FastCycleDetector<TaggedPtr<()>>, Self::Inner) -> R,
        TypeGcOnceLock: GcAllocObject<TypeGcOnceLock>,
    {
        self.get_value().map(|inner| f(path, inner.clone()))
    }
}

impl GCTraceable<TypeGcOnceLock> for TypeGcOnceLock {
    fn collect(&self, queue: &mut std::collections::VecDeque<GCArcWeak<TypeGcOnceLock>>) {
        if let Some(t) = self.inner.get() {
            t.collect(queue);
        }
    }
}

#[derive(Parser)]
#[command(name = "mutica")]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Run Mutica file
    Run {
        /// Code file path
        file: String,
    },
    /// Show Mutica version
    Version,
}

#[tokio::main]
async fn main() {
    set_stack_allocation_size(16 * 1024 * 1024); // 设置栈大小为16MB
    set_minimum_stack_size(512 * 1024); // 设置最小栈大小为512KB
    let cli = Cli::parse();
    match cli.command {
        Command::Run { file } => {
            let code = match fs::read_to_string(&file) {
                Ok(c) => c,
                Err(e) => {
                    eprintln!("Failed to read file '{}': {}", file, e);
                    process::exit(1);
                }
            };
            parse_and_reduce(&code, PathBuf::from(file)).await;
        }
        Command::Version => {
            println!("Mutica version {}", env!("CARGO_PKG_VERSION"));
        }
    }
}

fn report_builder_errors(
    builder_errors: &[WithLocation<MultiFileBuilderError>],
    path: &PathBuf,
    expr: &str,
) {
    for error_with_loc in builder_errors {
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
                report.eprint((filepath, ariadne::Source::from(source_content))).ok();
            }
            MultiFileBuilderError::IOError(e) => {
                let range = error_with_loc.location().map(|r| r.span().clone()).unwrap_or(0..0);
                ariadne::Report::build(ariadne::ReportKind::Error, filepath.as_str(), range.start)
                    .with_label(ariadne::Label::new((filepath.as_str(), range)).with_message(e))
                    .finish()
                    .eprint((filepath.as_str(), ariadne::Source::from(source_content)))
                    .ok();
            }
            MultiFileBuilderError::TopLevelBindError(var) => {
                let range = var.location().map(|r| r.span().clone()).unwrap_or(0..0);
                ariadne::Report::build(ariadne::ReportKind::Error, filepath.as_str(), range.start)
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
}

pub async fn parse_and_reduce(expr: &str, path: PathBuf) {
    #[cfg(debug_assertions)]
    println!("Parsing expression:\n{}\n", expr);

    // 使用 MultiFileBuilder 来构建整个项目
    let mut imported_ast = HashMap::new();
    let mut cycle_detector = FastCycleDetector::new();
    let mut builder_errors = Vec::new();
    let mut multifile_builder =
        MultiFileBuilder::new(&mut imported_ast, &mut cycle_detector, &mut builder_errors);
    let (mut ast, source) = multifile_builder.build(path.clone(), expr.to_string());
    if let Some((ast, _)) = ast.as_mut() {
        *ast = inject_std_library(ast.clone(), &mut builder_errors);
    }
    // 直接使用 MultiFileBuilder 构建
    let basic = match ast {
        Some(ast) if builder_errors.is_empty() => ast,
        None | Some(_) => {
            // 报告构建错误
            report_builder_errors(&builder_errors, &path, expr);
            return;
        }
    };

    // println!("Basic AST: {:#?}", basic);

    // 执行 auto_bind 转换，将 AutoBind 模式转换为 Standard 模式
    let (desugared, leftover_binds) = basic.0.auto_bind();

    // 检查是否有未处理的绑定（这通常表示顶层有 Bind 节点，这是不应该出现的）
    if !leftover_binds.is_empty() {
        for (var, _) in leftover_binds {
            builder_errors.push(var.clone().map(|_| MultiFileBuilderError::TopLevelBindError(var)));
        }
        // 报告错误
        report_builder_errors(&builder_errors, &path, expr);
        return;
    }

    let linearized =
        desugared.linearize(&mut LinearizeContext::new(), desugared.location()).finalize();
    // println!("Linearized AST: {:#?}", linearized);
    let mut flow_errors = Vec::new();
    let flowed = linearized.flow(&mut ParseContext::new(), linearized.location(), &mut flow_errors);
    // println!("Flowed AST: {:#?}", flowed.ty());

    if !flow_errors.is_empty() {
        // 获取源文件信息用于错误报告
        let filepath = source.filepath();
        let source_content = source.content();
        // 报告所有错误
        let mut has_error = false;
        for e in &flow_errors {
            let filepath =
                e.location().map(|loc| loc.source().filepath()).unwrap_or_else(|| filepath.clone());
            let source_content = e
                .location()
                .map(|loc| loc.source().content().to_string())
                .unwrap_or_else(|| source_content.to_string());
            e.report().eprint((filepath, ariadne::Source::from(source_content))).ok();
            if !e.is_warning() {
                has_error = true;
            }
        }
        if has_error {
            return;
        }
    }

    let flowed = flowed.ty().clone();

    let mut gc = GC::new();
    let mut roots = RootStack::new();
    let built_type =
        match flowed.to_type(&mut BuildContext::new(), &mut gc, &mut roots, flowed.location()) {
            Ok(result) => result,
            Err(Ok(type_error)) => {
                println!("Type building error: {:?}", type_error);
                return;
            }
            Err(Err(parse_error)) => {
                // 获取源文件信息用于错误报告
                let filepath = source.filepath();
                let source_content = source.content().to_string();
                parse_error.report().eprint((filepath, ariadne::Source::from(source_content))).ok();
                return;
            }
        };
    #[cfg(debug_assertions)]
    println!("Built type: {}\n", built_type.ty().display(&mut FastCycleDetector::new(), 0, 6));

    let mut linear_scheduler =
        roots.context(|_| scheduler::LinearScheduler::new(built_type.ty().clone(), None)); // 确保 roots 直到 linear_scheduler 被创建完成才丢弃

    let mut step_counter = 0;
    const SWEEP_INTERVAL: usize = 8192;
    let result = loop {
        // for debugging: 在每一步后进行垃圾收集和根栈清理
        #[cfg(debug_assertions)]
        gc.collect();
        #[cfg(debug_assertions)]
        linear_scheduler.sweep_roots();

        match linear_scheduler.step(&mut gc).await {
            Ok(true) => (),
            Ok(false) => break Ok(linear_scheduler.current().clone()),
            Err(e) => break Err(e),
        }

        step_counter += 1;
        if step_counter >= SWEEP_INTERVAL {
            linear_scheduler.sweep_roots();
            step_counter = 0;
        }
    };

    struct StackTraceReport {
        pub report: mutica_compiler::ariadne::Report<'static, (String, std::ops::Range<usize>)>,
        pub sources: Vec<(String, String)>, // (filepath, content) pairs
    }

    impl StackTraceReport {
        pub fn new(
            report: mutica_compiler::ariadne::Report<'static, (String, std::ops::Range<usize>)>,
            sources: Vec<(String, String)>,
        ) -> Self {
            Self { report, sources }
        }

        /// Print the report with all required sources
        pub fn eprint(&self) -> std::io::Result<()> {
            let cache = mutica_compiler::ariadne::sources(
                self.sources
                    .iter()
                    .map(|(path, content)| (path.clone(), content.clone()))
                    .collect::<std::collections::HashMap<_, _>>(),
            );
            self.report.eprint(cache)
        }
    }

    fn dump_stack_report(stack: &Stack<ContinuationOrHandler<TypeGcOnceLock>>) -> StackTraceReport {
        use mutica_compiler::ariadne::{Color, Label, Report, ReportKind};
        use mutica_core::types::CoinductiveType;
        use mutica_core::util::source_info::byte_offset_to_char_offset;

        let mut sources = Vec::new();

        if stack.is_empty() {
            let report = Report::build(ReportKind::Error, "<stack>".to_string(), 0)
                .with_message("Stack Trace")
                .with_note("Stack is empty")
                .finish();
            return StackTraceReport::new(report, sources);
        }

        let mut builder =
            Report::build(ReportKind::Error, "<stack>".to_string(), 0).with_message("Stack Trace");

        for (i, ty) in stack.iter().enumerate() {
            let (kind, ty_ref) = match ty {
                ContinuationOrHandler::Continuation(t) => ("Continuation", t),
                ContinuationOrHandler::PerformHandler(v) => ("Perform Handler", v),
            };

            let display = ty_ref.display(&mut FastCycleDetector::new(), 0, 2);

            // 尝试获取源位置信息
            if let Some(loc) = ty_ref.source_info() {
                let filepath = loc.source().filepath().to_string();
                let content = loc.source().content().to_string();
                let span = loc.span().clone();

                // 将字节偏移转换为字符偏移
                let char_start = byte_offset_to_char_offset(&content, span.start);
                let char_end = byte_offset_to_char_offset(&content, span.end);

                // 添加源文件（如果还没有添加过）
                if !sources.iter().any(|(path, _)| path == &filepath) {
                    sources.push((filepath.clone(), content.clone()));
                }

                builder = builder.with_label(
                    Label::new((filepath, char_start..char_end))
                        .with_message(format!("[{}] {}: {}", i, kind, display))
                        .with_color(Color::Cyan),
                );
            } else {
                builder = builder.with_note(format!("[{}] {}: {}", i, kind, display));
            }
        }

        let report = builder.finish();
        StackTraceReport::new(report, sources)
    }

    match result {
        Ok(v) => v
            .map(&mut FastCycleDetector::new(), |_, ty| match ty {
                TypeRef::Sequence(tuple) if tuple.is_unit() => (),
                _ => {
                    println!("{}", v.display(&mut FastCycleDetector::new(), 0, usize::MAX));
                }
            })
            .unwrap_or_else(|e| panic!("Error during type mapping: {:?}", e))
            .unwrap_or(()),
        Err(e) => {
            // Print stack trace as a report
            dump_stack_report(linear_scheduler.stack()).eprint().ok();

            // TypeErrorReport now bundles the report with all needed source files
            e.to_report().eprint().ok();
        }
    }
}
