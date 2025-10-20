use clap::{Parser, Subcommand};
use std::{collections::HashMap, fs, path::PathBuf, process, sync::OnceLock};

use mutica_compiler::{
    ariadne,
    parser::{
        BuildContext, MultiFileBuilder, MultiFileBuilderError, ParseContext, PatternCounter,
        SyntaxError, ast::LinearizeContext,
    },
};
use mutica_core::{
    arc_gc::{arc::GCArcWeak, gc::GC, traceable::GCTraceable},
    scheduler::{self, ContinuationOrHandler, stack::Stack},
    types::{AsDispatcher, GcAllocObject, Representable, Type, TypeError, TypeRef},
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
        TypeGcOnceLock {
            inner: OnceLock::new(),
        }
    }

    fn get_inner(&self) -> Option<&Self::Inner> {
        self.inner.get()
    }

    fn map_inner<F, R>(
        &self,
        path: &mut FastCycleDetector<*const ()>,
        f: F,
    ) -> Result<R, TypeError<Self::Inner, TypeGcOnceLock>>
    where
        F: FnOnce(
            &mut FastCycleDetector<*const ()>,
            <Self::Inner as AsDispatcher<Self::Inner, TypeGcOnceLock>>::RefDispatcher<'_>,
        ) -> R,
    {
        match self.inner.get() {
            Some(t) => path
                .with_guard(t as *const _ as *const (), |path| t.map_inner(path, f))
                .ok_or(TypeError::InfiniteRecursion)?,
            None => Err(TypeError::UnresolvableType),
        }
    }

    fn set_inner(&self, _value: Self::Inner) -> Result<(), TypeError<Self::Inner, TypeGcOnceLock>> {
        self.inner
            .set(_value)
            .map_err(|_| TypeError::RedeclaredType)
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
}

fn main() {
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
            parse_and_reduce(&code, PathBuf::from(file));
        }
    }
}

pub fn parse_and_reduce(expr: &str, path: PathBuf) {
    #[cfg(debug_assertions)]
    println!("Parsing expression:\n{}\n", expr);

    // 使用 MultiFileBuilder 来构建整个项目
    let mut imported_ast = HashMap::new();
    let mut cycle_detector = FastCycleDetector::new();
    let mut builder_errors = Vec::new();
    let mut multifile_builder =
        MultiFileBuilder::new(&mut imported_ast, &mut cycle_detector, &mut builder_errors);
    let (ast, source) = multifile_builder.build(path.clone(), expr.to_string());
    // 直接使用 MultiFileBuilder 构建
    let basic = match ast {
        Some(ast) if builder_errors.is_empty() => ast,
        None | Some(_) => {
            // 报告构建错误
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
                        let range = error_with_loc
                            .location()
                            .map(|r| r.span().clone())
                            .unwrap_or(0..0);
                        ariadne::Report::build(
                            ariadne::ReportKind::Error,
                            filepath.as_str(),
                            range.start,
                        )
                        .with_label(ariadne::Label::new((filepath.as_str(), range)).with_message(e))
                        .finish()
                        .eprint((filepath.as_str(), ariadne::Source::from(source_content)))
                        .ok();
                    }
                }
            }
            return;
        }
    };

    // println!("Basic AST: {:#?}", basic);
    let linearized = basic
        .0
        .linearize(&mut LinearizeContext::new(), basic.0.location())
        .finalize();
    // println!("Linearized AST: {:#?}", linearized);
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
        let source_content = source.content();
        // 报告所有错误
        let mut has_error = false;
        for e in &flow_errors {
            let filepath = e
                .location()
                .map(|loc| loc.source().filepath())
                .unwrap_or_else(|| filepath.clone());
            let source_content = e
                .location()
                .map(|loc| loc.source().content().to_string())
                .unwrap_or_else(|| source_content.to_string());
            e.report()
                .eprint((filepath, ariadne::Source::from(source_content)))
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

    let mut gc = GC::new();
    let mut roots = RootStack::new();
    let built_type = match flowed.to_type(
        &mut BuildContext::new(),
        &mut PatternCounter::new(),
        false,
        &mut gc,
        &mut roots,
        flowed.location(),
    ) {
        Ok(result) => result,
        Err(Ok(type_error)) => {
            println!("Type building error: {:?}", type_error);
            return;
        }
        Err(Err(parse_error)) => {
            // 获取源文件信息用于错误报告
            let filepath = source.filepath();
            let source_content = source.content().to_string();
            parse_error
                .report()
                .eprint((filepath, ariadne::Source::from(source_content)))
                .ok();
            return;
        }
    };
    #[cfg(debug_assertions)]
    println!(
        "Built type: {}\n",
        built_type.ty().display(&mut FastCycleDetector::new())
    );

    let mut linear_scheduler =
        roots.context(|_| scheduler::LinearScheduler::new(built_type.ty().clone())); // 确保 roots 直到 linear_scheduler 被创建完成才丢弃

    let mut step_counter = 0;
    const SWEEP_INTERVAL: usize = 8192;
    let result = loop {
        // for debugging: 在每一步后进行垃圾收集和根栈清理
        #[cfg(debug_assertions)]
        gc.collect();
        #[cfg(debug_assertions)]
        linear_scheduler.sweep_roots();

        match linear_scheduler.step(&mut gc) {
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

    fn dump_stack(stack: &Stack<ContinuationOrHandler<TypeGcOnceLock>>) -> String {
        let mut result = String::new();
        for (i, ty) in stack.iter().enumerate() {
            result.push_str(&format!(
                "## [{}]: {}\n",
                i,
                match ty {
                    ContinuationOrHandler::Continuation(t) => format!(
                        "Continuation - {}",
                        t.display(&mut FastCycleDetector::new())
                    ),
                    ContinuationOrHandler::PerformHandler(v) => format!(
                        "WithPerformHandler - Perform Handler: {}",
                        v.display(&mut FastCycleDetector::new())
                    ),
                }
            ));
        }
        result
    }

    match result {
        Ok(v) => v
            .map(&mut FastCycleDetector::new(), |_, ty| match ty {
                TypeRef::Tuple(tuple) if tuple.is_empty() => (),
                _ => {
                    println!("{}", v.display(&mut FastCycleDetector::new()));
                    ()
                }
            })
            .unwrap_or_else(|e| panic!("Error during type mapping: {:?}", e)),
        Err(e) => {
            eprintln!("--- Type Reduction Error ---");
            eprintln!(
                "Continuation stack:\n{}",
                dump_stack(linear_scheduler.stack())
            );
            eprintln!("Runtime Error: {:?}", e);
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::parse_and_reduce;
    // use super::parse_and_reduce_with_io;
    #[test]
    fn test_alpha_equivalence() {
        let expr = r#"
        let A: any = x: any -> y: any -> (x, y) \ false \ false;
        let B: any = a: any -> b: any -> (a, b) \ false \ false;
        A == B
        "#; // 测试函数α等价
        parse_and_reduce(expr, PathBuf::from("test_alpha_equivalence.mutica"));

        let expr = r#"
        let A: any = x: any -> y: any -> (x, y) \ false \ false;
        let B: any = a: any -> b: any -> (b, a) \ false \ false;
        A == B
        "#; // 测试函数不等价
        parse_and_reduce(expr, PathBuf::from("test_alpha_equivalence.mutica"));
    }

    #[test]
    fn test_list_advanced() {
        let expr = r#"
        let int_list: any = rec list: (() | (int, list));
        let my_list: any = @(int, int, int, 4);
        let my_list2: any = (1, (2, @(3, 4)));
        my_list <: int_list, my_list2 <: int_list, my_list2 <: my_list
        "#;
        parse_and_reduce(expr, PathBuf::from("test_list_advanced.mutica"));
    }

    #[test]
    fn test_specialize_generalize() {
        let expr = r#"
        (1 & 2) <: false, false <: (1 & 2)
        "#;
        parse_and_reduce(expr, PathBuf::from("test_specialize_generalize.mutica"));
    }

    #[test]
    fn test_char() {
        let expr = r#"
        "Hello, world!"
        "#;
        parse_and_reduce(expr, PathBuf::from("test_char.mutica"));
    }

    #[test]
    fn test_namespace() {
        let expr = r#"
        let v1: any = MyNamespace::1;
        let v2: any = MyNamespace::2;
        let np: any = MyNamespace::any;
        v1, v2, np, v1 == v2, v1 <: np, v2 <: np
        "#;
        parse_and_reduce(expr, PathBuf::from("test_namespace.mutica"));
    }

    #[test]
    fn test_optional() {
        let expr = r#"
        let Just: any = x: any |-> Just::x;
        let Nothing: any = Nothing::();
        let Option: any = T: any |-> (Nothing | Just T);
        let safe_div: any = (x: int, y: int) |->
            match y
                | 0 => Nothing
                | _ => Just (x / y)
                | panic;
        let get_value: any = opt: Option(any) |->
            match opt
                | Just::(v: any) => v
                | Nothing::any => ()
                | panic;
        get_value(safe_div(42, 0)), get_value(safe_div(42, 2))
        "#;
        parse_and_reduce(expr, PathBuf::from("test_optional.mutica"));
    }

    #[test]
    fn test_rec() {
        let expr = r#"
        let fib: any = rec f: n: int ->
            match n
                | 0 => 0
                | 1 => 1
                | _ => f(n - 1) + f(n - 2)
                | panic
            \ false;
        fib(10)
        "#; // 测试递归函数
        parse_and_reduce(expr, PathBuf::from("test_rec.mutica"));
    }

    #[test]
    fn test_fn() {
        let expr = r#"
        let y: any = 42;
        let f: any = x: any -> x + y \ false;
        f(1)
        "#;
        parse_and_reduce(expr, PathBuf::from("test_fn.mutica"));
    }

    #[test]
    fn test_pattern() {
        let expr = r#"
        let A : int = 1;
        let B : int = 2;
        let C : (any, any) = (A, B);
        let (x: int, y: int) = C;
        x + y
        "#;
        parse_and_reduce(expr, PathBuf::from("test_pattern.mutica"));

        let expr = r#"
        let A : any = (1, (2, 3));
        match A
            | (x: int, (y: int, z: int)) => x + y * z
            | (x: int, y: int) => x + y
            | _ => 42
            | panic
        "#;
        parse_and_reduce(expr, PathBuf::from("test_pattern.mutica"));

        let expr = r#"
        let A : any = (1, 2);
        match A
            | (x: int, (y: int, z: int)) => x + y * z
            | (x: int, y: int) => x + y
            | _ => 42
            | panic
        "#;
        parse_and_reduce(expr, PathBuf::from("test_pattern.mutica"));

        let expr = r#"
        let A: int = 1;
        let B: int = 2;
        let C: any = () -> (A, B) \ false;
        C()
        "#;
        parse_and_reduce(expr, PathBuf::from("test_pattern.mutica"));
    }

    #[test]
    fn test_assert_failed() {
        let expr = r#"
        let (x: int, y: int) = (1, 2, 3);
        x + y
        "#;
        parse_and_reduce(expr, PathBuf::from("test_assert_failed.mutica"));

        let expr = r#"
        let f: any = (x: int, y: int) -> (x, y) \ false;
        f(1), f(1, 2), f(1, 2, 3)
        "#;
        parse_and_reduce(expr, PathBuf::from("test_assert_failed.mutica"));

        let expr = r#"
        let (x: int | y: int) = 1;
        x + y
        "#;
        parse_and_reduce(expr, PathBuf::from("test_assert_failed.mutica"));
    }

    #[test]
    fn test_ambiguous() {
        let expr = r#"
        match 42
            | x: (int | (int, int)) => x
            | (x: int, y: int) => x + y
            | _ => 0
            | panic
        "#;
        parse_and_reduce(expr, PathBuf::from("test_ambiguous.mutica"));

        let expr = r#"
        match 42
            | x: (int | (y: int, int)) => x
            | (x: int, y: int) => x + y
            | _ => 0
            | panic
        "#;
        parse_and_reduce(expr, PathBuf::from("test_ambiguous.mutica"));
    }

    #[test]
    fn test_fn_2() {
        let expr = r#"
        let A: any = (int | char) -> int \ false;
        let B: any = int -> int \ false;
        let C: any = int -> (char | int) \ false;
        A <: B, B <: A, A <: C, C <: A, B <: C, C <: B
        "#;
        parse_and_reduce(expr, PathBuf::from("test_fn_2.mutica"));
    }

    #[test]
    fn test_nat() {
        let expr = r#"
        // let Nat: any = rec n: (() | ((), n));
        let succ: any = x: any -> ((), x) \ false;
        let zero: any = ();
        let one: any = succ(zero);
        let two: any = succ(one);
        let three: any = succ(two);
        let four: any = succ(three);
        let five: any = succ(four);
        let add: any = rec add: (n: any, m: any) ->
            match n
                | () => m
                | ((), n1: any) => succ(add(n1, m))
                | _ => false
                | panic
            \ false;
        add(three, two), add(four, one), add(five, zero)
        "#;
        parse_and_reduce(expr, PathBuf::from("test_nat.mutica"));
    }
    #[test]
    fn test_iter() {
        let expr = r#"
        
        let list: any = @(1, 2, 3, 4, 5);
        let break: any = v: any |-> Break::v;
        let continue: any = v: any |-> Continue::v;
        let iter: any = f: any |-> rec iter: (state: any) |-> 
            match f(state)
                | Continue::(next_state: any) => iter(next_state)
                | Break::(result: any) => result
                | panic;

        let sum: any = (count: int, lst: (() | (int, any))) |-> 
            match lst
                | () => break count
                | (head: int, tail: any) => continue(count + head, tail)
                | panic;
        iter(sum)(0, list)
        "#;
        parse_and_reduce(expr, PathBuf::from("test_iter.mutica"));
    }

    #[test]
    fn test_missing_pattern() {
        let expr = r#"
            match false
                | (x: int, y: int) => x + y
                | (x: int, y: int, z: int) => x + y + z
                | panic
        "#;
        parse_and_reduce(expr, PathBuf::from("test_missing_pattern.mutica"));
    }

    #[test]
    fn test_list_append() {
        let expr = r#"
        let int_list: any = rec list: (() | (int, list));
        let append: any = rec append: (list1: any, list2: any) |->
            match list1
                | () => list2
                | (head: int, tail: any) => (head, append(tail, list2))
                | panic;
        let lst1: any = @(1, 2, 3);
        let lst2: any = @(4, 5, 6);
        let lst3: any = append(lst1, lst2);
        lst3, lst3 <: int_list
        "#;
        parse_and_reduce(expr, PathBuf::from("test_list_append.mutica"));
    }

    #[test]
    fn test_discard() {
        let expr = r#"
        discard ();
        1
        "#;
        parse_and_reduce(expr, PathBuf::from("test_discard.mutica"));
    }

    #[test]
    fn test_dict() {
        let expr = r#"
        let simple_dict: any = {
            A::1 &
            A::int &
            // A::(1, 2) & // This line would cause a type error due to conflicting types for key A when trying to access it.
            B::2 &
            C::3
        };
        let A::(x: any) = simple_dict;
        simple_dict, x, simple_dict.A
        "#;
        parse_and_reduce(expr, PathBuf::from("test_dict.mutica"));
    }

    #[test]
    fn test_coinductive() {
        let expr = r#"
        let P0: ((a: 0, b: 1) |-> a + b) = (x: int, y: 1) |-> x + y;
        P0
        "#;
        parse_and_reduce(expr, PathBuf::from("test_coinductive.mutica"));
    }

    #[test]
    fn test_simple_fn() {
        let expr = r#"
        let f: any = rec f: x: int |-> match x
            | 0 => 0
            | 1 => 1
            | _ => f(x - 2)
            | panic;
        f(11)
        "#;
        parse_and_reduce(expr, PathBuf::from("test_simple_fn.mutica"));
    }

    #[test]
    fn test_struct() {
        let expr = r#"
        let get: any = rec get: (lst: [() | (any, any)], idx: int) |->
            match idx
                | 0 => lst[0]
                | _ => get(lst[1], idx - 1)
                | panic;
        let Point: any = (x: int, y: int) |-> { x::x & y::y };
        let p: any = Point(1, 2);
        let p2: any = Point(3, 4);
        let p3: any = Point(5, 6);
        let p_list: any = @(p, p2, p3); 
        p, p.x, p.y, get(p_list, 0), get(p_list, 1), get(p_list, 2)
        "#;
        parse_and_reduce(expr, PathBuf::from("test_struct.mutica"));
    }

    #[test]
    fn test_io() {
        let expr = r#"
        let print_chars: any = rec print_chars: (chars: (() | (char, any))) |->
            match chars
                | () => ()
                | (head: char, tail: any) => (discard print!(head); print_chars(tail))
                | panic;
        print_chars("Hello, world!\n")
        "#;
        parse_and_reduce(expr, PathBuf::from("test_io.mutica"));
    }

    #[test]
    fn test_literal() {
        let expr: &'static str = r#"
        let f: any = x: any |-> x + 1;
        let g: any = #f(42);
        let h: any = (#lazy_cps:any) |-> lazy_cps;
        h(g), g
        "#;
        parse_and_reduce(expr, PathBuf::from("test_literal.mutica"));
    }

    #[test]
    fn test_complex() {
        let expr: &'static str = r#"
        let y: any = 1;
        (x: (1+y)) |-> x
        "#;
        parse_and_reduce(expr, PathBuf::from("test_complex.mutica"));
    }

    #[test]
    fn test_generic() {
        let expr: &'static str = r#"
        let id: any = T: any |-> x: T |-> x;
        id(int)(42), id(char)('a'), id(()), id(int | char)(42), id(int | char)('a')
        "#;
        parse_and_reduce(expr, PathBuf::from("test_generic.mutica"));
    }

    #[test]
    fn test_multipattern() {
        let expr: &'static str = r#"
        let (_x: int, _x: char) = (1, 'a');
        "#;
        parse_and_reduce(expr, PathBuf::from("test_multipattern.mutica"));
    }

    #[test]
    fn test_import() {
        let expr = r#"import "./iter.mu""#;
        parse_and_reduce(expr, PathBuf::from("test_import.mutica"));
    }
}
