use clap::{Parser, Subcommand};
use std::{fs, path::PathBuf, process, sync::Arc};

use mutica_compiler::{
    SyntaxError, ariadne,
    grammar::TypeParser,
    logos::Logos,
    parser::{
        BuildContext, ParseContext, PatternCounter, SourceFile, ast::LinearizeContext,
        lexer::LexerToken,
    },
};
use mutica_core::{
    arc_gc::gc::GC,
    scheduler,
    types::{Representable, Type},
    util::cycle_detector::FastCycleDetector,
};

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
    let source_file = Arc::new(SourceFile::new(Some(path), expr.into()));

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
                    let filepath = source_file.filepath();
                    let report =
                        mutica_compiler::parser::report_error_recovery(&e, &filepath, expr);
                    let cache: (&str, ariadne::Source) =
                        (&filepath, ariadne::Source::from(expr.to_string()));
                    report.eprint(cache).ok();
                }
                return;
            }
            let basic = ast.into_basic(ast.location());
            // println!("Basic AST: {:#?}", basic);
            let linearized = basic
                .linearize(&mut LinearizeContext::new(), basic.location())
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
                let (filepath, source_content) = if let Some(location) = linearized.location() {
                    let source = location.source();
                    (source.filepath(), source.content().to_string())
                } else {
                    (source_file.filepath(), expr.to_string())
                };
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

            let mut gc = GC::new();
            let built_type = match flowed.to_type(
                &mut BuildContext::new(),
                &mut PatternCounter::new(),
                false,
                &mut gc,
                flowed.location(),
            ) {
                Ok(result) => result,
                Err(Ok(type_error)) => {
                    println!("Type building error: {:?}", type_error);
                    return;
                }
                Err(Err(parse_error)) => {
                    // 获取源文件信息用于错误报告
                    let (filepath, source_content) = if let Some(location) = flowed.location() {
                        let source = location.source();
                        (source.filepath(), source.content().to_string())
                    } else {
                        ("<input>".to_string(), expr.to_string())
                    };
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
            let mut linear_scheduler = scheduler::LinearScheduler::new(built_type.ty().clone());
            let result = loop {
                match linear_scheduler.step(&mut gc) {
                    Ok(true) => continue,
                    Ok(false) => break Ok(linear_scheduler.current_type().clone()),
                    Err(e) => break Err(e),
                }
            };
            match result {
                Ok(v) => v
                    .map(&mut FastCycleDetector::new(), |_, ty| match ty {
                        Type::Tuple(tuple) if tuple.is_empty() => (),
                        _ => {
                            println!("{}", v.display(&mut FastCycleDetector::new()));
                            ()
                        }
                    })
                    .unwrap_or_else(|e| panic!("Error during type mapping: {:?}", e)),
                Err(e) => eprintln!("Runtime Error: {:?}", e),
            }
        }
        Err(e) => {
            let syntax_error = SyntaxError::new(e);
            let filepath = source_file.filepath();
            let report = syntax_error.report(filepath.clone(), expr);
            report.eprint((filepath, ariadne::Source::from(expr))).ok();
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
}
