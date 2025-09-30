//! # Mutica
//! Mutica 是一个实验性的动态类型函数式编程语言，采用子类型化类型系统。在类型层面完全统一值和类型的概念。
//! 支持结构化子类型判定，支持alpha等价，支持递归类型等。
//! 本项目仅考虑可实现性和类型系统的表达能力，性能并非首要目标。

use lalrpop_util::lalrpop_mod;

pub mod parser;
pub mod types;
pub mod util;

lalrpop_mod!(pub grammar, "/parser/grammar.rs");

use std::ops::ControlFlow;

use arc_gc::gc::GC;
use logos::Logos;
use smallvec::smallvec;

use crate::{
    parser::{
        BuildContext, ParseContext,
        lexer::{LexerToken, LexicalError},
    },
    types::{
        CoinductiveType, Representable, Type,
        apply::Apply,
        character_value::CharacterValue,
        closure::{ClosureEnv, ParamEnv},
        list::List,
        tuple::Tuple,
    },
    util::{collector::Collector, cycle_detector::FastCycleDetector},
};

fn print_parse_error(expr: &str, e: lalrpop_util::ParseError<usize, LexerToken, LexicalError>) {
    use lalrpop_util::ParseError::*;
    let pos_to_line_col = |pos: usize| {
        let start_of_line = expr[..pos].rfind('\n').map(|i| i + 1).unwrap_or(0);
        let end_of_line = expr[pos..]
            .find('\n')
            .map(|i| pos + i)
            .unwrap_or(expr.len());
        let line_str = &expr[start_of_line..end_of_line];
        let line_no = expr[..start_of_line]
            .bytes()
            .filter(|&b| b == b'\n')
            .count()
            + 1;
        let col_chars = expr[start_of_line..pos].chars().count();
        (line_no, col_chars, line_str, start_of_line)
    };
    match e {
        InvalidToken { location } => {
            let (ln, col, line_str, _) = pos_to_line_col(location);
            println!("Parse error: Invalid token at line {} column {}", ln, col);
            println!("  {}", line_str);
            println!("  {}^", " ".repeat(col));
        }
        UnrecognizedToken {
            token: (start, token, end),
            expected,
        } => {
            let (ln, col, line_str, _line_start) = pos_to_line_col(start);
            let token_text = expr
                .get(start..end)
                .map(|s| s.to_string())
                .unwrap_or_else(|| format!("{:?}", token));
            let caret_len = token_text.chars().count().max(1);
            println!(
                "Parse error: Unrecognized token {:?} at line {} column {}",
                token, ln, col
            );
            println!("  {}", line_str);
            println!("  {}{}", " ".repeat(col), "^".repeat(caret_len));
            if !expected.is_empty() {
                println!("  expected: {}", expected.join(", "));
            }
        }
        UnrecognizedEof { location, expected } => {
            let (ln, col, line_str, _line_start) = pos_to_line_col(location);
            println!(
                "Parse error: Unexpected end of input at line {} column {}",
                ln, col
            );
            println!("  {}", line_str);
            println!("  {}^", " ".repeat(col));
            if !expected.is_empty() {
                println!("  expected: {}", expected.join(", "));
            }
        }
        ExtraToken {
            token: (start, token, end),
        } => {
            let (ln, col, line_str, _line_start) = pos_to_line_col(start);
            let token_text = expr
                .get(start..end)
                .map(|s| s.to_string())
                .unwrap_or_else(|| format!("{:?}", token));
            let caret_len = token_text.chars().count().max(1);
            println!(
                "Parse error: Extra token {:?} at line {} column {}",
                token, ln, col
            );
            println!("  {}", line_str);
            println!("  {}{}", " ".repeat(col), "^".repeat(caret_len));
        }
        User { error } => {
            println!("Parse error (user lexer error): {:?}", error);
        }
    }
}

pub fn parse_and_reduce(expr: &str) {
    let lexer = LexerToken::lexer(expr);
    let spanned_lexer = lexer.spanned().map(|(token_result, span)| {
        let token = token_result?;
        Ok((span.start, token, span.end))
    });

    let parser = crate::grammar::TypeParser::new();
    let parsed = parser.parse(spanned_lexer);
    match parsed {
        Ok(ast) => {
            let mut gc = GC::new();
            let basic = ast.into_basic();
            // println!("Basic AST: {:?}", basic);
            let flowed = basic
                .flow(&mut ParseContext::new(), false)
                .unwrap()
                .ty()
                .clone();
            let built_type = flowed
                .to_type(&mut BuildContext::new(), false, &mut gc)
                .unwrap();
            let reduced = built_type.ty().reduce(
                &ClosureEnv::new(Vec::<Type>::new()),
                &ParamEnv::from_collector(Collector::new()),
                &mut smallvec![],
                &mut gc,
            );
            match reduced {
                Ok(v) => println!("{}", v.display(&mut FastCycleDetector::new())),
                Err(e) => println!("{:?}", e),
            }
        }
        Err(e) => print_parse_error(expr, e),
    }
}

pub fn parse_and_reduce_with_io(expr: &str) {
    let lexer = LexerToken::lexer(expr);

    let spanned_lexer = lexer.spanned().map(|(token_result, span)| {
        let token = token_result?;
        Ok((span.start, token, span.end))
    });

    let parser = crate::grammar::TypeParser::new();
    let parsed = parser.parse(spanned_lexer);
    match parsed {
        Ok(ast) => {
            let mut gc = GC::new();
            let basic = ast.into_basic();
            // println!("Basic AST: {:?}", basic);
            let flowed = basic
                .flow(&mut ParseContext::new(), false)
                .unwrap()
                .ty()
                .clone();
            let built_type = flowed
                .to_type(&mut BuildContext::new(), false, &mut gc)
                .unwrap();
            let mut reduced = built_type.ty().clone();
            let reduced = loop {
                let result =
                    {
                        match reduced.reduce(
                            &ClosureEnv::new(Vec::<Type>::new()),
                            &ParamEnv::from_collector(Collector::new()),
                            &mut smallvec![],
                            &mut gc,
                        ) {
                            Ok(r) => r.map(&mut FastCycleDetector::new(), |_, ty| match ty {
                                Type::Effect(eff) => eff.payload().map(
                                    &mut FastCycleDetector::new(),
                                    |_, payload| match payload {
                                        Type::Namespace(ns) if ns.tag() == "Print" => {
                                            print!(
                                                "{}",
                                                ns.expr().display(&mut FastCycleDetector::new())
                                            );
                                            ControlFlow::Continue(Apply::new(
                                                eff.then(),
                                                Tuple::new(Vec::<Type>::new()),
                                            ))
                                        }
                                        Type::Namespace(ns) if ns.tag() == "Input" => {
                                            use std::io;
                                            let mut input = String::new();
                                            io::stdin().read_line(&mut input).unwrap();
                                            let input = input.trim_end().to_string();
                                            ControlFlow::Continue(Apply::new(
                                                eff.then(),
                                                List::new(
                                                    input
                                                        .chars()
                                                        .map(|c| CharacterValue::new(c))
                                                        .collect::<Vec<_>>(),
                                                ),
                                            ))
                                        }
                                        _ => ControlFlow::Break(ty.clone().stabilize()),
                                    },
                                ),
                                v => Ok(ControlFlow::Break(v.clone().stabilize())),
                            }),
                            Err(e) => Err(e),
                        }
                    };
                match result {
                    Ok(Ok(ControlFlow::Continue(v))) => reduced = v,
                    Ok(Ok(ControlFlow::Break(v))) => break Ok(v),
                    Err(e) | Ok(Err(e)) => {
                        break Err(e);
                    }
                }
            };
            match reduced {
                Ok(v) => println!("{}", v.display(&mut FastCycleDetector::new())),
                Err(e) => println!("{:?}", e),
            }
        }
        Err(e) => print_parse_error(expr, e),
    }
}

#[cfg(test)]
mod tests {
    use super::parse_and_reduce;
    use super::parse_and_reduce_with_io;
    #[test]
    fn test_alpha_equivalence() {
        let expr = r#"
        let A: any = x: any -> y: any -> (x, y) \ false \ false;
        let B: any = a: any -> b: any -> (a, b) \ false \ false;
        A == B
        "#; // 测试函数α等价
        parse_and_reduce(expr);

        let expr = r#"
        let A: any = x: any -> y: any -> (x, y) \ false \ false;
        let B: any = a: any -> b: any -> (b, a) \ false \ false;
        A == B
        "#; // 测试函数不等价
        parse_and_reduce(expr);
    }

    #[test]
    fn test_list_advanced() {
        let expr = r#"
        let int_list: any = rec list: (() | (int, list));
        let my_list: any = @(int, int, int, 4);
        let my_list2: any = (1, (2, @(3, 4)));
        my_list <: int_list, my_list2 <: int_list, my_list2 <: my_list
        "#;
        parse_and_reduce(expr);
    }

    #[test]
    fn test_specialize_generalize() {
        let expr = r#"
        (1 & 2) <: false, false <: (1 & 2)
        "#;
        parse_and_reduce(expr);
    }

    #[test]
    fn test_char() {
        let expr = r#"
        "Hello, world!"
        "#;
        parse_and_reduce(expr);
    }

    #[test]
    fn test_namespace() {
        let expr = r#"
        let v1: any = MyNamespace::1;
        let v2: any = MyNamespace::2;
        let np: any = MyNamespace::any;
        v1, v2, np, v1 == v2, v1 <: np, v2 <: np
        "#;
        parse_and_reduce(expr);
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
                | ! => Just (x / y);
        let get_value: any = opt: (Nothing::() | Just::any) |->
            match opt
                | Just::(v: any) => v
                | Nothing::any => ()
                | panic;
        get_value(safe_div(42, 0)), get_value(safe_div(42, 2))
        "#;
        parse_and_reduce(expr);
    }

    #[test]
    fn test_rec() {
        let expr = r#"
        let fib: any = rec f: n: int ->
            match n
                | 0 => 0
                | 1 => 1
                | ! => f(n - 1) + f(n - 2)
            \ false;
        fib(10)
        "#; // 测试递归函数
        parse_and_reduce(expr);
    }

    #[test]
    fn test_fn() {
        let expr = r#"
        let y: any = 42;
        let f: any = x: any -> x + y \ false;
        f(1)
        "#;
        parse_and_reduce(expr);
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
        parse_and_reduce(expr);

        let expr = r#"
        let A : any = (1, (2, 3));
        match A
            | (x: int, (y: int, z: int)) => x + y * z
            | (x: int, y: int) => x + y
            | ! => 42
        "#;
        parse_and_reduce(expr);

        let expr = r#"
        let A : any = (1, 2);
        match A
            | (x: int, (y: int, z: int)) => x + y * z
            | (x: int, y: int) => x + y
            | ! => 42
        "#;
        parse_and_reduce(expr);

        let expr = r#"
        let A: int = 1;
        let B: int = 2;
        let C: any = () -> (A, B) \ false;
        C()
        "#;
        parse_and_reduce(expr);
    }

    #[test]
    fn test_effects() {
        let expr = r#"
        let f: any = () -> {
            let x: int = 1;
            perform y: int = A::2;
            perform z: int = B::3;
            x + y * z
        } \ false;
        let handler: any = rec h: v: any -> match v 
            | A::(x: int) ~ (f: any) => h(f(x + 1))
            | B::(x: int) ~ (f: any) => h(f(x * 10))
            | v: int => v
            | ! => false
        \ false;
        handler(f())
        "#;
        parse_and_reduce(expr);
    }

    #[test]
    #[should_panic]
    fn test_assert_failed() {
        let expr = r#"
        let (x: int, y: int) = (1, 2, 3);
        x + y
        "#;
        parse_and_reduce(expr);

        let expr = r#"
        let f: any = (x: int, y: int) -> (x, y) \ false;
        f(1), f(1, 2), f(1, 2, 3)
        "#;
        parse_and_reduce(expr);

        let expr = r#"
        let (x: int | y: int) = 1;
        x + y
        "#;
        parse_and_reduce(expr);
    }

    #[test]
    #[should_panic]
    fn test_ambiguous() {
        let expr = r#"
        match 42
            | x: (int | (int, int)) => x
            | (x: int, y: int) => x + y
            | ! => 0
        "#;
        parse_and_reduce(expr);

        let expr = r#"
        match 42
            | x: (int | (y: int, int)) => x
            | (x: int, y: int) => x + y
            | ! => 0
        "#;
        parse_and_reduce(expr);
    }

    #[test]
    fn test_fn_2() {
        let expr = r#"
        let A: any = (int | char) -> int \ false;
        let B: any = int -> int \ false;
        let C: any = int -> (char | int) \ false;
        A <: B, B <: A, A <: C, C <: A, B <: C, C <: B
        "#;
        parse_and_reduce(expr);
    }

    #[test]
    fn test_nat() {
        let expr = r#"
        let Nat: any = rec n: (() | ((), n));
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
                | ! => false
            \ false;
        add(three, two), add(four, one), add(five, zero)
        "#;
        parse_and_reduce(expr);
    }
    #[test]
    fn test_iter() {
        let expr = r#"
        
        let list: any = @(1, 2, 3, 4, 5);
        let break: any = v: any |-> (perform _: any = Break::v;);
        let continue: any = v: any |-> {
            perform next: any = Continue::v;
            next
        };
        let iter: any = f: any |-> rec iter: (state: any) |-> 
            match f(state)
                | Continue::(next_state: any) ~ (next_f: any) => iter(next_f(next_state))
                | Break::(result: any) ~ (next_f: any) => result
                | panic;

        let sum: any = (count: int, lst: (() | (int, any))) |-> 
            match lst
                | () => break count
                | (head: int, tail: any) => continue(count + head, tail)
                | panic;
        iter(sum)(0, list)
        "#;
        parse_and_reduce(expr);
    }

    #[test]
    fn test_tmp() {
        let expr = r#"
            match false
                | (x: int, y: int) => x + y
                | (x: int, y: int, z: int) => x + y + z
                | panic
        "#;
        parse_and_reduce(expr);
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
        parse_and_reduce(expr);
    }

    #[test]
    fn test_discard() {
        let expr = r#"
        # (let A: any = 1;);
        perform 2;
        1
        "#;
        parse_and_reduce(expr);
    }

    #[test]
    fn test_dict() {
        let expr = r#"
        let simple_dict: any = {
            A::1 &
            A::int &
            A::(1, 2) &
            B::2 &
            C::3
        };
        let A::(x: any) = simple_dict;
        simple_dict, x, simple_dict.A
        "#;
        parse_and_reduce(expr);
    }

    #[test]
    fn test_io() {
        // let expr = r#"
        // perform Print::"Hello, world!";
        // perform v : any = Input::"Please enter something: ";
        // perform Print::v;
        // "#;
        // parse_and_reduce_with_io(expr);

        let expr = r#"
        let println: any = next: any |-> rec println: (chars: (() | (char, any))) |->
            match chars
                | () => {
                    perform Print::'\n';
                    next()
                }
                | (head: char, tail: any) => {
                    perform Print::head;
                    println(tail)
                }
                | panic;
        do println with "Please enter something: " as ();
        perform v: any = Input::();
        do println with v as ();
        "#;
        parse_and_reduce_with_io(expr);
    }

    #[test]
    fn test_coinductive() {
        let expr = r#"
        let P0: ((a: 0, b: 1) |-> a + b) = (x: int, y: 1) |-> x + y;
        P0
        "#;
        parse_and_reduce(expr);
    }

    #[test]
    fn test_struct() {
        let expr = r#"
        let get: any = rec get: (lst: [() | (any, any)], idx: int) |->
            match idx
                | 0 => lst[0]
                | ! => get(lst[1], idx - 1);
        let Point: any = (x: int, y: int) |-> { x::x & y::y };
        let p: any = Point(1, 2);
        let p2: any = Point(3, 4);
        let p3: any = Point(5, 6);
        let p_list: any = @(p, p2, p3); 
        p, p.x, p.y, get(p_list, 0), get(p_list, 1), get(p_list, 2)
        "#;
        parse_and_reduce(expr);
    }

    #[test]
    fn test_effect_list() {
        let expr = r#"
        let run: any = v: any |-> (
            rec run: (v: any, root: any) |-> 
                match v
                    | (payload: any) ~ (next: any) => {
                        let r: any = run(payload, false);
                        match root
                            | false => X: any |-> r[v: any |-> [next(v)] ~ X]
                            | ! => r[v: any |-> next(v)]
                    }
                    | result: any => v: any |-> result ~ v
                    | panic
            )(v, true);

        let handler: any = rec h: v: any |-> match v 
            | () ~ (f: any) => h(f())
            | (payload: any) ~ (f: any) => {
                perform v: any = payload;
                h(f(v))
            }
            | v: any => v
            | panic;

        let f: any = () |-> {
            perform Print::'A';
            let simple_num: any = 42;
            perform Print::simple_num;
        };

        handler(run {
            perform f();
            perform Print::'C';
            perform Print::'D';
        })
        "#;
        parse_and_reduce_with_io(expr);
    }

    #[test]
    fn test_effect_tree() {
        let expr = r#"
        let effects: any = [perform A::();
            perform (
                perform (
                    perform B::();
                    perform C::();
                );
                perform D::();
                perform E::();
            );
            perform F::();];

        let handler: any = rec h: v: any |-> match v
            | (subeffect: (any ~ any)) ~ (f: any) => {

            }
            | (payload: any) ~ (f: any) => {
                perform v: any = payload;
                h(f(v))
            }
            | v: any => v
            | panic;

        "#;
        parse_and_reduce_with_io(expr);
    }
}
