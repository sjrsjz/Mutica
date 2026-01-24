use stacksafe::stacksafe;
use std::cell::RefCell;

pub enum Pair<V> {
    Single(V, V),
    Structured(Vec<Pair<V>>),
    OneOf(Vec<Pair<V>>),
}

impl<V> Pair<V> {
    pub fn walk<C, Gen, F>(&self, stack: &mut Vec<C>, mut generator: Gen, mut visitor: F)
    where
        Gen: FnMut(&V, &V) -> C,
        F: FnMut(&[C]),
    {
        // 1. 使用 RefCell 包裹 generator，允许在闭包中共享可变引用
        let gen_cell = RefCell::new(&mut generator as &mut dyn FnMut(&V, &V) -> C);

        // 2. 最终的访问者适配器，将 &mut Vec 降级为 &[C] 供用户使用
        let mut final_visitor = |s: &mut Vec<C>| {
            visitor(s as &[C]);
        };

        // 3. 执行内部递归过程
        self.walk_internal(stack, &gen_cell, &mut final_visitor);
    }

    // 核心递归：使用 &mut (dyn ... + '_) 彻底解决单态化爆炸和生命周期问题
    #[stacksafe]
    #[allow(clippy::type_complexity)]
    fn walk_internal<'a, C>(
        &self,
        stack: &mut Vec<C>,
        gen_: &RefCell<&mut dyn FnMut(&V, &V) -> C>,
        // 这里的 + '_ 是关键：它允许闭包生命周期短于 'static
        cont: &mut (dyn FnMut(&mut Vec<C>) + '_),
    ) {
        match self {
            Pair::Single(l, r) => {
                let c = (gen_.borrow_mut())(l, r);
                stack.push(c);
                cont(stack); // 执行后续任务
                stack.pop();
            }
            Pair::OneOf(variants) => {
                for variant in variants {
                    variant.walk_internal(stack, gen_, cont);
                }
            }
            Pair::Structured(fields) => {
                self.walk_fields(fields, stack, gen_, cont);
            }
        }
    }

    #[stacksafe]
    #[allow(clippy::type_complexity)]
    fn walk_fields<'a, C>(
        &self,
        fields: &'a [Pair<V>],
        stack: &mut Vec<C>,
        gen_: &RefCell<&mut dyn FnMut(&V, &V) -> C>,
        cont: &mut (dyn FnMut(&mut Vec<C>) + '_),
    ) {
        if let Some((first, rest)) = fields.split_first() {
            // 构造续体：当处理完第一个字段后，递归处理剩余字段
            let mut next_cont = |s: &mut Vec<C>| {
                self.walk_fields(rest, s, gen_, cont);
            };

            // 递归进入
            first.walk_internal(stack, gen_, &mut next_cont);
        } else {
            // 所有字段处理完毕，触发当前层级的后续任务
            cont(stack);
        }
    }
}
// 测试部分保持不变，现在可以正常编译
#[cfg(test)]
mod tests {
    use super::*;

    fn run_test(pair: &Pair<&str>) -> Vec<Vec<String>> {
        let mut results = Vec::new();
        let mut stack = Vec::new();
        pair.walk(
            &mut stack,
            |l, r| format!("{}<:{}", l, r),
            |current_stack| {
                results.push(current_stack.to_vec());
            },
        );
        results
    }

    #[test]
    fn test_recursion_limit_fix() {
        // 构造一个较深的嵌套结构验证编译和运行
        let p = Pair::Structured(vec![
            Pair::Single("a", "1"),
            Pair::Structured(vec![
                Pair::Single("b", "2"),
                Pair::Structured(vec![Pair::Single("c", "3")]),
            ]),
        ]);
        let paths = run_test(&p);
        assert_eq!(paths.len(), 1);
        assert_eq!(paths[0], vec!["a<:1", "b<:2", "c<:3"]);
    }

    #[test]
    fn test_structured_cartesian_product() {
        let p = Pair::Structured(vec![
            Pair::OneOf(vec![Pair::Single("x", "A"), Pair::Single("x", "B")]),
            Pair::OneOf(vec![Pair::Single("y", "1"), Pair::Single("y", "2")]),
        ]);
        let paths = run_test(&p);
        assert_eq!(paths.len(), 4);
    }
}

#[cfg(test)]
mod stress_tests {
    use super::*;

    // 辅助函数：快速构造一个包含 N 个分支的 OneOf
    fn any_of(prefix: &'static str, count: usize) -> Pair<&'static str> {
        Pair::OneOf(
            (0..count)
                .map(|_| {
                    // 这里我们模拟一些具有特定命名规律的原子约束
                    Pair::Single(prefix, "Type")
                })
                .collect(),
        )
    }

    #[test]
    fn test_high_dimensional_cartesian_product() {
        // 场景：3层嵌套的 10x10x10 笛卡尔积
        // 逻辑：All< Any<10>, Any<10>, Any<10> >
        // 期望路径总数：10 * 10 * 10 = 1000 条

        let p =
            Pair::Structured(vec![any_of("var_a", 10), any_of("var_b", 10), any_of("var_c", 10)]);

        let mut count = 0;
        let mut stack = Vec::new();
        p.walk(
            &mut stack,
            |l, r| format!("{}:{}", l, r),
            |path| {
                count += 1;
                assert_eq!(path.len(), 3);
            },
        );

        assert_eq!(count, 1000);
    }

    #[test]
    fn test_deep_recursive_linear_structure() {
        // 场景：深达 500 层的线性 Structured 嵌套
        // 目的：测试编译器的 recursion_limit 是否被 dyn 彻底解决，以及运行时是否触发栈溢出

        let mut root = Pair::Single("leaf", "end");
        for _ in 0..500 {
            root = Pair::Structured(vec![Pair::Single("node", "step"), root]);
        }

        let mut count = 0;
        let mut stack = Vec::new();
        root.walk(
            &mut stack,
            |l, r| format!("{}:{}", l, r),
            |path| {
                count += 1;
                // 路径长度应为 500 (nodes) + 1 (leaf) = 501
                assert_eq!(path.len(), 501);
            },
        );

        assert_eq!(count, 1);
    }

    #[test]
    fn test_logical_maze_with_pruning() {
        // 场景：复杂的逻辑迷宫
        // 结构：All<
        //         Any< A, B >,
        //         Any<
        //            All< C, D >,
        //            Never (Any<空>)
        //         >
        //      >
        // 逻辑推导：
        // 1. 第二个分量中的 Never 会导致 Any<All<C,D>, Never> 坍缩为只剩 All<C,D>
        // 2. 最终结果应为 (A, C, D) 和 (B, C, D) 两条路径

        let never: Pair<&str> = Pair::OneOf(vec![]);

        let branch_1 = Pair::OneOf(vec![Pair::Single("v1", "A"), Pair::Single("v1", "B")]);

        let branch_2 = Pair::OneOf(vec![
            Pair::Structured(vec![Pair::Single("v2", "C"), Pair::Single("v3", "D")]),
            never, // 这一支应该被彻底剪掉，不产生任何路径
        ]);

        let maze = Pair::Structured(vec![branch_1, branch_2]);

        let mut results = Vec::new();
        maze.walk(
            &mut Vec::new(),
            |l, r| format!("{}~{}", l, r),
            |path| {
                results.push(path.to_vec());
            },
        );

        assert_eq!(results.len(), 2);
        assert_eq!(results[0], vec!["v1~A", "v2~C", "v3~D"]);
        assert_eq!(results[1], vec!["v1~B", "v2~C", "v3~D"]);
    }

    #[test]
    fn test_non_linear_overlap_simulation() {
        // 场景：模拟非线性模式绑定的路径
        // 结构：All< x:A, y:B, All<x:C, y:D> >
        // 目的：验证在深度嵌套下，所有变量的约束是否能按顺序累积到一个 Stack 中

        let p = Pair::Structured(vec![
            Pair::Single("x", "A"),
            Pair::Single("y", "B"),
            Pair::Structured(vec![Pair::Single("x", "C"), Pair::Single("y", "D")]),
        ]);

        let mut called = false;
        p.walk(
            &mut Vec::new(),
            |&l, &r| (l, r),
            |path| {
                called = true;
                // 验证顺序
                assert_eq!(path[0], ("x", "A"));
                assert_eq!(path[1], ("y", "B"));
                assert_eq!(path[2], ("x", "C"));
                assert_eq!(path[3], ("y", "D"));
            },
        );
        assert!(called);
    }

    #[test]
    fn test_empty_top_in_nested_all() {
        // 场景：All< A, All<>, B >
        // 逻辑：All<> 是 Top，不应提供额外约束，也不应破坏路径

        let p = Pair::Structured(vec![
            Pair::Single("v1", "A"),
            Pair::Structured(vec![]), // Top
            Pair::Single("v2", "B"),
        ]);

        let mut count = 0;
        p.walk(
            &mut Vec::new(),
            |l, r| format!("{}:{}", l, r),
            |path| {
                count += 1;
                assert_eq!(path.len(), 2);
                assert_eq!(path[0], "v1:A");
                assert_eq!(path[1], "v2:B");
            },
        );
        assert_eq!(count, 1);
    }
}
