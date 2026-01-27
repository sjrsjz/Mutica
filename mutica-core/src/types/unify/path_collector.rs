use smallvec::SmallVec;
use std::{fmt::Debug, ops::ControlFlow};

use crate::util::three_valued_logic::ThreeValuedLogic;

#[derive(Debug, Clone)]
pub enum PathNode<V: Debug + Clone> {
    Single(V),
    OneOf(usize, bool), // len, is_last
}

pub struct PathCollector<'a, V: Debug + Clone> {
    pub nodes: &'a mut Vec<PathNode<V>>,
}

impl<'a, V: Debug + Clone> PathCollector<'a, V> {
    pub fn from(nodes: &'a mut Vec<PathNode<V>>) -> Self {
        Self { nodes }
    }

    pub fn push_single(&mut self, value: V) {
        self.nodes.push(PathNode::Single(value));
    }

    pub fn mark_oneof<'b>(&'b mut self, branches: usize) -> OneOfMarker<'b, V> {
        OneOfMarker { nodes: self.nodes, oneof_left: branches }
    }

    pub fn collect<F, E>(&mut self, f: F) -> Result<ThreeValuedLogic, E>
    where
        F: FnOnce(&mut Self) -> Result<ThreeValuedLogic, E>,
    {
        let len = self.nodes.len();
        let result = f(self);
        match result {
            Ok(ThreeValuedLogic::True) => Ok(ThreeValuedLogic::True),
            Ok(ThreeValuedLogic::Unknown) => {
                self.nodes.truncate(len);
                Ok(ThreeValuedLogic::Unknown)
            }
            Ok(ThreeValuedLogic::False) => {
                self.nodes.truncate(len);
                Ok(ThreeValuedLogic::False)
            }
            Err(e) => Err(e),
        }
    }
}

// --- 核心遍历逻辑 ---
impl<'a, V: Debug + Clone> PathCollector<'a, V> {
    pub fn walk<F>(&self, mut f: F)
    where
        F: FnMut(&[V]) -> ControlFlow<()>,
    {
        let mut stack = Vec::new();
        // 初始续体为空
        let continuations = SmallVec::<[(usize, usize); 8]>::new();

        let _ = self.walk_recursive(&mut stack, 0, self.nodes.len(), &continuations, &mut f);
    }

    // 这是一个通用的“片段执行器”
    // start..end: 当前要执行的代码片段
    // continuations: 执行完当前片段后，接下来要执行的片段列表（后进先出）
    #[stacksafe::stacksafe]
    fn walk_recursive<F>(
        &self,
        stack: &mut Vec<V>,
        mut pos: usize,
        end: usize,
        continuations: &[(usize, usize)], // 剩余的任务片段：[(start, end), ...]
        f: &mut F,
    ) -> ControlFlow<()>
    where
        F: FnMut(&[V]) -> ControlFlow<()>,
    {
        // 记录进入此函数时的栈深度，用于最后回溯
        let initial_len = stack.len();

        while pos < end {
            match &self.nodes[pos] {
                PathNode::Single(v) => {
                    stack.push(v.clone());
                    pos += 1;
                }
                PathNode::OneOf(_, _) => {
                    // 1. 扫描 OneOf 组，解析出所有分支范围和合并点
                    // 这里不需要递归，只是简单的线性扫描 Header
                    let mut branches = SmallVec::<[(usize, usize); 4]>::new();
                    let mut scan_pos = pos;

                    loop {
                        if let PathNode::OneOf(len, is_last) = &self.nodes[scan_pos] {
                            let start = scan_pos + 1;
                            let end = start + len;
                            branches.push((start, end));

                            scan_pos = end; // 跳到下一个分支 Header 或合并点
                            if *is_last {
                                break;
                            }
                        } else {
                            panic!("Corrupted Path: Expected OneOf header");
                        }
                    }

                    // scan_pos 现在指向 OneOf 组之后的第一个节点（合并点）
                    let merge_point = scan_pos;

                    // 2. 构造新的续体列表
                    // 逻辑：子分支跑完后 -> 跑合并点及其后续 -> 跑上层续体
                    let mut next_continuations =
                        SmallVec::<[(usize, usize); 8]>::from_slice(continuations);
                    // 只有当合并点后面还有内容时，才需要压入续体
                    if merge_point < end {
                        next_continuations.insert(0, (merge_point, end));
                    }

                    // 3. 递归分发
                    for (b_start, b_end) in branches {
                        // 递归调用会自动处理栈的 push，并在返回前 truncate 回复原状
                        // 这样保证了分支之间的隔离
                        self.walk_recursive(stack, b_start, b_end, &next_continuations, f)?;
                    }

                    // OneOf 处理完毕意味着当前线性片段被截断并分叉了
                    // 我们不需要继续执行 while 循环后的内容，因为它们已经被打包进 next_continuations
                    // 并由子分支递归触发了。
                    // 恢复栈状态（弹出 while 循环中 push 的 Single）并返回
                    stack.truncate(initial_len);
                    return ControlFlow::Continue(());
                }
            }
        }

        // 当前片段 (pos..end) 执行完毕
        // 检查是否有后续任务 (Continuation)
        if let Some((next_start, next_end)) = continuations.first() {
            // 取出第一个续体，剩下的作为新的 continuations 传下去
            self.walk_recursive(stack, *next_start, *next_end, &continuations[1..], f)?;
        } else {
            // 没有续体了，说明一条完整的路径走到头了
            f(stack)?;
        }

        // 回溯：清理当前函数压入的所有 Single
        stack.truncate(initial_len);
        ControlFlow::Continue(())
    }
}

// --- Marker 和 Guard 保持原样 ---
pub struct OneOfMarker<'a, V: Debug + Clone> {
    pub nodes: &'a mut Vec<PathNode<V>>,
    pub oneof_left: usize,
}

impl<'a, V: Debug + Clone> OneOfMarker<'a, V> {
    pub fn enter_oneof(&mut self) -> OneOfGuard<'_, V> {
        if self.oneof_left == 0 {
            panic!("No more oneof branches left");
        }
        let is_last = self.oneof_left == 1;
        self.oneof_left -= 1;
        let header_pos = self.nodes.len();
        self.nodes.push(PathNode::OneOf(0, is_last));
        OneOfGuard { nodes: self.nodes, header_pos }
    }
}

pub struct OneOfGuard<'a, V: Debug + Clone> {
    nodes: &'a mut Vec<PathNode<V>>,
    header_pos: usize,
}

impl<'a, V: Debug + Clone> OneOfGuard<'a, V> {
    pub fn path(&mut self) -> PathCollector<'_, V> {
        PathCollector { nodes: self.nodes }
    }
}

impl<'a, V: Debug + Clone> Drop for OneOfGuard<'a, V> {
    fn drop(&mut self) {
        let branch_len = self.nodes.len() - self.header_pos - 1;
        if let PathNode::OneOf(ref mut len, _) = self.nodes[self.header_pos] {
            *len = branch_len;
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_path_construction() {
        let mut nodes = Vec::new();
        let mut path = PathCollector::from(&mut nodes);

        path.push_single(1);

        {
            // 使用你的 OneOfMarker 模式
            let mut oneof_marker = path.mark_oneof(2);
            {
                // 分支 1：通过 Guard 自动管理
                let mut branch = oneof_marker.enter_oneof();
                let mut p = branch.path();
                p.push_single(2);
                p.push_single(3);
            } // branch 销毁，回填 Index 1 的长度为 2

            {
                // 分支 2
                let mut branch = oneof_marker.enter_oneof();
                branch.path().push_single(4);
            } // branch 销毁，回填 Index 4 的长度为 1
        }

        path.push_single(5);

        // 最终 nodes 结构：
        // [Single(1), OneOf(2, false), Single(2), Single(3), OneOf(1, true), Single(4), Single(5)]
        println!("{:?}", nodes);
    }

    #[test]
    fn test_path_walk() {
        let mut nodes = Vec::new();
        let mut path = PathCollector::from(&mut nodes);

        path.push_single(1);

        {
            let mut oneof_marker = path.mark_oneof(2);
            {
                let mut branch = oneof_marker.enter_oneof();
                let mut p = branch.path();
                p.push_single(2);
                p.push_single(3);
            }
            {
                let mut branch = oneof_marker.enter_oneof();
                branch.path().push_single(4);
            }
        }

        path.push_single(5);

        // 收集所有路径
        let mut paths = Vec::new();
        let path = PathCollector::from(&mut nodes);
        path.walk(|p| {
            paths.push(p.to_vec());
            ControlFlow::Continue(())
        });

        // 验证生成的路径
        assert_eq!(paths.len(), 2);
        assert_eq!(paths[0], vec![1, 2, 3, 5]);
        assert_eq!(paths[1], vec![1, 4, 5]);
    }

    #[test]
    fn test_path_walk_complex() {
        let mut nodes = Vec::new();
        let mut path = PathCollector::from(&mut nodes);

        // 构建复杂的嵌套分支结构
        path.push_single("start");

        {
            // 第一层分支：2个分支
            let mut outer_oneof = path.mark_oneof(2);

            {
                // 第一个分支：包含内嵌的3个分支
                let mut branch1 = outer_oneof.enter_oneof();
                let mut p = branch1.path();
                p.push_single("branch1");

                {
                    // 内嵌的第二层分支：3个分支
                    let mut inner_oneof = p.mark_oneof(3);

                    {
                        let mut inner_branch1 = inner_oneof.enter_oneof();
                        inner_branch1.path().push_single("inner_a");
                    }

                    {
                        let mut inner_branch2 = inner_oneof.enter_oneof();
                        let mut p = inner_branch2.path();
                        p.push_single("inner_b1");
                        p.push_single("inner_b2");
                    }

                    {
                        let mut inner_branch3 = inner_oneof.enter_oneof();
                        inner_branch3.path().push_single("inner_c");
                    }
                }
            }

            {
                // 第二个分支：简单路径
                let mut branch2 = outer_oneof.enter_oneof();
                let mut p = branch2.path();
                p.push_single("branch2");
                p.push_single("branch2_extra");
            }
        }

        path.push_single("end");

        // 收集所有路径
        let mut paths = Vec::new();
        let path = PathCollector::from(&mut nodes);
        path.walk(|p| {
            paths.push(p.to_vec());
            ControlFlow::Continue(())
        });

        // 验证生成的路径数量和内容
        assert_eq!(paths.len(), 4, "应该生成4条路径");

        // 路径 1: start -> branch1 -> inner_a -> end
        assert_eq!(paths[0], vec!["start", "branch1", "inner_a", "end"]);

        // 路径 2: start -> branch1 -> inner_b1 -> inner_b2 -> end
        assert_eq!(paths[1], vec!["start", "branch1", "inner_b1", "inner_b2", "end"]);

        // 路径 3: start -> branch1 -> inner_c -> end
        assert_eq!(paths[2], vec!["start", "branch1", "inner_c", "end"]);

        // 路径 4: start -> branch2 -> branch2_extra -> end
        assert_eq!(paths[3], vec!["start", "branch2", "branch2_extra", "end"]);

        println!("Generated paths:");
        for (i, p) in paths.iter().enumerate() {
            println!("  Path {}: {:?}", i + 1, p);
        }
    }
}
