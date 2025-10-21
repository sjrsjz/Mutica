use std::ops::Index;

pub struct Stack<T> {
    stack: Vec<T>,
    frames: Vec<(usize, usize)>,
}

impl<T> Stack<T> {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            frames: vec![(0, 0)], // (fork的上一逻辑栈帧的长度，当前逻辑栈帧新增的长度)
        }
    }

    pub fn push(&mut self, value: T) {
        self.stack.push(value);
        self.frames.last_mut().unwrap().1 += 1;
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.frames.last().unwrap().1 == 0 {
            return None;
        }
        self.frames.last_mut().unwrap().1 -= 1;
        self.stack.pop()
    }

    /// 弹出栈顶元素，并自动删除右端的空逻辑栈帧
    /// 这个自动删除操作是给尾调用优化设计的，目的是确保退出perform handler后，栈帧能够正确恢复到调用perform之前的状态
    pub fn pop_and_auto_defork(&mut self) -> Option<T> {
        // 原则上pop元素会剔除掉所有右端空的frame
        // 先删掉所有右端的空frame
        while self.frames.len() > 1 && self.frames.last().unwrap().1 == 0 {
            self.frames.pop();
        }
        if self.frames.last().unwrap().1 == 0 {
            // 最底层frame也空了
            return None;
        }
        self.frames.last_mut().unwrap().1 -= 1;
        // pop元素后，再次删掉所有右端的空frame
        while self.frames.len() > 1 && self.frames.last().unwrap().1 == 0 {
            self.frames.pop();
        }
        self.stack.pop()
    }

    pub fn len(&self) -> usize {
        self.frames.last().unwrap().0 + self.frames.last().unwrap().1
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn real_len(&self) -> usize {
        self.stack.len()
    }

    pub fn real_empty(&self) -> bool {
        self.stack.is_empty()
    }

    pub fn real_stack(&self) -> &Vec<T> {
        &self.stack
    }

    /// 从当前逻辑栈fork出一个新的逻辑栈帧
    /// fork操作允许创建空的逻辑栈帧，但是对应的pop_and_auto_defork操作会自动删除右端的空逻辑栈帧
    pub fn fork(&mut self, base_len: usize) -> bool {
        if base_len > self.len() {
            // 无法fork到比当前逻辑栈更深的地方
            return false;
        }
        self.frames.push((base_len, 0));
        true
    }

    #[stacksafe::stacksafe]
    fn __get(frames: &[(usize, usize)], index: usize) -> Option<usize> {
        // 我们需要从逻辑栈中获取元素
        if index >= frames.last().unwrap().0 + frames.last().unwrap().1 {
            // 超出逻辑栈范围
            return None;
        }

        if index < frames.last().unwrap().0 {
            // 如果index在逻辑栈的base范围内
            // 从前面的frame中获取
            if frames.len() == 1 {
                return Some(index); // 只有一个frame，直接返回index（物理栈索引）
            }
            Self::__get(&frames[..frames.len() - 1], index)
        } else {
            // 从当前frame中获取
            // 我们可以计算出父栈帧总长度
            let mut acc = 0;
            for i in 0..frames.len() - 1 {
                acc += frames[i].1; // 累加前面frame的实际新增长度
            }
            Some(acc + (index - frames.last().unwrap().0))
        }
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        Self::__get(&self.frames, index).and_then(|real_index| self.stack.get(real_index))
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        Self::__get(&self.frames, index).and_then(move |real_index| self.stack.get_mut(real_index))
    }

    pub fn clear(&mut self) {
        self.stack.clear();
        self.frames.clear();
        self.frames.push((0, 0));
    }

    pub fn frames(&self) -> &[(usize, usize)] {
        &self.frames
    }

    pub fn skip_frames<'a>(&'a self, n: usize) -> Option<StackView<'a, T>> {
        if n >= self.frames.len() {
            return None;
        }
        Some(StackView::new(
            &self.stack,
            &self.frames[..(self.frames.len() - n)],
        ))
    }

    pub fn view(&self) -> StackView<'_, T> {
        StackView::new(&self.stack, &self.frames)
    }
}

impl<T> Index<usize> for Stack<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        self.get(index).expect("Index out of bounds")
    }
}

pub struct StackIter<'a, T> {
    stack: StackView<'a, T>,
    front_index: usize, // 从头开始的索引
    back_index: usize,  // 从尾开始的索引
}

impl<'a, T> Iterator for StackIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        // 当头索引追上或超过尾索引时，迭代结束
        if self.front_index >= self.back_index {
            return None;
        }
        let item = self.stack.get(self.front_index);
        self.front_index += 1;
        item
    }
}

impl<'a, T> DoubleEndedIterator for StackIter<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        // 当头索引追上或超过尾索引时，迭代结束
        if self.front_index >= self.back_index {
            return None;
        }
        // 尾索引是“开”区间，所以先减一
        self.back_index -= 1;
        self.stack.get(self.back_index)
    }
}

impl<'a, T> ExactSizeIterator for StackIter<'a, T> {
    fn len(&self) -> usize {
        self.back_index - self.front_index
    }
}
impl<T> Stack<T> {
    pub fn iter(&self) -> StackIter<'_, T> {
        let len = self.len();
        StackIter {
            stack: StackView::new(&self.stack, &self.frames),
            front_index: 0,
            back_index: len,
        }
    }
}

pub struct StackView<'a, T> {
    stack: &'a [T],
    frames: &'a [(usize, usize)],
}

impl<'a, T> Clone for StackView<'a, T> {
    fn clone(&self) -> Self {
        Self {
            stack: self.stack,
            frames: self.frames,
        }
    }
}

impl<'a, T> StackView<'a, T> {
    pub fn new(stack: &'a [T], frames: &'a [(usize, usize)]) -> Self {
        Self { stack, frames }
    }

    pub fn get(&self, index: usize) -> Option<&'a T> {
        Stack::<T>::__get(self.frames, index).and_then(|real_index| self.stack.get(real_index))
    }
}

impl<'a, T> Index<usize> for StackView<'a, T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        self.get(index).expect("Index out of bounds")
    }
}

impl<'a, T> StackView<'a, T> {
    pub fn len(&self) -> usize {
        if let Some((base, added)) = self.frames.last() {
            base + added
        } else {
            0
        }
    }
}

impl<'a, T> StackView<'a, T> {
    pub fn iter(&self) -> StackIter<'a, T> {
        let len = self.len();
        StackIter {
            stack: self.clone(),
            front_index: 0,
            back_index: len,
        }
    }
}
