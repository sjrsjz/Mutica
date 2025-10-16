use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::types::{GcAllocObject, Type};

pub struct RootStack<T: GcAllocObject<T>> {
    roots_a: Vec<GCArc<T>>,
    roots_b: Vec<GCArc<T>>,
    flag: bool,
}

impl<T: GcAllocObject<T>> RootStack<T> {
    pub fn new() -> Self {
        Self {
            roots_a: Vec::new(),
            roots_b: Vec::new(),
            flag: false,
        }
    }

    /// 向当前根栈添加根
    pub fn push(&mut self, root: GCArc<T>) {
        if self.flag {
            self.roots_b.push(root);
        } else {
            self.roots_a.push(root);
        }
    }

    /// 将一个类型连接到当前根栈
    pub fn attach(&mut self, ty: &Type<T>) {
        ty.upgrade(if self.flag {
            &mut self.roots_b
        } else {
            &mut self.roots_a
        });
    }

    /// 切换当前根栈并清理
    /// 将当前根栈清空，并重新收集
    pub fn sweep(&mut self) {
        // 1. 获取非活跃堆栈作为目标
        let target_stack = if self.flag {
            &mut self.roots_a
        } else {
            &mut self.roots_b
        };
        target_stack.clear();
        self.flag = !self.flag;
    }

    pub fn context<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        f(self)
    }
}

pub trait Rootable<GcType: GCTraceable<GcType> + 'static> {
    #[allow(unused_variables)]
    fn upgrade<'roots>(&self, collected: &'roots mut Vec<GCArc<GcType>>) {}
}
