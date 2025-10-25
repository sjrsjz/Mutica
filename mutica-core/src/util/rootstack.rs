use arc_gc::{arc::GCArc, traceable::GCTraceable};

use crate::types::{CoinductiveType, GcAllocObject};

pub struct RootStack<U: CoinductiveType<U, V>, V: GcAllocObject<V>> {
    roots_a: Vec<GCArc<V>>,
    roots_b: Vec<GCArc<V>>,
    flag: bool,
    _phantom: std::marker::PhantomData<U>,
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> Default for RootStack<U, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<U: CoinductiveType<U, V>, V: GcAllocObject<V>> RootStack<U, V> {
    pub fn new() -> Self {
        Self {
            roots_a: Vec::new(),
            roots_b: Vec::new(),
            flag: false,
            _phantom: std::marker::PhantomData,
        }
    }

    /// 向当前根栈添加根
    pub fn push(&mut self, root: GCArc<V>) {
        if self.flag {
            self.roots_b.push(root);
        } else {
            self.roots_a.push(root);
        }
    }

    /// 将一个类型连接到当前根栈
    pub fn attach(&mut self, ty: &U) {
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

pub trait Rootable<T: GCTraceable<T> + 'static> {
    #[allow(unused_variables)]
    fn upgrade(&self, collected: &mut Vec<GCArc<T>>) {}
}
