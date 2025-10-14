use arc_gc::arc::GCArc;

use crate::types::{Type, fixpoint::FixPointInner};

pub struct RootStack {
    roots_a: Vec<GCArc<FixPointInner>>,
    roots_b: Vec<GCArc<FixPointInner>>,
    flag: bool,
}

impl RootStack {
    pub fn new() -> Self {
        Self {
            roots_a: Vec::new(),
            roots_b: Vec::new(),
            flag: false,
        }
    }

    /// 向当前根栈添加根
    pub fn push(&mut self, root: GCArc<FixPointInner>) {
        if self.flag {
            self.roots_b.push(root);
        } else {
            self.roots_a.push(root);
        }
    }

    /// 切换当前根栈并清理
    /// 将当前根栈清空，并重新收集
    pub fn sweep(&mut self, ty: &Type) {
        // 1. 获取非活跃堆栈作为目标
        let target_stack = if self.flag {
            &mut self.roots_a
        } else {
            &mut self.roots_b
        };
        target_stack.clear();
        ty.upgrade(target_stack);
        self.flag = !self.flag;
    }

    pub fn context<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        f(self)
    }
}

pub trait Rootable {
    #[allow(unused_variables)]
    fn upgrade<'roots>(&self, collected: &'roots mut Vec<GCArc<FixPointInner>>) {}
}
