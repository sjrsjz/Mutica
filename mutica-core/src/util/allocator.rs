#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Id {
    index: usize,
    generation: u32,
}

impl Id {
    /// 获取 ID 的索引值
    pub fn index(&self) -> usize {
        self.index
    }

    /// 获取 ID 的代值
    pub fn generation(&self) -> u32 {
        self.generation
    }

    pub fn from_parts(index: usize, generation: u32) -> Self {
        Id { index, generation }
    }
}
/// 表示存储槽位的两种状态
enum Entry<T> {
    /// 槽位被占用，存储着实际的数据
    Occupied(T),
    /// 槽位空闲，存储着下一个空闲槽位的索引
    Free { next_free_index: usize },
}

/// 一个支持自动扩容的 O(1) ID 分配器
pub struct IdAllocator<T> {
    entries: Vec<Entry<T>>,
    generations: Vec<u32>,
    free_list_head: usize,
    /// 用于标记空闲链表的末尾
    sentinel: usize,
}

impl<T> IdAllocator<T> {
    /// 创建一个新的空分配器
    pub fn new() -> Self {
        let sentinel = usize::MAX;
        IdAllocator {
            entries: Vec::new(),
            generations: Vec::new(),
            free_list_head: sentinel,
            sentinel,
        }
    }

    /// 创建一个具有预设容量的分配器
    pub fn with_capacity(capacity: usize) -> Self {
        let sentinel = usize::MAX;
        let mut allocator = IdAllocator {
            entries: Vec::with_capacity(capacity),
            generations: Vec::with_capacity(capacity),
            free_list_head: sentinel,
            sentinel,
        };
        // 预填充空闲链表
        if capacity > 0 {
            allocator.grow(capacity);
        }
        allocator
    }

    /// 扩展存储并更新空闲链表
    fn grow(&mut self, additional: usize) {
        if additional == 0 {
            return;
        }

        let old_len = self.entries.len();
        let new_len = old_len + additional;
        self.entries.reserve(additional);
        self.generations.reserve(additional);

        // 将新创建的槽位链接起来
        for i in old_len..new_len {
            // 每个新槽位都指向下一个，最后一个指向之前的空闲链表头
            let next_free = if i < new_len - 1 {
                i + 1
            } else {
                self.free_list_head // 链表的尾部接上旧的链表头
            };
            self.entries.push(Entry::Free {
                next_free_index: next_free,
            });
            self.generations.push(0);
        }

        // 将空闲链表的头更新为新创建的第一个槽位
        self.free_list_head = old_len;
    }

    /// 分配一个新的 ID 并存储数据
    ///
    /// Amortized O(1).
    pub fn alloc(&mut self, value: T) -> Id {
        // 如果空闲链表为空，则进行扩容
        if self.free_list_head == self.sentinel {
            // 扩容策略：如果当前容量为0，则扩展到4，否则翻倍
            let current_len = self.entries.len();
            let new_capacity = if current_len == 0 { 4 } else { current_len };
            self.grow(new_capacity);
        }

        // 从空闲链表头部取出一个 ID
        let new_id_index = self.free_list_head;
        let entry = &mut self.entries[new_id_index];

        match entry {
            Entry::Free { next_free_index } => {
                // 更新空闲链表头
                self.free_list_head = *next_free_index;
                // 将槽位标记为占用
                *entry = Entry::Occupied(value);
            }
            Entry::Occupied(_) => {
                // 这不应该发生，意味着数据结构已损坏
                unreachable!("free_list_head pointed to an occupied entry");
            }
        }

        let generation = self.generations[new_id_index];
        Id {
            index: new_id_index,
            generation,
        }
    }

    /// 移除一个 ID 并返回其存储的数据
    ///
    /// O(1).
    pub fn dealloc(&mut self, id: Id) -> Option<T> {
        if id.index >= self.entries.len() {
            return None; // 无效 ID
        }

        if self.generations[id.index] != id.generation {
            return None; // 陈旧的 ID
        }

        // 取出当前槽位的数据，并替换为空闲标记
        let entry = &mut self.entries[id.index];
        if let Entry::Occupied(_) = entry {
            let old_entry = std::mem::replace(
                entry,
                Entry::Free {
                    // 将此槽位插入空闲链表的头部
                    next_free_index: self.free_list_head,
                },
            );

            // 更新空闲链表头
            self.free_list_head = id.index;
            self.generations[id.index] = self.generations[id.index].wrapping_add(1);

            if let Entry::Occupied(value) = old_entry {
                Some(value)
            } else {
                None
            }
        } else {
            // 尝试释放一个已经空闲的 ID
            None
        }
    }

    /// 根据 ID 获取数据的不可变引用
    pub fn get(&self, id: Id) -> Option<&T> {
        if self.generations.get(id.index) != Some(&id.generation) {
            return None;
        }
        match self.entries.get(id.index) {
            Some(Entry::Occupied(value)) => Some(value),
            _ => None,
        }
    }

    /// 根据 ID 获取数据的可变引用
    pub fn get_mut(&mut self, id: Id) -> Option<&mut T> {
        if self.generations.get(id.index) != Some(&id.generation) {
            return None;
        }
        match self.entries.get_mut(id.index) {
            Some(Entry::Occupied(value)) => Some(value),
            _ => None,
        }
    }

    /// 返回当前已分配的 ID 数量
    pub fn len(&self) -> usize {
        self.entries.len() - self.free_list_len()
    }

    /// 检查是否为空
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    // 辅助函数，计算空闲链表长度（主要用于调试）
    fn free_list_len(&self) -> usize {
        let mut count = 0;
        let mut current = self.free_list_head;
        while current != self.sentinel {
            count += 1;
            if let Some(Entry::Free { next_free_index }) = self.entries.get(current) {
                current = *next_free_index;
            } else {
                break; // 链表中断
            }
        }
        count
    }

    pub fn iter(&self) -> impl Iterator<Item = Option<&T>> {
        self.entries.iter().map(|entry| match entry {
            Entry::Occupied(value) => Some(value),
            Entry::Free { .. } => None,
        })
    }
}

// 默认构造函数
impl<T> Default for IdAllocator<T> {
    fn default() -> Self {
        Self::new()
    }
}
