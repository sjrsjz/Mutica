use std::{
    fmt::Debug,
    path::PathBuf,
    sync::{Arc, RwLock},
};

use crate::util::colorize::TokenColor;

pub struct SourceFile {
    path: Option<PathBuf>,
    content: String,
    color_mapping: RwLock<Vec<TokenColor>>,
}

impl Debug for SourceFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "SourceFile {{ path: {:?}, content: <{} bytes> }}",
            self.path,
            self.content.len()
        )
    }
}

impl PartialEq for SourceFile {
    fn eq(&self, other: &Self) -> bool {
        self.path == other.path && self.content == other.content
    }
}

impl SourceFile {
    pub fn new(path: Option<PathBuf>, content: String) -> Self {
        Self {
            path,
            color_mapping: RwLock::new(TokenColor::new_buffer(content.len())),
            content,
        }
    }

    pub fn filepath(&self) -> String {
        if let Some(path) = &self.path {
            // 优先返回完整路径，这样 ariadne 的 cache 才能正确匹配
            return path.to_string_lossy().to_string();
        }
        "<input>".to_string()
    }

    pub fn path(&self) -> Option<&PathBuf> {
        self.path.as_ref()
    }

    pub fn content(&self) -> &str {
        &self.content
    }

    pub fn color_mapping(&self) -> std::sync::RwLockReadGuard<'_, Vec<TokenColor>> {
        self.color_mapping
            .read()
            .expect("Failed to acquire read lock on color mapping")
    }

    pub fn color_mapping_mut(&self) -> std::sync::RwLockWriteGuard<'_, Vec<TokenColor>> {
        self.color_mapping
            .write()
            .expect("Failed to acquire write lock on color mapping")
    }
}

#[derive(Debug, Clone)]
pub struct SourceLocation {
    source: Arc<SourceFile>,
    span: std::ops::Range<usize>, // byte range
}

impl SourceLocation {
    pub fn new(source: Arc<SourceFile>, span: std::ops::Range<usize>) -> Self {
        Self { source, span }
    }

    pub fn source(&self) -> &SourceFile {
        self.source.as_ref()
    }

    pub fn span(&self) -> &std::ops::Range<usize> {
        &self.span
    }
}