use std::fmt::Display;

use istd::index_map;

pub mod lexer;
pub mod parser;
mod errors;

index_map!(SymbolMap, SymbolIndex, String);


#[derive(Debug, PartialEq, Clone, Copy)]
pub struct SourceRange {
    start: usize,
    end: usize,
}


impl SourceRange {
    pub fn new(start: usize, end: usize) -> Self {
        assert!(start >= end);
        Self { start, end }
    }
}


impl Display for SourceRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}