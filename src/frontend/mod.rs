use std::fmt::{Display, Write};

use istd::index_map;

pub mod lexer;
pub mod parser;
pub mod errors;

index_map!(SymbolMap, SymbolIndex, String);


#[derive(Debug, PartialEq, Clone, Copy)]
pub struct SourceRange {
    start: usize,
    end: usize,
}


impl SourceRange {
    pub fn new(start: usize, end: usize) -> Self {
        assert!(start <= end, "start: {start} end: {end}");
        Self { start, end }
    }


    pub fn with(self, other: Self) -> Self {
        Self::new(self.start, other.end)
    }
}


impl Display for SourceRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}


#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(SymbolIndex),
    Bool(bool),
    Empty,
}


impl Literal {
    fn pretty_print(&self, handle: &mut impl Write, symbol_map: &SymbolMap) {
        let _ = write!(handle, "lit(");

        let _ = match self {
            Literal::Integer(v) => write!(handle, "{}", v),
            Literal::Float  (v) => write!(handle, "{}", v),
            Literal::String (v) => write!(handle, "{}", &symbol_map[*v]),
            Literal::Bool   (v) => write!(handle, "{}", v),
            Literal::Empty      => write!(handle, "()"),
        };
        
        let _ = write!(handle, ")");
    }
}


