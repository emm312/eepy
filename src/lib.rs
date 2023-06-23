use frontend::SymbolMap;

pub mod backend;
pub mod frontend;
pub mod ir;
pub mod semantic_anal;

pub trait PrettyPrint {
    fn pretty_print(&self, symbol_map: &SymbolMap) -> String;
}
