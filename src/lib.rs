use frontend::SymbolMap;

pub mod backend;
pub mod ir;
pub mod semantic_anal;
pub mod envs;
pub mod frontend;


pub trait PrettyPrint {
    fn pretty_print(&self, symbol_map: &SymbolMap) -> String;
}

