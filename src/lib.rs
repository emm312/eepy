use frontend::SymbolMap;

// pub mod backend;
pub mod ir;
pub mod semantics;
pub mod envs;
pub mod frontend;


pub trait PrettyPrint {
    fn pretty_print(&self, symbol_map: &SymbolMap) -> String;
}

