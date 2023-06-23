use frontend::SymbolMap;

pub mod backend;
pub mod ir;
pub mod semantic_anal;
#[rustfmt::skip]
pub mod envs;
#[rustfmt::skip]
pub mod frontend;


pub trait PrettyPrint {
    fn pretty_print(&self, symbol_map: &SymbolMap) -> String;
}
