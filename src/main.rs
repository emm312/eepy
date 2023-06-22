use std::{fs::read_to_string, collections::HashMap};

use clap::Parser as ClapParser;
use taube::{PrettyPrint, {
    frontend::{lexer::{lex, self}, SymbolMap}},
    ir::{IRBasicBlock, IRExpr, IRFunction, IRLinkage, IRInstr, IRModule, IRTerminator, IRType, IRValue}, backend::codegen::Codegen,
};

#[derive(ClapParser)]
struct Args {
    #[arg()]
    input_file: String,

    #[arg(short, long, default_value_t = String::from("out.o"))]
    output_file: String,

    #[arg(short, long, default_value_t = false)]
    jit: bool
}

fn main() {
    let args = Args::parse();
    let code = read_to_string(&args.input_file).unwrap().replace('\t', "    ").replace('\r', "");

    let mut symbol_map = SymbolMap::new();
    let file = symbol_map.push(args.input_file);

    let tokens = lex(&code, file, &mut symbol_map);
    let tokens = match tokens {
        Ok(v) => v,
        Err(v) => {
            let message = v.build(&HashMap::from([(file, (symbol_map[file].clone(), code))]));
            eprintln!("{message}");
            return;
        },
    };
    
    println!("{}", (&*tokens).pretty_print(&symbol_map));

    let zero = IRExpr::Value(IRValue::I8(0));
    let main_fn = IRFunction {
        name: String::from("main"),
        return_type: IRType::I8,
        args: vec![],
        blocks: Some(vec![IRBasicBlock {
            name: String::from("entry"),
            instrs: vec![
                IRInstr::Expr(IRExpr::FnCall(
                    String::from("putchar"),
                    vec![IRExpr::Value(IRValue::I8(69))],
                )),
                IRInstr::Expr(IRExpr::FnCall(
                    String::from("putchar"),
                    vec![IRExpr::Value(IRValue::I8(10))],
                )),
            ],
            terminator: IRTerminator::Ret(zero),
        }]),
        linkage: IRLinkage::Public,
    };

    let putchar_fn = IRFunction {
        name: String::from("putchar"),
        return_type: IRType::I32,
        args: vec![(String::from("c"), IRType::I8)],
        blocks: None,
        linkage: IRLinkage::External,
    };

    let ir_module = IRModule {
        functions: vec![putchar_fn, main_fn],
    };

    Codegen::compile(ir_module, args.jit);
}
