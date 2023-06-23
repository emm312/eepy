use std::{collections::HashMap, fs::read_to_string};

use clap::Parser as ClapParser;

use taube::{PrettyPrint, frontend::{lexer::{lex, self}, SymbolMap, errors::UnwrapError}, frontend::parser::parse, envs::{DUMP_TOKENS, env_flag, DUMP_AST},
    ir::{IRBasicBlock, IRExpr, IRFunction, IRLinkage, IRInstr, IRModule, IRTerminator, IRType, IRValue}, backend::codegen::Codegen,
};

#[derive(ClapParser)]
struct Args {
    #[arg()]
    input_file: String,

    #[arg(short, long, default_value_t = String::from("out.o"))]
    output_file: String,

    #[arg(short, long, default_value_t = false)]
    jit: bool,
}


fn main() {
    let args = Args::parse();
    let code = read_to_string(&args.input_file)
        .unwrap()
        .replace('\t', "    ")
        .replace('\r', "");

    let mut symbol_map = SymbolMap::new();
    let file = symbol_map.push(args.input_file);

    let tokens = lex(file, &code, &mut symbol_map);
    let tokens = tokens.unwrap_as_error(|| HashMap::from([(file, (symbol_map[file].clone(), code.clone()))]));
    
    if env_flag(DUMP_TOKENS) { println!("{}", (&*tokens).pretty_print(&symbol_map)); };


    let putchar_fn = IRFunction {
        name: String::from("putchar"),
        return_type: IRType::ZeroSized,
        args: vec![(String::from("c"), IRType::I8)],
        blocks: None,
        linkage: IRLinkage::External,
    };
    let ast = parse(file, &tokens, &mut symbol_map).unwrap_as_error(|| HashMap::from([(file, (symbol_map[file].clone(), code.clone()))]));
    if env_flag(DUMP_AST) { println!("{:#?}", ast); };

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
