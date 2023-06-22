use std::{fs::read_to_string, collections::HashMap};

use clap::Parser as ClapParser;
use taube::{PrettyPrint, frontend::{lexer::{lex, self}, SymbolMap}};

#[derive(ClapParser)]
struct Args {
    #[arg()]
    input_file: String,

    #[arg(short, long, default_value_t = String::from("out.o"))]
    output_file: String,
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
}
