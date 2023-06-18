use std::fs::read_to_string;

use clap::Parser as ClapParser;
use taube::frontend::lexer::tokenise;

#[derive(ClapParser)]
struct Args {
    #[arg()]
    input_file: String,

    #[arg(short, long, default_value_t = String::from("out.o"))]
    output_file: String,
}

fn main() {
    let args = Args::parse();
    let code = read_to_string(args.input_file).unwrap();
    println!("{:#?}", tokenise(&code));
}
