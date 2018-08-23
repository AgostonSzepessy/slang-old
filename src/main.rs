extern crate slang;

use slang::tokenizer::TokenIterator;
use slang::parser::Parser;
use slang::interpreter::Interpreter;

use std::env;
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let args: Vec<_> = env::args().collect();

    if args.len() < 2 {
        println!("filename must be specified");
        std::process::exit(1);
    }

    let filename = &args[1];
    let mut f = File::open(filename).expect("file not found");

    let mut contents = String::new();
    f.read_to_string(&mut contents).expect("something went wrong while reading file");

    let mut tokenizer = TokenIterator::new(contents.chars().peekable());
    let mut parser = Parser::new(tokenizer);
    let stmts = parser.parse().unwrap();

    let interpreter = Interpreter::new();
    interpreter.interpret(stmts);
}
