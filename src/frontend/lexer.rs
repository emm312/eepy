use std::ops::Range;

use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r#"//.*"#)]
#[logos(skip r"[ \t\n\f]+")]
pub enum TokenType {
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBrace,
    #[token("]")]
    RBrace,
    #[token("{")]
    LCurly,
    #[token("}")]
    RCurly,

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("%")]
    Mod,
    #[token("/")]
    Div,
    #[token("*")]
    Mul,
    #[token("=")]
    Assign,
    #[token("==")]
    Eq,
    #[token("!=")]
    Neq,
    #[token(">")]
    Gt,
    #[token(">=")]
    Gte,
    #[token("<")]
    Lt,
    #[token("<=")]
    Lte,
    #[token("|")]
    BitOr,
    #[token("^")]
    BitXor,
    #[token("&")]
    BitAnd,
    #[token("||")]
    Or,
    #[token("&&")]
    And,

    #[token(":")]
    Colon,
    #[token("::")]
    DoubleColon,
    #[token(";")]
    Semicolon,
    #[token("#")]
    Hash,
    #[token("..")]
    DoubleDot,

    #[regex(r"[_a-zA-Z][_a-zA-Z0-9]*", |lex| lex.slice().to_string())]
    Ident(String),
    #[regex("[0-9]+", |lex| lex.slice().parse().ok())]
    NumLit(i64),

    #[token("fn")]
    Fn,
    #[token("struct")]
    Struct,
    #[token("namespace")]
    Namespace,
    #[token("unsafe")]
    Unsafe,
    #[token("promise")]
    Promise,
    #[token("extern")]
    Extern,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("while")]
    While,
    #[token("for")]
    For,
    #[token("loop")]
    Loop,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("var")]
    Var,
    #[token("ret")]
    Return,
    #[token("as")]
    As,
    #[token("mut")]
    Mut,
    #[token("const")]
    Const,
    #[token("static")]
    Static,
}

#[derive(Debug, Clone)]
pub struct Token {
    typ: TokenType,
    span: Range<usize>,
}

pub fn tokenise(src: &str) -> Vec<Token> {
    let mut lexer = TokenType::lexer(src);
    let mut ret = Vec::new();
    while let Some(typ) = lexer.next() {
        ret.push(Token {
            typ: typ.expect(&format!("Lexer error at {:?}", lexer.span())),
            span: lexer.span(),
        })
    }
    ret
}
