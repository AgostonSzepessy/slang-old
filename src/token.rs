#[derive(Debug, PartialEq)]
pub enum Token {
    // Single character tokens
    Comma,
    Dot,
    Colon,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    Plus,
    Minus,
    Slash,
    Star,
    Semicolon,

    // One or two character tokens
    Bang,
    BangEq,
    Eq,
    EqEq,
    Greater,
    GreaterEq,
    Less,
    LessEq,

    // Literals
    Int(i64),
    Float(f64),
    String(String),
    Identifier(String),

    // Keywords
    And,
    Class,
    If,
    Else,
    False,
    For,
    Fn,
    None,
    Super,
    Ret,
    This,
    True,
    Val,
    Var,
    While,

    // Error tokens
    MalformedNumber(u32),
    UnexpectedCharacter(String, u32),
}
