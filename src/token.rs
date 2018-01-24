#[derive(Debug, Fail, PartialEq)]
pub enum TokenError {
    #[fail(display = "Encountered malformed number {} on line {}", _0, _1)]
    MalformedNumber(String, u32),
    #[fail(display = "Encountered unexpected character {} on line {}", _0, _1)]
    UnexpectedCharacter(String, u32),
}

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
    Clone,
    Const,
    If,
    Else,
    False,
    For,
    Fn,
    Let,
    None,
    Static,
    Super,
    Ret,
    This,
    True,
    While,

    // Error tokens
    Error(TokenError),
}
