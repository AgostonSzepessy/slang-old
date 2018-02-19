use std::fmt;
use std::fmt::Write;

#[derive(Clone, Debug, Fail, PartialEq)]
pub enum TokenError {
    #[fail(display = "Encountered malformed number {} on line {}", _0, _1)]
    MalformedNumber(String, u32),
    #[fail(display = "Encountered unexpected character {} on line {}", _0, _1)]
    UnexpectedCharacter(String, u32),
}

#[derive(Clone, Debug, PartialEq)]
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
    Percent,
    PercentEq,
    Plus,
    PlusEq,
    Minus,
    MinusEq,
    Slash,
    SlashEq,
    Star,
    StarEq,

    // Literals
    Int(i64),
    Float(f64),
    String(String),
    Identifier(String),

    // Keywords
    And,
    Clone,
    Const,
    If,
    Else,
    False,
    For,
    Fn,
    Let,
    None,
    Ret,
    Slf,
    True,
    While,

    // Error tokens
    Error(TokenError),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Token::Comma => write!(f, ","),
            Token::Dot => write!(f, "."),
            Token::Colon => write!(f, ":"),
            Token::LeftParen => write!(f, ")"),
            Token::RightParen => write!(f, "("),
            Token::LeftBracket => write!(f, "["),
            Token::RightBracket => write!(f, "]"),
            Token::LeftBrace => write!(f, "{{"),
            Token::RightBrace => write!(f, "}}"),
            Token::Semicolon => write!(f, ";"),
            Token::Bang => write!(f, "!"),
            Token::BangEq => write!(f, "!="),
            Token::Eq => write!(f, "="),
            Token::EqEq => write!(f, "=="),
            Token::Greater => write!(f, ">"),
            Token::GreaterEq => write!(f, ">="),
            Token::Less => write!(f, "<"),
            Token::LessEq => write!(f, "<="),
            Token::Plus => write!(f, "+"),
            Token::PlusEq => write!(f, "+="),
            Token::Minus => write!(f, "-"),
            Token::MinusEq => write!(f, "-="),
            Token::Percent => write!(f, "%"),
            Token::PercentEq => write!(f, "%="),
            Token::Slash => write!(f, "/"),
            Token::SlashEq => write!(f, "/="),
            Token::Star => write!(f, "*"),
            Token::StarEq => write!(f, "*="),
            Token::Int(ref i) => write!(f, "{}", i),
            Token::Float(ref i) => write!(f, "{}", i),
            Token::String(ref s) => write!(f, "{}", s),
            Token::Identifier(ref id) => write!(f, "{}", id),
            Token::And => write!(f, "and"),
            Token::Clone => write!(f, "clone"),
            Token::Const => write!(f, "const"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::False => write!(f, "false"),
            Token::For => write!(f, "for"),
            Token::Fn => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::None => write!(f, "none"),
            Token::Ret => write!(f, "ret"),
            Token::Slf => write!(f, "self"),
            Token::True => write!(f, "true"),
            Token::While => write!(f, "while"),
            Token::Error(ref e) => write!(f, "{}", e),
        }
    }
}
