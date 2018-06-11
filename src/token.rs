use std::fmt;

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
    Comma(u32, u32),
    Dot(u32, u32),
    Colon(u32, u32),
    LeftParen(u32, u32),
    RightParen(u32, u32),
    LeftBracket(u32, u32),
    RightBracket(u32, u32),
    LeftBrace(u32, u32),
    RightBrace(u32, u32),
    Semicolon(u32, u32),

    // One or two character tokens
    Bang(u32, u32),
    BangEq(u32, u32),
    Eq(u32, u32),
    EqEq(u32, u32),
    Greater(u32, u32),
    GreaterEq(u32, u32),
    Less(u32, u32),
    LessEq(u32, u32),
    Percent(u32, u32),
    PercentEq(u32, u32),
    Plus(u32, u32),
    PlusEq(u32, u32),
    Minus(u32, u32),
    MinusEq(u32, u32),
    Slash(u32, u32),
    SlashEq(u32, u32),
    Star(u32, u32),
    StarEq(u32, u32),

    // Literals
    Float(f64, u32, u32),
    String(String, u32, u32),
    Identifier(String, u32, u32),

    // Keywords
    And(u32, u32),
    Clone(u32, u32),
    Const(u32, u32),
    If(u32, u32),
    Else(u32, u32),
    False(u32, u32),
    For(u32, u32),
    Fn(u32, u32),
    Let(u32, u32),
    None(u32, u32),
    Ret(u32, u32),
    Slf(u32, u32),
    True(u32, u32),
    While(u32, u32),

    // Error tokens
    Error(TokenError),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Token::Comma(..) => write!(f, ","),
            Token::Dot(..) => write!(f, "."),
            Token::Colon(..) => write!(f, ":"),
            Token::LeftParen(..) => write!(f, ")"),
            Token::RightParen(..) => write!(f, "("),
            Token::LeftBracket(..) => write!(f, "["),
            Token::RightBracket(..) => write!(f, "]"),
            Token::LeftBrace(..) => write!(f, "{{"),
            Token::RightBrace(..) => write!(f, "}}"),
            Token::Semicolon(..) => write!(f, ";"),
            Token::Bang(..) => write!(f, "!"),
            Token::BangEq(..) => write!(f, "!="),
            Token::Eq(..) => write!(f, "="),
            Token::EqEq(..) => write!(f, "=="),
            Token::Greater(..) => write!(f, ">"),
            Token::GreaterEq(..) => write!(f, ">="),
            Token::Less(..) => write!(f, "<"),
            Token::LessEq(..) => write!(f, "<="),
            Token::Plus(..) => write!(f, "+"),
            Token::PlusEq(..) => write!(f, "+="),
            Token::Minus(..) => write!(f, "-"),
            Token::MinusEq(..) => write!(f, "-="),
            Token::Percent(..) => write!(f, "%"),
            Token::PercentEq(..) => write!(f, "%="),
            Token::Slash(..) => write!(f, "/"),
            Token::SlashEq(..) => write!(f, "/="),
            Token::Star(..) => write!(f, "*"),
            Token::StarEq(..) => write!(f, "*="),
            Token::Float(ref i, ..) => write!(f, "{}", i),
            Token::String(ref s, ..) => write!(f, "{}", s),
            Token::Identifier(ref id, ..) => write!(f, "{}", id),
            Token::And(..) => write!(f, "and"),
            Token::Clone(..) => write!(f, "clone"),
            Token::Const(..) => write!(f, "const"),
            Token::If(..) => write!(f, "if"),
            Token::Else(..) => write!(f, "else"),
            Token::False(..) => write!(f, "false"),
            Token::For(..) => write!(f, "for"),
            Token::Fn(..) => write!(f, "fn"),
            Token::Let(..) => write!(f, "let"),
            Token::None(..) => write!(f, "none"),
            Token::Ret(..) => write!(f, "ret"),
            Token::Slf(..) => write!(f, "self"),
            Token::True(..) => write!(f, "true"),
            Token::While(..) => write!(f, "while"),
            Token::Error(ref e) => write!(f, "{}", e),
        }
    }
}
