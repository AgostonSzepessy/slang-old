use std::iter::Peekable;
use std::iter::Iterator;
use std::slice::Iter;
use std::str::Chars;
use std::fmt;

use either::Either;

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
    Int(i64, u32, u32),
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
    Print(u32, u32),
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
            Token::Int(ref i, ..) => write!(f, "{}", i),
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
            Token::Print(..) => write!(f, "print"),
            Token::Ret(..) => write!(f, "ret"),
            Token::Slf(..) => write!(f, "self"),
            Token::True(..) => write!(f, "true"),
            Token::While(..) => write!(f, "while"),
            Token::Error(ref e) => write!(f, "{}", e),
        }
    }
}

struct CharIter<'a> {
    chars: Peekable<Chars<'a>>,
    row: u32,
    col: u32,
}

impl<'a> CharIter<'a> {
    pub fn new(chars: Peekable<Chars<'a>>) -> Self {
        CharIter {
            chars: chars,
            row: 1,
            col: 1,
        }
    }
}

pub struct TokenIterator<'a> {
    stream: Either<CharIter<'a>, Iter<'a, Token>>,
}

impl<'a> TokenIterator<'a> {
    /// Creates a new TokenIterator.
    /// `chars`: the stream of input characters to tokenize
    pub fn new(chars: Peekable<Chars<'a>>) -> Self {
        TokenIterator {
            stream: Either::Left(CharIter::new(chars)),
        }
    }

    /// Returns the next token in the input stream.
    pub fn next_token(&mut self) -> Option<Token> {
        if let Either::Left(ref mut stream) = self.stream {
            // Used for parsing operators that have an equal sign as a part of them.
            // `token` is the single character token, and `eq_token` is the character
            // with an equal sign appended to it.
            // Examples: +=, -=, !=, etc
            macro_rules! parse_op_equal_case {
                ($token: expr, $eq_token: expr) => {{
                    let col = stream.col;
                    stream.col += 1;
                    match stream.chars.peek() {
                        Some(&'=') => { 
                            stream.chars.next();
                            stream.col += 1;
                            return Some($eq_token(stream.row, col));
                        }
                        _ => return Some($token(stream.row, col)),
                    }
                }}
            }

            macro_rules! single_char_token {
                ($token: expr) => {{
                    let col = stream.col;
                    stream.col += 1;
                    return Some($token(stream.row, col));
                }}
            }

            while let Some(c) = stream.chars.next() {
                match c {
                    // Parse an identifier
                    'a'...'z' | 'A'...'Z' | '_' => {
                        let mut chars = Vec::new();
                        chars.push(c);

                        // Start tracking the end of the identifier that we're going to read
                        let mut identifier_end = stream.col + 1;

                        // Keep scanning until we come across something that isn't
                        // alpha-numeric or an underscore.
                        while let Some(&ch) = stream.chars.peek() {
                            match ch {
                                '0'...'9' | 'a'...'z' | 'A'...'Z' | '_' => {
                                    chars.push(ch);
                                    identifier_end += 1;
                                    stream.chars.next();
                                }

                                _ => break,
                            }
                        }

                        let identifier: String = chars.into_iter().collect();

                        // Advances stream.col by the length of the current 
                        // identifier so we know where the next token will start
                        let col = stream.col;
                        stream.col = identifier_end;

                        match identifier.as_ref() {
                            "and" => return Some(Token::And(stream.row, col)),
                            "clone" => return Some(Token::Clone(stream.row, col)),
                            "const" => return Some(Token::Const(stream.row, col)),
                            "if" => return Some(Token::If(stream.row, col)),
                            "else" => return Some(Token::Else(stream.row, col)),
                            "false" => return Some(Token::False(stream.row, col)),
                            "for" => return Some(Token::For(stream.row, col)),
                            "fn" => return Some(Token::Fn(stream.row, col)),
                            "let" => return Some(Token::Let(stream.row, col)),
                            "none" => return Some(Token::None(stream.row, col)),
                            "print" => return Some(Token::Print(stream.row, col)),
                            "ret" => return Some(Token::Ret(stream.row, col)),
                            "self" => return Some(Token::Slf(stream.row, col)),
                            "true" => return Some(Token::True(stream.row, col)),
                            "while" => return Some(Token::While(stream.row, col)),
                            _ => return Some(Token::Identifier(identifier, stream.row, col)),
                        }
                    }

                    // Found something that starts with a number
                    '0'...'9' => {
                        // Create vector to store individual digits
                        let mut digits = Vec::new();
                        digits.push(c);
                        let mut num_end = stream.col + 1;

                        let mut radix = None;
                        let mut illegal_num = false;

                        // Check if next digit is a number
                        while let Some(&n) = stream.chars.peek() {
                            match n {
                                '0'...'9' => {
                                    digits.push(n);
                                    num_end += 1;
                                    stream.chars.next();
                                },

                                // Try to parse a floating point number
                                '.' => {
                                    digits.push(n);
                                    num_end += 1;
                                    stream.chars.next();

                                    while let Some(&decimal) = stream.chars.peek() {
                                        match decimal {
                                            '0'...'9' => {
                                                digits.push(decimal);
                                                stream.chars.next();
                                            }

                                            'a'...'z' | 'A'...'Z' => {
                                                digits.push(decimal);
                                                stream.chars.next();
                                                illegal_num = true;
                                            }

                                            _ => break,
                                        }
                                        num_end += 1;
                                    }
                                }

                                // Parse hexadecimals
                                'x' => {
                                    digits.push(n);
                                    num_end += 1;
                                    stream.chars.next();

                                    while let Some(&hex) = stream.chars.peek() {
                                        match hex {
                                            '0'...'9' | 'a'...'f' | 'A'...'F' => {
                                                digits.push(hex);
                                                stream.chars.next();
                                            }

                                            'g'...'z' | 'G'...'Z' => {
                                                digits.push(hex);
                                                stream.chars.next();
                                                illegal_num = true;
                                            }

                                            _ => break,
                                        }

                                        num_end += 1;
                                    }

                                    radix = Some(16);
                                }

                                // Parse octal numbers
                                'o' => {
                                    digits.push(n);
                                    num_end += 1;
                                    stream.chars.next();

                                    while let Some(&oct) = stream.chars.peek() {
                                        match oct { 
                                            '0'...'7' => {
                                                digits.push(oct);
                                                stream.chars.next();
                                            }

                                            'a'...'z' | 'A'...'Z' | '8'...'9' => {
                                                digits.push(oct);
                                                stream.chars.next();
                                                illegal_num = true;
                                            }

                                            _ => break,
                                        }
                                        
                                        num_end += 1;
                                    }

                                    radix = Some(8);
                                }

                                // Parse binary numbers
                                'b' => {
                                    digits.push(n);
                                    num_end += 1;
                                    stream.chars.next();

                                    while let Some(&bin) = stream.chars.peek() {
                                        match bin {
                                            '0'...'1' => {
                                                digits.push(bin);
                                                stream.chars.next();
                                            }

                                            '2'...'9' | 'a'...'z' | 'A'...'Z' => {
                                                digits.push(bin);
                                                stream.chars.next();
                                                illegal_num = true;
                                            }

                                            _ => break,
                                        }

                                        num_end += 1;
                                    }

                                    radix = Some(2);
                                }

                                // Came across a number such as 543z, which is not allowed
                                'a'...'z' | 'A'...'Z' => {
                                    digits.push(n);
                                    num_end += 1;
                                    stream.chars.next();
                                    illegal_num = true;
                                },

                                _ => {
                                    break;
                                },
                            }
                        }

                        // Convert string into number
                        let result: String = digits.into_iter().collect();

                        // Move stream.col forward by the size of the number
                        // we just read and use the old value of stream.col
                        // to mark where in the line the token starts
                        let col = stream.col;
                        stream.col = num_end;

                        // Found an illegal number
                        if illegal_num {
                            return Some(Token::Error(TokenError::MalformedNumber(result, stream.row)));
                        }

                        // Convert number to appropriate base and return result
                        if let Some(base) = radix {
                            if let Ok(num) = i64::from_str_radix(&result[2..], base) {
                                return Some(Token::Int(num, stream.row, col));
                            }

                            return Some(Token::Error(TokenError::MalformedNumber(result, stream.row))); 
                        }

                        // Parse as int or float
                        if let Ok(num) = result.parse::<i64>() {
                            return Some(Token::Int(num, stream.row, col));
                        }
                        else if let Ok(num) = result.parse::<f64>() {
                            return Some(Token::Float(num, stream.row, col));
                        }

                        // Something went wrong
                        return Some(Token::Error(TokenError::MalformedNumber(result, stream.row)));
                    },

                    // Parse single character tokens
                    ',' => single_char_token!(Token::Comma),
                    '.' => single_char_token!(Token::Dot),
                    ':' => single_char_token!(Token::Colon),
                    '(' => single_char_token!(Token::LeftParen),
                    ')' => single_char_token!(Token::RightParen),
                    '[' => single_char_token!(Token::LeftBracket),
                    ']' => single_char_token!(Token::RightBracket),
                    '{' => single_char_token!(Token::LeftBrace),
                    '}' => single_char_token!(Token::RightBrace),
                    ';' => single_char_token!(Token::Semicolon),

                    // Parse single or double character tokens
                    '!' => parse_op_equal_case!(Token::Bang, Token::BangEq),
                    '=' => parse_op_equal_case!(Token::Eq, Token::EqEq),
                    '>' => parse_op_equal_case!(Token::Greater, Token::GreaterEq),
                    '<' => parse_op_equal_case!(Token::Less, Token::LessEq),
                    '%' => parse_op_equal_case!(Token::Percent, Token::PercentEq),
                    '+' => parse_op_equal_case!(Token::Plus, Token::PlusEq),
                    '-' => parse_op_equal_case!(Token::Minus, Token::MinusEq),
                    '/' => parse_op_equal_case!(Token::Slash, Token::SlashEq),
                    '*' => parse_op_equal_case!(Token::Star, Token::StarEq),

                    // Encountered a comment
                    // `#` means a line comment
                    // `#- -#` is a multiline comment. These can be nested
                    '#' => {
                        stream.col += 1;
                        match stream.chars.peek() {
                            Some(&'-') => {
                                stream.chars.next();
                                stream.col += 1;
                                // Keep track of the comment nesting level
                                let mut num_levels = 1;

                                let mut last_comment_symbol = ' ';
                                while let Some(&nxt) = stream.chars.peek() {
                                    match nxt {
                                        '#' => {
                                            // Last two characters form `-#`, which means
                                            // the comment level must be decremented
                                            if last_comment_symbol == '-' {
                                                last_comment_symbol = ' ';
                                                num_levels -= 1;
                                            }
                                            else {
                                                last_comment_symbol = '#';
                                            }
                                        },

                                        '-' => {
                                            // Last two characters form `#-`, which means
                                            // comment level must be incremented
                                            if last_comment_symbol == '#' {
                                                last_comment_symbol = ' ';
                                                num_levels += 1;
                                            }
                                            else {
                                                last_comment_symbol = '-';
                                            }
                                        },

                                        '\n' => {
                                            last_comment_symbol = ' ';
                                            stream.row += 1;
                                            // Set this to 0 because it will get incremented
                                            // outside the loop every iteration
                                            stream.col = 0;
                                        },

                                        _ => last_comment_symbol = ' ',
                                    }

                                    stream.col += 1;
                                    stream.chars.next();

                                    if num_levels == 0 {
                                        break;
                                    }
                                }
                            },

                            // Encountered a single line comment. Consume the entire
                            // line.
                            _ => {
                                while let Some(&next) = stream.chars.peek() {
                                    match next {
                                        '\n' => {
                                            stream.row += 1;
                                            stream.col = 1;
                                            stream.chars.next();
                                            break;
                                        },

                                        _ => {
                                            stream.col += 1;
                                            stream.chars.next();
                                        },
                                    }
                                }
                            },
                        }
                    },

                    x if x.is_whitespace() => {
                        match x {
                            '\n' => {
                                stream.row += 1;
                                stream.col = 0;
                            },
                            ' ' | '\t' => stream.col += 1,
                            _ => (),
                        }
                    },

                    _ => {
                        return Some(Token::Error(TokenError::UnexpectedCharacter(c.to_string(), stream.row)));
                    }
                }

            }
        } else if let Either::Right(ref mut tokens) = self.stream {
            return tokens.next().cloned();
        }

        None
    }
}

impl<'a> Iterator for TokenIterator<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        self.next_token()
    }
}

impl<'a> From<&'a Vec<Token>> for TokenIterator<'a> {
    fn from(v: &'a Vec<Token>) -> Self {
        TokenIterator {
            stream: Either::Right(v.iter()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Generates a test for testing the Tokenizer. `$name` is the name of the
    /// test to generate, `$input` is the input string to test, and 
    /// `$expected_res` is the expected Token that the Tokenizer will return.
    macro_rules! gen_test {
        ($name: ident, $input: expr, $expected_res: expr) => {
            #[test]
            fn $name() {
                let mut it = TokenIterator::new($input.chars().peekable());
                let token = it.next_token();

                assert_eq!(token, $expected_res);
            }
        }
    }

    gen_test!(test_comma, ",", Some(Token::Comma(1, 1)));
    gen_test!(test_dot, ".", Some(Token::Dot(1, 1)));
    gen_test!(test_colon, ":", Some(Token::Colon(1, 1)));
    gen_test!(test_left_paren, "(", Some(Token::LeftParen(1, 1)));
    gen_test!(test_right_paren, ")", Some(Token::RightParen(1, 1)));
    gen_test!(test_left_bracket, "[", Some(Token::LeftBracket(1, 1)));
    gen_test!(test_right_bracket, "]", Some(Token::RightBracket(1, 1)));
    gen_test!(test_left_brace, "{", Some(Token::LeftBrace(1, 1)));
    gen_test!(test_right_brace, "}", Some(Token::RightBrace(1, 1)));
    gen_test!(test_semicolon, ";", Some(Token::Semicolon(1, 1)));

    // Test single or double character tokens
    gen_test!(test_bang, "!", Some(Token::Bang(1, 1)));
    gen_test!(test_bang_eq, "!=", Some(Token::BangEq(1, 1)));
    gen_test!(test_eq, "=", Some(Token::Eq(1, 1)));
    gen_test!(test_eq_eq, "==", Some(Token::EqEq(1, 1)));
    gen_test!(test_greater, ">", Some(Token::Greater(1, 1)));
    gen_test!(test_gerater_eq, ">=", Some(Token::GreaterEq(1, 1)));
    gen_test!(test_less, "<", Some(Token::Less(1, 1)));
    gen_test!(test_less_eq, "<=", Some(Token::LessEq(1, 1)));
    gen_test!(test_percent, "%", Some(Token::Percent(1, 1)));
    gen_test!(test_percent_eq, "%=", Some(Token::PercentEq(1, 1)));
    gen_test!(test_plus, "+", Some(Token::Plus(1, 1)));
    gen_test!(test_plus_eq, "+=", Some(Token::PlusEq(1, 1)));
    gen_test!(test_minus, "-", Some(Token::Minus(1, 1)));
    gen_test!(test_minus_eq, "-=", Some(Token::MinusEq(1, 1)));
    gen_test!(test_slash, "/", Some(Token::Slash(1, 1)));
    gen_test!(test_slash_eq, "/=", Some(Token::SlashEq(1, 1)));
    gen_test!(test_star, "*", Some(Token::Star(1, 1)));
    gen_test!(test_star_eq, "*=", Some(Token::StarEq(1, 1)));

    // Test numbers
    gen_test!(test_int, "123", Some(Token::Int(123, 1, 1)));
    gen_test!(test_float, "1.23", Some(Token::Float(1.23, 1, 1)));
    gen_test!(test_hex, "0xAB12", Some(Token::Int(0xAB12, 1, 1)));
    gen_test!(test_octal, "0o12", Some(Token::Int(0o12, 1, 1)));
    gen_test!(test_binary, "0b11", Some(Token::Int(0b11, 1, 1)));

    // Test keywords
    gen_test!(test_and, "and", Some(Token::And(1, 1)));
    gen_test!(test_clone, "clone", Some(Token::Clone(1, 1)));
    gen_test!(test_const, "const", Some(Token::Const(1, 1)));
    gen_test!(test_if, "if", Some(Token::If(1, 1)));
    gen_test!(test_else, "else", Some(Token::Else(1, 1)));
    gen_test!(test_false, "false", Some(Token::False(1, 1)));
    gen_test!(test_for, "for", Some(Token::For(1, 1)));
    gen_test!(test_fn, "fn", Some(Token::Fn(1, 1)));
    gen_test!(test_let, "let", Some(Token::Let(1, 1)));
    gen_test!(test_none, "none", Some(Token::None(1, 1)));
    gen_test!(test_print, "print", Some(Token::Print(1, 1)));
    gen_test!(test_ret, "ret", Some(Token::Ret(1, 1)));
    gen_test!(test_self, "self", Some(Token::Slf(1, 1)));
    gen_test!(test_true, "true", Some(Token::True(1, 1)));
    gen_test!(test_while, "while", Some(Token::While(1, 1)));
    gen_test!(test_identifier, "foo", Some(Token::Identifier("foo".to_string(), 1, 1)));

    // Test comments
    gen_test!(test_comment_and, r##"# This is a comment
                                    and"##, Some(Token::And(2, 37)));
    gen_test!(test_multiline_comment_and, r##"#- This is
                                                 a multiline comment
                                                -# and"##, Some(Token::And(3, 52)));
    gen_test!(test_multiline_nested_comment_and, r##"#- This is
                                                        #- a nested -#
                                                        multiline comment
                                                        -# and"##, Some(Token::And(4, 60)));
    gen_test!(test_comment_int, r##"# This is a comment
                                    123"##, Some(Token::Int(123, 2, 37)));

    // Test illegal numbers
    gen_test!(test_illegal_int, "543za", Some(Token::Error(TokenError::MalformedNumber("543za".to_string(), 1))));
    gen_test!(test_illegal_float, "52.23aEz", Some(Token::Error(TokenError::MalformedNumber("52.23aEz".to_string(), 1))));
    gen_test!(test_illegal_octal, "0o7ae8", Some(Token::Error(TokenError::MalformedNumber("0o7ae8".to_string(), 1))));
    gen_test!(test_illegal_octal_prefix, "23o7", Some(Token::Error(TokenError::MalformedNumber("23o7".to_string(), 1))));
    gen_test!(test_illegal_hex, "0xA1zxE2", Some(Token::Error(TokenError::MalformedNumber("0xA1zxE2".to_string(), 1))));
    gen_test!(test_illegal_hex_prefix, "23xA1", Some(Token::Error(TokenError::MalformedNumber("23xA1".to_string(), 1))));
    gen_test!(test_illegal_bin, "0b01Aefx2", Some(Token::Error(TokenError::MalformedNumber("0b01Aefx2".to_string(), 1))));
    gen_test!(test_illegal_bin_prefix, "23b10", Some(Token::Error(TokenError::MalformedNumber("23b10".to_string(), 1))));

    gen_test!(test_eof, "", None);

    #[test]
    fn test_token_vec() {
        let tokens = vec![Token::While(0, 0), Token::Int(0, 0, 0)];
        let mut it = TokenIterator::from(&tokens);
        assert_eq!(it.next(), Some(Token::While(0, 0)));
        assert_eq!(it.next(), Some(Token::Int(0, 0, 0)));
        assert_eq!(it.next(), None);
    }

    /// Generates a test case that tests two tokens as input.
    /// Parameters:
    /// * `name`: Name of the test case
    /// * `input`: Input string
    /// * `expected`: Two expected tokens, passed as a tuple
    macro_rules! test_two_tokens {
        ($name: ident, $input: expr, $expected: expr) => {
            #[test]
            fn $name() {
                let mut it = TokenIterator::new($input.chars().peekable());
                let (expected_token1, expected_token2) = $expected;
                assert_eq!(it.next(), expected_token1);
                assert_eq!(it.next(), expected_token2);
            }
        }
    }

    // Test inputs with two tokens
    test_two_tokens!(test_int_and, "123 and", (Some(Token::Int(123, 1, 1)), Some(Token::And(1, 5))));
    test_two_tokens!(test_float_and, "12.3 and", (Some(Token::Float(12.3, 1, 1)), Some(Token::And(1, 6))));
    test_two_tokens!(test_and_identifier, "and identifier", (Some(Token::And(1, 1)), Some(Token::Identifier("identifier".to_string(), 1, 5))));
    test_two_tokens!(test_int_comma, "123,", (Some(Token::Int(123, 1, 1)), Some(Token::Comma(1, 4))));
    test_two_tokens!(test_bang_false, "!false", (Some(Token::Bang(1, 1)), Some(Token::False(1, 2))));

    /// Generates a test case that tests three tokens as input.
    /// Parameters:
    /// * `name`: Name of the test case
    /// * `input`: Input string
    /// * `expected`: Three expected tokens, passed as a tuple
    macro_rules! test_three_tokens {
        ($name: ident, $input: expr, $expected: expr) => {
            #[test]
            fn $name() {
                let mut it = TokenIterator::new($input.chars().peekable());
                let (expected_token1, expected_token2, expected_token3) = $expected;
                assert_eq!(it.next(), expected_token1);
                assert_eq!(it.next(), expected_token2);
                assert_eq!(it.next(), expected_token3);
            }
        }
    }

    // Test inputs with three tokens
    test_three_tokens!(test_int_bang_eq_int, "123 != 122", (Some(Token::Int(123, 1, 1)), Some(Token::BangEq(1, 5)), Some(Token::Int(122, 1, 8))));
}
