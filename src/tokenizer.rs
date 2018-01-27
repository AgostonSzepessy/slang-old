use std::iter::Peekable;
use std::iter::Iterator;
use std::str::Chars;

use token::{Token, TokenError};

pub struct TokenIterator<'a> {
    chars: Peekable<Chars<'a>>,
}

impl<'a> TokenIterator<'a> {
    fn new(chars: Peekable<Chars<'a>>) -> Self {
        TokenIterator {
            chars: chars,
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        // Used for parsing operators that have an equal sign as a part of them.
        // `token` is the single character token, and `eq_token` is the character
        // with an equal sign appended to it.
        // Examples: +=, -=, !=, etc
        macro_rules! parse_op_equal_case {
            ($token: expr, $eq_token: expr) => {{
                match self.chars.peek() {
                    Some(&'=') => { 
                        self.chars.next();
                        return Some($eq_token);
                    }
                    _ => return Some($token),
                }
            }}
        }

        // Keeps track of the row that is being parsed, so when an error happens
        // the user knows which line to check
        let mut row = 1;

        while let Some(c) = self.chars.next() {
            match c {
                // Parse an identifier
                'a'...'z' | 'A'...'Z' | '_' => {
                    let mut chars = Vec::new();
                    chars.push(c);

                    // Keep scanning until we come across something that isn't
                    // alpha-numeric or an underscore.
                    while let Some(&ch) = self.chars.peek() {
                        match ch {
                            '0'...'9' | 'a'...'z' | 'A'...'Z' | '_' => {
                                chars.push(ch);
                                self.chars.next();
                            }

                            _ => break,
                        }
                    }

                    let identifier: String = chars.into_iter().collect();

                    match identifier.as_ref() {
                        "and" => return Some(Token::And),
                        "class" => return Some(Token::Class),
                        "clone" => return Some(Token::Clone),
                        "const" => return Some(Token::Const),
                        "if" => return Some(Token::If),
                        "else" => return Some(Token::Else),
                        "false" => return Some(Token::False),
                        "for" => return Some(Token::For),
                        "fn" => return Some(Token::Fn),
                        "let" => return Some(Token::Let),
                        "none" => return Some(Token::None),
                        "static" => return Some(Token::Static),
                        "super" => return Some(Token::Super),
                        "ret" => return Some(Token::Ret),
                        "this" => return Some(Token::This),
                        "true" => return Some(Token::True),
                        "while" => return Some(Token::While),
                        _ => return Some(Token::Identifier(identifier)),
                    }
                }

                // Found something that starts with a number
                '0'...'9' => {
                    // Create vector to store individual digits
                    let mut digits = Vec::new();
                    digits.push(c);

                    let mut radix = None;

                    // Check if next digit is a number
                    while let Some(&n) = self.chars.peek() {
                        match n {
                            '0'...'9' => {
                                digits.push(n);
                                self.chars.next();
                            },

                            // Try to parse a floating point number
                            '.' => {
                                digits.push(n);
                                self.chars.next();

                                while let Some(&decimal) = self.chars.peek() {
                                    match decimal {
                                        '0'...'9' => {
                                            digits.push(decimal);
                                            self.chars.next();
                                        }

                                        _ => break,
                                    }
                                }
                            }

                            // Parse hexadecimals
                            'x' => {
                                digits.push(n);
                                self.chars.next();

                                while let Some(&hex) = self.chars.peek() {
                                    match hex {
                                        '0'...'9' | 'a'...'f' | 'A'...'F' => {
                                            digits.push(hex);
                                            self.chars.next();
                                        }

                                        _ => break,
                                    }
                                }

                                radix = Some(16);
                            }

                            // Parse octal numbers
                            'o' => {
                                digits.push(n);
                                self.chars.next();

                                while let Some(&oct) = self.chars.peek() {
                                    match oct { 
                                        '0'...'7' => {
                                            digits.push(oct);
                                            self.chars.next();
                                        }

                                        _ => break,
                                    }
                                }

                                radix = Some(8);
                            }

                            // Parse binary numbers
                            'b' => {
                                digits.push(n);
                                self.chars.next();

                                while let Some(&bin) = self.chars.peek() {
                                    match bin {
                                        '0'...'1' => {
                                            digits.push(bin);
                                            self.chars.next();
                                        }

                                        _ => break,
                                    }
                                }

                                radix = Some(2);
                            }

                            _ => {
                                break;
                            },
                        }
                    }

                    // Convert string into number
                    let result: String = digits.into_iter().collect();

                    if let Some(base) = radix {
                        if let Ok(num) = i64::from_str_radix(&result[2..], base) {
                            return Some(Token::Int(num));
                        }

                        return Some(Token::Error(TokenError::MalformedNumber(result, row))); 
                    }

                    if let Ok(num) = result.parse::<i64>() {
                        return Some(Token::Int(num));
                    }
                    else if let Ok(num) = result.parse::<f64>() {
                        return Some(Token::Float(num));
                    }

                    return Some(Token::Error(TokenError::MalformedNumber(result, row)));
                },

                // Parse single character tokens
                ',' => return Some(Token::Comma),
                '.' => return Some(Token::Dot),
                ':' => return Some(Token::Colon),
                '(' => return Some(Token::LeftParen),
                ')' => return Some(Token::RightParen),
                '[' => return Some(Token::LeftBracket),
                ']' => return Some(Token::RightBracket),
                '{' => return Some(Token::LeftBrace),
                '}' => return Some(Token::RightBrace),
                ';' => return Some(Token::Semicolon),

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
                    match self.chars.peek() {
                        Some(&'-') => {
                            self.chars.next();
                            // Keep track of the comment nesting level
                            let mut num_levels = 1;

                            let mut last_comment_symbol = ' ';
                            while let Some(&nxt) = self.chars.peek() {
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

                                    _ => last_comment_symbol = ' ',
                                }

                                self.chars.next();

                                if num_levels == 0 {
                                    break;
                                }
                            }
                        },

                        // Encountered a single line comment. Consume the entire
                        // line.
                        _ => {
                            while let Some(&next) = self.chars.peek() {
                                match next {
                                    '\n' => {
                                        row += 1;
                                        self.chars.next();
                                        break;
                                    },

                                    _ => {
                                        self.chars.next();
                                    },
                                }
                            }
                        },
                    }
                },

                x if x.is_whitespace() => {
                    if x == '\n' {
                        row += 1;
                    }
                },

                _ => {
                    return Some(Token::Error(TokenError::UnexpectedCharacter(c.to_string(), row)));
                }
            }

        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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

    gen_test!(test_comma, ",", Some(Token::Comma));
    gen_test!(test_dot, ".", Some(Token::Dot));
    gen_test!(test_colon, ":", Some(Token::Colon));
    gen_test!(test_left_paren, "(", Some(Token::LeftParen));
    gen_test!(test_right_paren, ")", Some(Token::RightParen));
    gen_test!(test_left_bracket, "[", Some(Token::LeftBracket));
    gen_test!(test_right_bracket, "]", Some(Token::RightBracket));
    gen_test!(test_left_brace, "{", Some(Token::LeftBrace));
    gen_test!(test_right_brace, "}", Some(Token::RightBrace));
    gen_test!(test_semicolon, ";", Some(Token::Semicolon));

    // Test single or double character tokens
    gen_test!(test_bang, "!", Some(Token::Bang));
    gen_test!(test_bang_eq, "!=", Some(Token::BangEq));
    gen_test!(test_eq, "=", Some(Token::Eq));
    gen_test!(test_eq_eq, "==", Some(Token::EqEq));
    gen_test!(test_greater, ">", Some(Token::Greater));
    gen_test!(test_gerater_eq, ">=", Some(Token::GreaterEq));
    gen_test!(test_less, "<", Some(Token::Less));
    gen_test!(test_less_eq, "<=", Some(Token::LessEq));
    gen_test!(test_percent, "%", Some(Token::Percent));
    gen_test!(test_percent_eq, "%=", Some(Token::PercentEq));
    gen_test!(test_plus, "+", Some(Token::Plus));
    gen_test!(test_plus_eq, "+=", Some(Token::PlusEq));
    gen_test!(test_minus, "-", Some(Token::Minus));
    gen_test!(test_minus_eq, "-=", Some(Token::MinusEq));
    gen_test!(test_slash, "/", Some(Token::Slash));
    gen_test!(test_slash_eq, "/=", Some(Token::SlashEq));
    gen_test!(test_star, "*", Some(Token::Star));
    gen_test!(test_star_eq, "*=", Some(Token::StarEq));

    // Test numbers
    gen_test!(test_int, "123", Some(Token::Int(123)));
    gen_test!(test_float, "1.23", Some(Token::Float(1.23)));
    gen_test!(test_hex, "0xAB12", Some(Token::Int(0xAB12)));
    gen_test!(test_octal, "0o12", Some(Token::Int(0o12)));
    gen_test!(test_binary, "0b11", Some(Token::Int(0b11)));

    // Test keywords
    gen_test!(test_and, "and", Some(Token::And));
    gen_test!(test_class, "class", Some(Token::Class));
    gen_test!(test_clone, "clone", Some(Token::Clone));
    gen_test!(test_const, "const", Some(Token::Const));
    gen_test!(test_if, "if", Some(Token::If));
    gen_test!(test_else, "else", Some(Token::Else));
    gen_test!(test_false, "false", Some(Token::False));
    gen_test!(test_for, "for", Some(Token::For));
    gen_test!(test_fn, "fn", Some(Token::Fn));
    gen_test!(test_let, "let", Some(Token::Let));
    gen_test!(test_none, "none", Some(Token::None));
    gen_test!(test_static, "static", Some(Token::Static));
    gen_test!(test_super, "super", Some(Token::Super));
    gen_test!(test_ret, "ret", Some(Token::Ret));
    gen_test!(test_this, "this", Some(Token::This));
    gen_test!(test_true, "true", Some(Token::True));
    gen_test!(test_while, "while", Some(Token::While));
    gen_test!(test_identifier, "foo", Some(Token::Identifier("foo".to_string())));

    // Test comments
    gen_test!(test_comment_and, r##"# This is a comment
                                    and"##, Some(Token::And));
    gen_test!(test_multiline_comment_and, r##"#- This is
                                                 a multiline comment
                                                -# and"##, Some(Token::And));
    gen_test!(test_multiline_nested_comment_and, r##"#- This is
                                                        #- a nested -#
                                                        multiline comment
                                                        -# and"##, Some(Token::And));
}
