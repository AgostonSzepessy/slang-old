use std::iter::Peekable;
use std::iter::Iterator;
use std::str::Chars;

use token::Token;

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
        let mut row = 1;

        while let Some(c) = self.chars.next() {
            // TODO: Scan identifiers before numbers
            match c {
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

                        return Some(Token::MalformedNumber(row)); 
                    }

                    if let Ok(num) = result.parse::<i64>() {
                        return Some(Token::Int(num));
                    }
                    else if let Ok(num) = result.parse::<f64>() {
                        return Some(Token::Float(num));
                    }

                    return Some(Token::MalformedNumber(row));
                },

                '\n' => {
                    row += 1;
                },

                _ => {

                }
            }

        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! gen_num_test {
        ($name: ident, $input: expr, $expected_res: expr) => {
            #[test]
            fn $name() {
                let mut it = TokenIterator::new($input.chars().peekable());
                let token = it.next_token();

                assert_eq!(token, $expected_res);
            }
        }
    }

    gen_num_test!(test_int, "123", Some(Token::Int(123)));
    gen_num_test!(test_float, "1.23", Some(Token::Float(1.23)));
    gen_num_test!(test_hex, "0xAB12", Some(Token::Int(0xAB12)));
    gen_num_test!(test_octal, "0o12", Some(Token::Int(0o12)));
    gen_num_test!(test_binary, "0b11", Some(Token::Int(0b11)));
}
