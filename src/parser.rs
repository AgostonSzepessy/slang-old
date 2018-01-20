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
        let mut col = 1;

        while let Some(c) = self.chars.next() {
            match c {
                // Found something that starts with a number
                '0'...'9' => {
                    // Create vector to store individual digits
                    let mut digits = Vec::new();
                    digits.push(c);

                    // Check if next digit is a number
                    while let Some(&n) = self.chars.peek() {
                        match n {
                            '0'...'9' => {
                                digits.push(n);
                                self.chars.next();
                            },

                            _ => {
                                break;
                            },
                        }
                    }

                    // Convert string into number
                    let result: String = digits.into_iter().collect();

                    if let Ok(num) = result.parse::<i64>() {
                        return Some(Token::Int(num));
                    }
                    else {
                        return Some(Token::MalformedNumber(row, col));
                    }
                },

                '\n' => {
                    row += 1;
                    col = 0;
                },

                _ => {

                }
            }

            col += 1;
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_int() {
        let input = "123";
        let mut it = TokenIterator::new(input.chars().peekable());

        let token = it.next_token();
        assert_eq!(token, Some(Token::Int(123)));
    }
}
