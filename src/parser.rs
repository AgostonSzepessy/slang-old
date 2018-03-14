use token::Token;
use tokenizer::TokenIterator;

use std::mem;
use std::fmt;
use std::iter::Peekable;

#[derive(Debug, PartialEq)]
pub struct TokenVec(pub Vec<Token>);

impl fmt::Display for TokenVec {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Convert tokens into a comma separated list
        let output = self.0[..].iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", ");
        write!(f, "{}", output)
    }
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    /// Expressions that have 2 operands
    Binary(Box<Expr>, Token, Box<Expr>),
    /// Expressions that have 1 operand
    Unary(Token, Box<Expr>),
    /// Expressions that only contain a number or string
    Primary(Token),
    /// Expressions that are within parentheses
    Grouping(Box<Expr>),
}

#[derive(Debug, PartialEq, Fail)]
pub enum ParseError {
    #[fail(display = "Error: expected {}, found end of file", _0)]
    UnexpectedEndOfFile(TokenVec),
    #[fail(display = "Error: expected {}, found {}", _0, _1)]
    UnexpectedToken(Token, Token),
    #[fail(display = "Error: expected one of: {}", _0)]
    UnmatchedTokens(TokenVec),
    #[fail(display = "Error: expected an expression")]
    NoExpression,
}

pub struct Parser<'a> {
    tokens: Peekable<TokenIterator<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Peekable<TokenIterator<'a>>) -> Self {
        Parser {
            tokens: tokens,
        }
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.equality()
    }
    
    fn equality(&mut self) -> Result<Expr, ParseError> {
        let expr = self.comparison()?;

        if let Some(operator) = self.match_token(&[Token::BangEq, Token::EqEq]) {
            let right = self.comparison()?;
            return Ok(Expr::Binary(Box::new(expr), operator, Box::new(right)));
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let expr = self.addition()?;

        if let Some(operator) = self.match_token(&[Token::Greater, Token::GreaterEq, Token::Less, Token::LessEq]) {
            let right = self.addition()?;
            return Ok(Expr::Binary(Box::new(expr), operator, Box::new(right)));
        }

        Ok(expr)
    }

    fn addition(&mut self) -> Result<Expr, ParseError> {
        let expr = self.multiplication()?;

        if let Some(operator) = self.match_token(&[Token::Plus, Token::Minus]) {
            let right = self.multiplication()?;
            return Ok(Expr::Binary(Box::new(expr), operator, Box::new(right)));
        }

        Ok(expr)
    }

    fn multiplication(&mut self) -> Result<Expr, ParseError> {
        let expr = self.unary()?;

        if let Some(operator) = self.match_token(&[Token::Star, Token::Slash, Token::Percent]) {
            let right = self.unary()?;
            return Ok(Expr::Binary(Box::new(expr), operator, Box::new(right)));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if let Some(operator) = self.match_token(&[Token::Minus, Token::Bang]) {
            let expr = self.primary()?;
            return Ok(Expr::Unary(operator, Box::new(expr)));
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        // Found a primary token
        if let Some(token) = self.match_token(&[Token::Int(0), Token::Float(0f64), Token::False, Token::True, Token::None]) {
            return Ok(Expr::Primary(token));
        }

        // Found the start of a grouping
        if let Some(_) = self.match_token(&[Token::LeftParen]) {
            let expr = self.expression()?;
            self.consume(Token::RightParen)?;
            return Ok(Expr::Grouping(Box::new(expr)));
        }

        // TODO: Rename this to UnexpectedExpression
        Err(ParseError::NoExpression)
    }

    /// Checks if the current token matches one of the specified tokens
    /// Parameters: `tokens`: slice of tokens to check
    fn match_token(&mut self, tokens: &[Token]) -> Option<Token> {
        let mut found_token = false;
        for token in tokens.iter() {
            // Make sure we haven't reached the end of the tokens
            if let Some(t) = self.tokens.peek() {
                if mem::discriminant(token) == mem::discriminant(t) {
                    found_token = true;
                    break;
                }
            } else {
                // Reached end of file; no more tokens available
                return None;
            }
        }

        if found_token {
            return self.tokens.next();
        }

        // Didn't find any tokens
        None
    }

    /// Checks whether the current token is equal to the specified token. If it is, the iterator
    /// advances to the next token. Otherwise, it returns an error.
    /// Parameters: `token`: `Token` to check for.
    fn consume(&mut self, token: Token) -> Result<(), ParseError> {
        if let Some(_) = self.match_token(&[(token.clone())]) {
            self.tokens.next();
            return Ok(());
        }

        Err(ParseError::UnexpectedToken(token, self.tokens.peek().unwrap().clone()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use token::Token;
    use tokenizer::TokenIterator;

    macro_rules! binary_test {
        ($name: ident, $token: expr) => {
            #[test]
            fn $name() {
                let token_vec = vec![Token::Int(1), $token, Token::Int(1)];
                let tokens = TokenIterator::from(&token_vec).peekable();
                let mut parser = Parser::new(tokens);
                assert_eq!(parser.expression(), Ok(Expr::Binary(Box::new(Expr::Primary(Token::Int(1))), $token, Box::new(Expr::Primary(Token::Int(1))))));
            }
        }
    }

    macro_rules! primary_test {
        ($name: ident, $token: expr) => {
            #[test]
            fn $name() {
                let token_vec = vec![$token];
                let tokens = TokenIterator::from(&token_vec).peekable();
                let mut parser = Parser::new(tokens);
                assert_eq!(parser.expression(), Ok(Expr::Primary($token)));
            }
        }
    }

    // Test binary expressions such as 5 + 3
    binary_test!(test_bang_eq, Token::BangEq);
    binary_test!(test_eqeq, Token::EqEq);
    binary_test!(test_greater, Token::Greater);
    binary_test!(test_greater_eq, Token::GreaterEq);
    binary_test!(test_less, Token::Less);
    binary_test!(test_less_eq, Token::LessEq);
    binary_test!(test_binary_addition, Token::Plus);
    binary_test!(test_binary_minus, Token::Minus);
    binary_test!(test_multiplication, Token::Star);
    binary_test!(test_division, Token::Slash);
    binary_test!(test_modulus, Token::Percent);

    // Test primary expressions such as 5 and false
    primary_test!(test_number, Token::Int(5));
    primary_test!(test_false, Token::False);
    primary_test!(test_true, Token::True);
    primary_test!(test_none, Token::None);

    #[test]
    fn test_unary_minus() {
        let token_vec = vec![Token::Minus, Token::Int(1)];
        let tokens = TokenIterator::from(&token_vec).peekable();
        let mut parser = Parser::new(tokens);
        assert_eq!(parser.expression(), Ok(Expr::Unary(Token::Minus, Box::new(Expr::Primary(Token::Int(1))))));
    }

    #[test]
    fn test_unary_bang() {
        let token_vec = vec![Token::Bang, Token::Int(1)];
        let tokens = TokenIterator::from(&token_vec).peekable();
        let mut parser = Parser::new(tokens);
        assert_eq!(parser.expression(), Ok(Expr::Unary(Token::Bang, Box::new(Expr::Primary(Token::Int(1))))));
    }

    #[test]
    fn test_fail_unary_plus() {
        let token_vec = vec![Token::Plus, Token::Int(1)];
        let tokens = TokenIterator::from(&token_vec).peekable();
        let mut parser = Parser::new(tokens);
        assert_eq!(parser.expression(), Err(ParseError::NoExpression));
    }

}
