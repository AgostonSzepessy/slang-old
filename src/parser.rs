use token::Token;
use tokenizer::TokenIterator;

use std::mem;
use std::fmt;
use std::iter::Peekable;

use prev_iter::PrevPeekable;

/// A vector meant to store tokens and display them.
#[derive(Debug, PartialEq)]
pub struct TokenVec(pub Vec<Token>);

impl fmt::Display for TokenVec {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // Convert tokens into a comma separated list
        let output = self.0[..].iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", ");
        write!(f, "{}", output)
    }
}

/// `Expr` represents the possible expressions that can be created. There are
/// 4 different kinds:
/// * `Binary`: These are expressions that have 2 operands, such as `5 + 3`.
/// * `Unary`: These are expressions that have an operator prefixed to them,
/// such as `-3` or `!condition`.
/// * `Primary`: This is a primitive value, such as a number, string, boolean,
/// or `none`.
/// * `Grouping`: This represents an expression that is within parentheses,
/// such as `(5 + 3)`.
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
    #[fail(display = "Error: expected {}", _0)]
    ExpectedToken(Token),
    #[fail(display = "Error: unexpected expression {}. Expected one of: {}", _0, _1)]
    UnexpectedExpressionAlts(Token, TokenVec),
    #[fail(display = "Error: unexpected expression {}", _0)]
    UnexpectedExpression(Token),
    #[fail(display = "Error: expected {}, found {}", _0, _1)]
    UnexpectedToken(Token, Token),
    #[fail(display = "Error: expected an expression after {}", _0)]
    NoExpression(Token),
    #[fail(display = "Error: expected an expression but end of file was reached")]
    Eof,
}

/// Creates an abstract syntax tree from the `Token`s supplied to it.
pub struct Parser<'a> {
    tokens: PrevPeekable<TokenIterator<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(mut tokens: PrevPeekable<TokenIterator<'a>>) -> Self {
        Parser {
            tokens: tokens,
        }
    }

    pub fn parse(&mut self) -> Result<Expr, ParseError> {
        self.expression()
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.equality()
    }
    
    fn equality(&mut self) -> Result<Expr, ParseError> {
        let expr = self.comparison()?;

        if let Some(operator) = self.match_token(&[Token::BangEq(0, 0), Token::EqEq(0, 0)]) {
            let right = self.comparison()?;
            return Ok(Expr::Binary(Box::new(expr), operator, Box::new(right)));
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let expr = self.addition()?;

        if let Some(operator) = self.match_token(&[Token::Greater(0, 0), Token::GreaterEq(0, 0), Token::Less(0, 0), Token::LessEq(0, 0)]) {
            let right = self.addition()?;
            return Ok(Expr::Binary(Box::new(expr), operator, Box::new(right)));
        }

        Ok(expr)
    }

    fn addition(&mut self) -> Result<Expr, ParseError> {
        let expr = self.multiplication()?;

        if let Some(operator) = self.match_token(&[Token::Plus(0, 0), Token::Minus(0, 0)]) {
            let right = self.multiplication()?;
            return Ok(Expr::Binary(Box::new(expr), operator, Box::new(right)));
        }

        Ok(expr)
    }

    fn multiplication(&mut self) -> Result<Expr, ParseError> {
        let expr = self.unary()?;

        if let Some(operator) = self.match_token(&[Token::Star(0, 0), Token::Slash(0, 0), Token::Percent(0, 0)]) {
            let right = self.unary()?;
            return Ok(Expr::Binary(Box::new(expr), operator, Box::new(right)));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if let Some(operator) = self.match_token(&[Token::Minus(0, 0), Token::Bang(0, 0)]) {
            let expr = self.primary()?;
            return Ok(Expr::Unary(operator, Box::new(expr)));
        }

        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        // Found a primary token
        if let Some(token) = self.match_token(&[Token::Float(0f64, 0, 0), Token::False(0, 0), Token::True(0, 0), Token::None(0, 0)]) {
            return Ok(Expr::Primary(token));
        }

        // Found the start of a grouping
        if let Some(_) = self.match_token(&[Token::LeftParen(0, 0)]) {
            let expr = self.expression()?;
            self.consume(Token::RightParen(0, 0))?;
            return Ok(Expr::Grouping(Box::new(expr)));
        }

        if let Some(prev) = self.tokens.prev() {
            return Err(ParseError::NoExpression(prev));
        }

        if let Some(t) = self.tokens.next() {
            return Err(ParseError::UnexpectedExpression(t));
        }

        Err(ParseError::Eof)
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

        // Found a different token than the one we expected
        if let Some(t) = self.tokens.peek() {
            return Err(ParseError::UnexpectedToken(token, t.clone()));
        } 

        // Couldn't find expected token
        Err(ParseError::ExpectedToken(token))
    }

    fn synchronize(&mut self) {
        self.tokens.next();

        while let Some(_) = self.tokens.peek() {
            if let Some(&Token::Semicolon(..)) = self.tokens.prev_peek() {
                return;
            }

            use token::Token::*;
            match *self.tokens.peek().unwrap() {
                And(..)
                | Clone(..)
                | Const(..)
                | If(..)
                | Else(..)
                | For(..)
                | Fn(..)
                | Let(..)
                | Ret(..)
                | Slf(..)
                | While(..) => return,

                _ => { self.tokens.next(); continue },
            }
        }
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
                let token_vec = vec![Token::Float(1f64, 0, 0), $token, Token::Float(1f64, 0, 0)];
                let tokens = PrevPeekable::new(TokenIterator::from(&token_vec));
                let mut parser = Parser::new(tokens);
                assert_eq!(parser.expression(), Ok(Expr::Binary(Box::new(Expr::Primary(Token::Float(1f64, 0, 0))), $token, Box::new(Expr::Primary(Token::Float(1f64, 0, 0))))));
            }
        }
    }

    macro_rules! primary_test {
        ($name: ident, $token: expr) => {
            #[test]
            fn $name() {
                let token_vec = vec![$token];
                let tokens = PrevPeekable::new(TokenIterator::from(&token_vec));
                let mut parser = Parser::new(tokens);
                assert_eq!(parser.expression(), Ok(Expr::Primary($token)));
            }
        }
    }

    // Test binary expressions such as 5 + 3
    binary_test!(test_bang_eq, Token::BangEq(0, 0));
    binary_test!(test_eqeq, Token::EqEq(0, 0));
    binary_test!(test_greater, Token::Greater(0, 0));
    binary_test!(test_greater_eq, Token::GreaterEq(0, 0));
    binary_test!(test_less, Token::Less(0, 0));
    binary_test!(test_less_eq, Token::LessEq(0, 0));
    binary_test!(test_binary_addition, Token::Plus(0, 0));
    binary_test!(test_binary_minus, Token::Minus(0, 0));
    binary_test!(test_multiplication, Token::Star(0, 0));
    binary_test!(test_division, Token::Slash(0, 0));
    binary_test!(test_modulus, Token::Percent(0, 0));

    // Test primary expressions such as 5 and false
    primary_test!(test_number, Token::Float(5f64, 0, 0));
    primary_test!(test_false, Token::False(0, 0));
    primary_test!(test_true, Token::True(0, 0));
    primary_test!(test_none, Token::None(0, 0));

    #[test]
    fn test_unary_minus() {
        let token_vec = vec![Token::Minus(0, 0), Token::Float(1f64, 0, 0)];
        let tokens = PrevPeekable::new(TokenIterator::from(&token_vec));
        let mut parser = Parser::new(tokens);
        assert_eq!(parser.expression(), Ok(Expr::Unary(Token::Minus(0, 0), Box::new(Expr::Primary(Token::Float(1f64, 0, 0))))));
    }

    #[test]
    fn test_unary_bang() {
        let token_vec = vec![Token::Bang(0, 0), Token::Float(1f64, 0, 0)];
        let tokens = PrevPeekable::new(TokenIterator::from(&token_vec));
        let mut parser = Parser::new(tokens);
        assert_eq!(parser.expression(), Ok(Expr::Unary(Token::Bang(0, 0), Box::new(Expr::Primary(Token::Float(1f64, 0, 0))))));
    }

    #[test]
    fn test_fail_unary_plus() {
        let token_vec = vec![Token::Plus(0, 0), Token::Float(1f64, 0, 0)];
        let tokens = PrevPeekable::new(TokenIterator::from(&token_vec));
        let mut parser = Parser::new(tokens);
        assert_eq!(parser.expression(), Err(ParseError::UnexpectedExpression(Token::Plus(0, 0))));
    }

}
