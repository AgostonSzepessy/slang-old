use parser::Expr;
use token::Token;

use std::ops::{Add, Div, Mul, Sub};

#[derive(Debug, PartialEq, Fail)]
enum InterpreterError {
    #[fail(display = "Type error")]
    TypeError,
}

#[derive(Debug, PartialEq, PartialOrd)]
enum ExprVal {
    Num(f64),
    Bool(bool),
    String(String),
}

/// Macro that implements basic arithmetic operations
/// for ExprVal. It returns a Result<ExprVal, InterpreterError>
/// based on whether the types match or not.
macro_rules! impl_arithmetic {
    ($trait: ident, $fn: ident, $opr: tt) => {
        impl $trait for ExprVal {
            type Output = Result<ExprVal, InterpreterError>;

            fn $fn(self, other: ExprVal) -> Result<ExprVal, InterpreterError> {
                match (self, other) {
                    (ExprVal::Num(n1), ExprVal::Num(n2)) => Ok(ExprVal::Num(n1 $opr n2)),
                    _ => Err(InterpreterError::TypeError),
                }
            }
        }
    }
}

impl_arithmetic!(Add, add, +);
impl_arithmetic!(Mul, mul, *);
impl_arithmetic!(Sub, sub, -);
impl_arithmetic!(Div, div, /);


impl ExprVal {
    fn is_bool(&self) -> bool {
        match *self {
            ExprVal::Bool(..) => true,
            _ => false,
        }
    }

    fn get_bool(self) -> Result<bool, InterpreterError> {
        match self {
            ExprVal::Bool(b) => return Ok(b),
            _ => Err(InterpreterError::TypeError),
        }
    }

    fn is_float(&self) -> bool {
        match *self {
            ExprVal::Num(..) => true,
            _ => false,
        }
    }

    fn get_float(self) -> Result<f64, InterpreterError> {
        match self {
            ExprVal::Num(f) => return Ok(f),
            _ => Err(InterpreterError::TypeError),
        }
    }

    fn is_string(&self) -> bool {
        match *self {
            ExprVal::String(..) => true,
            _ => false,
        }
    }

    fn get_string(self) -> Result<String, InterpreterError> {
        match self {
            ExprVal::String(s) => return Ok(s),
            _ => Err(InterpreterError::TypeError),
        }
    }
}

#[derive(Debug, PartialEq)]
struct Interpreter {
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
        }
    }

    pub fn interpret(&self, expr: Expr) -> Result<ExprVal, InterpreterError> {
        match expr {
            Expr::Primary(ref token) => Ok(self.literal(&token)),
            Expr::Grouping(e) => Ok(self.interpret(*e)?),
            Expr::Unary(token, ex) => {
                let val = self.interpret(*ex)?;

                match token {
                    Token::Bang(..) => Ok(ExprVal::Bool(!val.get_bool()?)),
                    Token::Minus(..) => {
                        return match val {
                            ExprVal::Num(f) => Ok(ExprVal::Num(-f)),
                            _ => Err(InterpreterError::TypeError),
                        };
                    },
                    _ => unreachable!(),
                }
            },
            Expr::Binary(left_side, operator, right_side) => {
                let left_val = self.interpret(*left_side)?;
                let right_val = self.interpret(*right_side)?;

                /// Macro that generates a match statement for boolean expressions.
                macro_rules! bool_impl {
                    ($left: expr, $opr: tt, $right: expr) => {{
                        match ($left, $right) {
                            (ExprVal::Bool(b1), ExprVal::Bool(b2)) => return Ok(ExprVal::Bool(b1 $opr b2)),
                            (ExprVal::Num(n1), ExprVal::Num(n2)) => return Ok(ExprVal::Bool(n1 $opr n2)),
                            (ExprVal::String(s1), ExprVal::String(s2)) => return Ok(ExprVal::Bool(s1 $opr s2)),
                            _ => return Err(InterpreterError::TypeError),
                        }
                    }}
                }

                match operator {
                    Token::Plus(..) => left_val + right_val,
                    Token::Minus(..) => left_val - right_val,
                    Token::Star(..) => left_val * right_val,
                    Token::Slash(..) => left_val / right_val,
                    Token::EqEq(..) => bool_impl!(left_val, ==, right_val),
                    Token::BangEq(..) => bool_impl!(left_val, !=, right_val),
                    Token::Greater(..) => bool_impl!(left_val, >, right_val),
                    Token::GreaterEq(..) => bool_impl!(left_val, >=, right_val),
                    Token::Less(..) => bool_impl!(left_val, <, right_val),
                    Token::LessEq(..) => bool_impl!(left_val, <=, right_val),

                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
    }

    fn literal(&self, token: &Token) -> ExprVal {
        use token::Token::*;
        match token {
            &Num(f, ..) => ExprVal::Num(f),
            &False(..) => ExprVal::Bool(false),
            &String(ref s, ..) => ExprVal::String(s.clone()),
            &True(..) => ExprVal::Bool(true),
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! gen_test {
        ($name: ident, $input: expr, $expect: expr) => {
            #[test]
            fn $name() {
                let interpreter = Interpreter::new();
                let result = interpreter.interpret($input);

                assert_eq!(result, Ok($expect));
            }
        }
    }

    gen_test!(test_float_primary, Expr::Primary(Token::Num(1.0, 0, 0)), ExprVal::Num(1.0));
    gen_test!(test_true_primary, Expr::Primary(Token::True(1, 1)), ExprVal::Bool(true));
    gen_test!(test_false_primary, Expr::Primary(Token::False(1, 1)), ExprVal::Bool(false));
    gen_test!(test_string_primary, Expr::Primary(Token::String("slang".to_string(), 1, 1)), ExprVal::String("slang".to_string()));

    macro_rules! gen_binary_test {
        ($name: ident, $opr: expr, $expect: expr) => {
            #[test]
            fn $name() {
                let interpreter = Interpreter::new();
                let result = interpreter.interpret(Expr::Binary(Box::new(Expr::Primary(Token::Num(1f64, 0, 0))), $opr, Expr::Binary(Box::new(Expr::Primary(Token::Num(1f64, 0, 0))))));

                assert_eq!(result, ExprVal::Num($expect));
            }
        }
    }
    
    gen_test!(test_add_binary, Expr::Binary(Box::new(Expr::Primary(Token::Num(1f64, 0, 0))), Token::Plus(0, 0), Box::new(Expr::Primary(Token::Num(1f64, 0, 0)))), ExprVal::Num(2f64));
    gen_binary_test!(test_eqeq, Token::EqEq(0, 0), 1f64);
}
