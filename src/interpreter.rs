use parser::Expr;
use token::Token;

use std::ops::{Add, Div, Mul, Rem, Sub};

#[derive(Debug, PartialEq, Fail)]
enum InterpreterError {
    #[fail(display = "Type error")]
    TypeError,
}

#[derive(Debug, PartialEq, PartialOrd)]
enum ExprVal {
    Int(i64),
    Float(f64),
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
                    (ExprVal::Int(i1), ExprVal::Int(i2)) => Ok(ExprVal::Int(i1 $opr i2)),
                    (ExprVal::Float(n1), ExprVal::Float(n2)) => Ok(ExprVal::Float(n1 $opr n2)),
                    _ => Err(InterpreterError::TypeError),
                }
            }
        }
    }
}

impl_arithmetic!(Add, add, +);
impl_arithmetic!(Mul, mul, *);
impl_arithmetic!(Sub, sub, -);
impl_arithmetic!(Rem, rem, %);
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
            ExprVal::Float(..) => true,
            _ => false,
        }
    }

    fn get_float(self) -> Result<f64, InterpreterError> {
        match self {
            ExprVal::Float(f) => return Ok(f),
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
                            ExprVal::Int(i) => Ok(ExprVal::Int(-i)),
                            ExprVal::Float(f) => Ok(ExprVal::Float(-f)),
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
                            (ExprVal::Float(n1), ExprVal::Float(n2)) => return Ok(ExprVal::Bool(n1 $opr n2)),
                            (ExprVal::Int(i1), ExprVal::Int(i2)) => return Ok(ExprVal::Bool(i1 $opr i2)),
                            (ExprVal::String(s1), ExprVal::String(s2)) => return Ok(ExprVal::Bool(s1 $opr s2)),
                            _ => return Err(InterpreterError::TypeError),
                        }
                    }}
                }

                match operator {
                    Token::Plus(..) => left_val + right_val,
                    Token::Minus(..) => left_val - right_val,
                    Token::Star(..) => left_val * right_val,
                    Token::Percent(..) => left_val % right_val,
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
            &Int(i, ..) => ExprVal::Int(i),
            &Float(f, ..) => ExprVal::Float(f),
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

    gen_test!(test_int_primary, Expr::Primary(Token::Int(1, 0, 0)), ExprVal::Int(1));
    gen_test!(test_float_primary, Expr::Primary(Token::Float(1.0, 0, 0)), ExprVal::Float(1.0));
    gen_test!(test_true_primary, Expr::Primary(Token::True(1, 1)), ExprVal::Bool(true));
    gen_test!(test_false_primary, Expr::Primary(Token::False(1, 1)), ExprVal::Bool(false));
    gen_test!(test_string_primary, Expr::Primary(Token::String("slang".to_string(), 1, 1)), ExprVal::String("slang".to_string()));

    macro_rules! gen_binary_test {
        ($name: ident, $left: expr, $opr: expr, $right: expr, $expect: expr) => {
            #[test]
            fn $name() {
                let interpreter = Interpreter::new();
                let result = interpreter.interpret(Expr::Binary(Box::new(Expr::Primary($left)), $opr, Box::new(Expr::Primary($right))));

                assert_eq!(result, Ok($expect));
            }
        }
    }

    gen_binary_test!(test_float_eqeq_false, Token::Float(1f64, 0, 0), Token::EqEq(0, 0), Token::Float(2f64, 0, 0), ExprVal::Bool(false));
    gen_binary_test!(test_float_eqeq_true, Token::Float(1f64, 0, 0), Token::EqEq(0, 0), Token::Float(1f64, 0, 0), ExprVal::Bool(true));
    gen_binary_test!(test_float_less_false, Token::Float(2f64, 0, 0), Token::Less(0, 0), Token::Float(1f64, 0, 0), ExprVal::Bool(false));
    gen_binary_test!(test_float_less_true, Token::Float(1f64, 0, 0), Token::Less(0, 0), Token::Float(2f64, 0, 0), ExprVal::Bool(true));
    gen_binary_test!(test_float_less_eq_false, Token::Float(2f64, 0, 0), Token::LessEq(0, 0), Token::Float(1f64, 0, 0), ExprVal::Bool(false));
    gen_binary_test!(test_float_less_eq_true, Token::Float(1f64, 0, 0), Token::LessEq(0, 0), Token::Float(1f64, 0, 0), ExprVal::Bool(true));
    gen_binary_test!(test_float_add, Token::Float(1f64, 0, 0), Token::Plus(0, 0), Token::Float(1f64, 0, 0), ExprVal::Float(2f64));
    gen_binary_test!(test_float_sub, Token::Float(2f64, 0, 0), Token::Minus(0, 0), Token::Float(1f64, 0, 0), ExprVal::Float(1f64));
    gen_binary_test!(test_float_div, Token::Float(4f64, 0, 0), Token::Slash(0, 0), Token::Float(2f64, 0, 0), ExprVal::Float(2f64));
    gen_binary_test!(test_float_mul, Token::Float(2f64, 0, 0), Token::Star(0, 0), Token::Float(2f64, 0, 0), ExprVal::Float(4f64));
    gen_binary_test!(test_float_rem, Token::Float(5f64, 0, 0), Token::Percent(0, 0), Token::Float(2.4f64, 0, 0), ExprVal::Float(0.20000000000000018f64));

    gen_binary_test!(test_int_eqeq_false, Token::Int(1, 0, 0), Token::EqEq(0, 0), Token::Int(2, 0, 0), ExprVal::Bool(false));
    gen_binary_test!(test_int_eqeq_true, Token::Int(1, 0, 0), Token::EqEq(0, 0), Token::Int(1, 0, 0), ExprVal::Bool(true));
    gen_binary_test!(test_int_less_false, Token::Int(2, 0, 0), Token::Less(0, 0), Token::Int(1, 0, 0), ExprVal::Bool(false));
    gen_binary_test!(test_int_less_true, Token::Int(1, 0, 0), Token::Less(0, 0), Token::Int(2, 0, 0), ExprVal::Bool(true));
    gen_binary_test!(test_int_less_eq_false, Token::Int(2, 0, 0), Token::LessEq(0, 0), Token::Int(1, 0, 0), ExprVal::Bool(false));
    gen_binary_test!(test_int_less_eq_true, Token::Int(1, 0, 0), Token::LessEq(0, 0), Token::Int(1, 0, 0), ExprVal::Bool(true));
    gen_binary_test!(test_int_add, Token::Int(1, 0, 0), Token::Plus(0, 0), Token::Int(1, 0, 0), ExprVal::Int(2));
    gen_binary_test!(test_int_sub, Token::Int(2, 0, 0), Token::Minus(0, 0), Token::Int(1, 0, 0), ExprVal::Int(1));
    gen_binary_test!(test_int_div, Token::Int(4, 0, 0), Token::Slash(0, 0), Token::Int(2, 0, 0), ExprVal::Int(2));
    gen_binary_test!(test_int_mul, Token::Int(2, 0, 0), Token::Star(0, 0), Token::Int(2, 0, 0), ExprVal::Int(4));
    gen_binary_test!(test_int_rem, Token::Int(5, 0, 0), Token::Percent(0, 0), Token::Int(2, 0, 0), ExprVal::Int(1));

}
