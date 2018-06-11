use parser::Expr;
use token::Token;

#[derive(Debug, PartialEq, Fail)]
enum InterpreterError {
    #[fail(display = "Type error")]
    TypeError,
}

#[derive(Debug, PartialEq)]
enum ExprVal {
    Float(f64),
    Bool(bool),
    String(String),
}

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
                            ExprVal::Float(f) => Ok(ExprVal::Float(-f)),
                            _ => Err(InterpreterError::TypeError),
                        };
                    },
                    _ => unreachable!(),
                }
            },
            Expr::Binary(left_side, operator, right_side) => {
                let left_val = self.interpret(*left_side);
                let right_val = self.interpret(*right_side);

                match operator {
                    Token::Plus(..) => {
                        macro_rules! bin_op {
                            ($left: expr, $right: expr, $opr: tt) => {{

                            }}
                        }
                        panic!();
                    },
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
    }

    fn literal(&self, token: &Token) -> ExprVal {
        use token::Token::*;
        match token {
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

    macro_rules! primary_test {
        ($name: ident, $input: expr, $expect: expr) => {
            #[test]
            fn $name() {
                let interpreter = Interpreter::new();
                let result = interpreter.interpret($input);

                assert_eq!(result, Ok($expect));
            }
        }
    }

    primary_test!(test_float_primary, Expr::Primary(Token::Float(1.0, 0, 0)), ExprVal::Float(1.0));
    primary_test!(test_true_primary, Expr::Primary(Token::True(1, 1)), ExprVal::Bool(true));
    primary_test!(test_false_primary, Expr::Primary(Token::False(1, 1)), ExprVal::Bool(false));
    primary_test!(test_string_primary, Expr::Primary(Token::String("slang".to_string(), 1, 1)), ExprVal::String("slang".to_string()));
}
