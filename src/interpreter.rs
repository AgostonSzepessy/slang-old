use parser::{Expr, ParseToken, VarInfo, Stmt};

use std::ops::{Add, Div, Mul, Rem, Sub};
use std::collections::HashMap;

#[derive(Debug, PartialEq, Fail)]
pub enum InterpreterError {
    #[fail(display = "Type error")]
    TypeError,
    #[fail(display = "Undefined variable {}", _0)]
    UndefinedVar(String),
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum ExprVal {
    Int(i64),
    Float(f64),
    Bool(bool),
    None,
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
    fn get_bool(self) -> Result<bool, InterpreterError> {
        match self {
            ExprVal::Bool(b) => Ok(b),
            _ => Err(InterpreterError::TypeError),
        }
    }
}

/// Environment in which variables are stored. Each lexical scope gets its own
/// environment.
#[derive(Debug, PartialEq)]
struct Env {
    /// The vaariables stored in this environment
    values: HashMap<String, ExprVal>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            values: HashMap::new(),
        }
    }

    /// Adds a new variable to the environment. Variables can be redefined.
    pub fn define(&mut self, name: String, value: ExprVal) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Result<ExprVal, InterpreterError> {
        match self.values.get(name) {
            Some(v) => Ok(v.to_owned()),
            None => return Err(InterpreterError::UndefinedVar(name.to_string())),
        }
    }
}

/// Executes the list of statements that are passed to it. If a runtime error happens,
/// it returns an `InterpreterError`.
#[derive(Debug, PartialEq)]
pub struct Interpreter {
    env: Env,
}

impl Interpreter {
    /// Creates a new interpreter
    pub fn new() -> Self {
        Interpreter {
            env: Env::new(),
        }
    }

    /// Executes the list of `Stmt`s that are passed to it. If something goes wrong while executing
    /// them it will return an `InterpreterError`.
    pub fn interpret(&mut self, stmts: Vec<Stmt>) -> Result<(), InterpreterError> {
        for stmt in stmts {
            match stmt {
                Stmt::Print(e) => self.print_stmt(e)?,
                Stmt::Variable(parse_token, expr) => self.variable_stmt(parse_token, expr)?,
                _ => unreachable!(),
            };
        }

        Ok(())
    }

    /// Handles a print statement; it evaluates the arguments within the print statement and then
    /// displays them on the screen.
    fn print_stmt(&self, expr: Expr) -> Result<(), InterpreterError> {
        let val = self.interpret_expr(expr)?;
        println!("{}", self.stringify(&val));
        Ok(())
    }

    /// Defines a new variable and initializes it with an expression if there is one, or assigns it
    /// a value of `none`.
    fn variable_stmt(&mut self, var_info: VarInfo, expr: Option<Expr>) -> Result<(), InterpreterError> {
        let val = match expr {
            Some(e) => self.interpret_expr(e)?,
            None => ExprVal::None,
        };

        self.env.define(var_info.name, val);
        Ok(())
    }

    /// Interprets an expression such as `5 + 3`, or `-3`. Expressions can include arithmetic and
    /// boolean expressions.
    pub fn interpret_expr(&self, expr: Expr) -> Result<ExprVal, InterpreterError> {
        match expr {
            Expr::Primary(ref token) => Ok(self.literal(&token)),
            Expr::Grouping(e) => Ok(self.interpret_expr(*e)?),
            Expr::Unary(token, ex) => {
                let val = self.interpret_expr(*ex)?;

                match token {
                    ParseToken::Bang(..) => Ok(ExprVal::Bool(!val.get_bool()?)),
                    ParseToken::Minus(..) => {
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
                let left_val = self.interpret_expr(*left_side)?;
                let right_val = self.interpret_expr(*right_side)?;

                /// Macro that generates a match statement for boolean expressions.
                macro_rules! bool_impl {
                    ($left: expr, $opr: tt, $right: expr) => {{
                        match ($left, $right) {
                            (ExprVal::Bool(b1), ExprVal::Bool(b2)) => Ok(ExprVal::Bool(b1 $opr b2)),
                            (ExprVal::Float(n1), ExprVal::Float(n2)) => Ok(ExprVal::Bool(n1 $opr n2)),
                            (ExprVal::Int(i1), ExprVal::Int(i2)) => Ok(ExprVal::Bool(i1 $opr i2)),
                            (ExprVal::String(s1), ExprVal::String(s2)) => Ok(ExprVal::Bool(s1 $opr s2)),
                            _ => Err(InterpreterError::TypeError),
                        }
                    }}
                }

                match operator {
                    ParseToken::Plus(..) => left_val + right_val,
                    ParseToken::Minus(..) => left_val - right_val,
                    ParseToken::Star(..) => left_val * right_val,
                    ParseToken::Percent(..) => left_val % right_val,
                    ParseToken::Slash(..) => left_val / right_val,
                    ParseToken::EqEq(..) => {
                        let val = bool_impl!(left_val, ==, right_val);
                        if let Err(_) = val {
                            return Ok(ExprVal::Bool(false));
                        }

                        return val;
                    },
                    ParseToken::BangEq(..) => {
                        let val = bool_impl!(left_val, !=, right_val);
                        if let Err(_) = val {
                            return Ok(ExprVal::Bool(true));
                        }

                        return val;
                    },
                    ParseToken::Greater(..) => bool_impl!(left_val, >, right_val),
                    ParseToken::GreaterEq(..) => bool_impl!(left_val, >=, right_val),
                    ParseToken::Less(..) => bool_impl!(left_val, <, right_val),
                    ParseToken::LessEq(..) => bool_impl!(left_val, <=, right_val),

                    _ => panic!(),
                }
            },

            Expr::Var(var_info) => Ok(self.env.get(&var_info.name)?),
        }
    }

    /// Base case for expressions; eventually everything boils down to a literal.
    fn literal(&self, token: &ParseToken) -> ExprVal {
        use parser::ParseToken::*;
        match token {
            &Int(i, ..) => ExprVal::Int(i),
            &Float(f, ..) => ExprVal::Float(f),
            &False(..) => ExprVal::Bool(false),
            None(..) => ExprVal::None,
            &String(ref s, ..) => ExprVal::String(s.clone()),
            &True(..) => ExprVal::Bool(true),
            _ => unreachable!(),
        }
    }

    /// Converts the `ExprVal` to a `String`.
    fn stringify(&self, val: &ExprVal) -> String {
        match val {
            &ExprVal::Int(i) => i.to_string(),
            &ExprVal::Bool(b) => {
                if b { "true".to_string() } else { "false".to_string() }
            },
            &ExprVal::Float(f, ..) => f.to_string(),
            &ExprVal::None => "none".to_string(),
            &ExprVal::String(ref s, ..) => s.clone(),
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
                let result = interpreter.interpret_expr($input);

                assert_eq!(result, Ok($expect));
            }
        }
    }

    gen_test!(test_int_primary, Expr::Primary(ParseToken::Int(1, 0, 0)), ExprVal::Int(1));
    gen_test!(test_float_primary, Expr::Primary(ParseToken::Float(1.0, 0, 0)), ExprVal::Float(1.0));
    gen_test!(test_true_primary, Expr::Primary(ParseToken::True(1, 1)), ExprVal::Bool(true));
    gen_test!(test_false_primary, Expr::Primary(ParseToken::False(1, 1)), ExprVal::Bool(false));
    gen_test!(test_string_primary, Expr::Primary(ParseToken::String("slang".to_string(), 1, 1)), ExprVal::String("slang".to_string()));

    macro_rules! gen_binary_test {
        ($name: ident, $left: expr, $opr: expr, $right: expr, $expect: expr) => {
            #[test]
            fn $name() {
                let interpreter = Interpreter::new();
                let result = interpreter.interpret_expr(Expr::Binary(Box::new(Expr::Primary($left)), $opr, Box::new(Expr::Primary($right))));

                assert_eq!(result, Ok($expect));
            }
        }
    }

    gen_binary_test!(test_float_eqeq_false, ParseToken::Float(1f64, 0, 0), ParseToken::EqEq(0, 0), ParseToken::Float(2f64, 0, 0), ExprVal::Bool(false));
    gen_binary_test!(test_float_eqeq_true, ParseToken::Float(1f64, 0, 0), ParseToken::EqEq(0, 0), ParseToken::Float(1f64, 0, 0), ExprVal::Bool(true));
    gen_binary_test!(test_float_less_false, ParseToken::Float(2f64, 0, 0), ParseToken::Less(0, 0), ParseToken::Float(1f64, 0, 0), ExprVal::Bool(false));
    gen_binary_test!(test_float_less_true, ParseToken::Float(1f64, 0, 0), ParseToken::Less(0, 0), ParseToken::Float(2f64, 0, 0), ExprVal::Bool(true));
    gen_binary_test!(test_float_less_eq_false, ParseToken::Float(2f64, 0, 0), ParseToken::LessEq(0, 0), ParseToken::Float(1f64, 0, 0), ExprVal::Bool(false));
    gen_binary_test!(test_float_less_eq_true, ParseToken::Float(1f64, 0, 0), ParseToken::LessEq(0, 0), ParseToken::Float(1f64, 0, 0), ExprVal::Bool(true));
    gen_binary_test!(test_float_add, ParseToken::Float(1f64, 0, 0), ParseToken::Plus(0, 0), ParseToken::Float(1f64, 0, 0), ExprVal::Float(2f64));
    gen_binary_test!(test_float_sub, ParseToken::Float(2f64, 0, 0), ParseToken::Minus(0, 0), ParseToken::Float(1f64, 0, 0), ExprVal::Float(1f64));
    gen_binary_test!(test_float_div, ParseToken::Float(4f64, 0, 0), ParseToken::Slash(0, 0), ParseToken::Float(2f64, 0, 0), ExprVal::Float(2f64));
    gen_binary_test!(test_float_mul, ParseToken::Float(2f64, 0, 0), ParseToken::Star(0, 0), ParseToken::Float(2f64, 0, 0), ExprVal::Float(4f64));
    gen_binary_test!(test_float_rem, ParseToken::Float(5f64, 0, 0), ParseToken::Percent(0, 0), ParseToken::Float(2.4f64, 0, 0), ExprVal::Float(0.20000000000000018f64));

    gen_binary_test!(test_int_eqeq_false, ParseToken::Int(1, 0, 0), ParseToken::EqEq(0, 0), ParseToken::Int(2, 0, 0), ExprVal::Bool(false));
    gen_binary_test!(test_int_eqeq_true, ParseToken::Int(1, 0, 0), ParseToken::EqEq(0, 0), ParseToken::Int(1, 0, 0), ExprVal::Bool(true));
    gen_binary_test!(test_int_less_false, ParseToken::Int(2, 0, 0), ParseToken::Less(0, 0), ParseToken::Int(1, 0, 0), ExprVal::Bool(false));
    gen_binary_test!(test_int_less_true, ParseToken::Int(1, 0, 0), ParseToken::Less(0, 0), ParseToken::Int(2, 0, 0), ExprVal::Bool(true));
    gen_binary_test!(test_int_less_eq_false, ParseToken::Int(2, 0, 0), ParseToken::LessEq(0, 0), ParseToken::Int(1, 0, 0), ExprVal::Bool(false));
    gen_binary_test!(test_int_less_eq_true, ParseToken::Int(1, 0, 0), ParseToken::LessEq(0, 0), ParseToken::Int(1, 0, 0), ExprVal::Bool(true));
    gen_binary_test!(test_int_add, ParseToken::Int(1, 0, 0), ParseToken::Plus(0, 0), ParseToken::Int(1, 0, 0), ExprVal::Int(2));
    gen_binary_test!(test_int_sub, ParseToken::Int(2, 0, 0), ParseToken::Minus(0, 0), ParseToken::Int(1, 0, 0), ExprVal::Int(1));
    gen_binary_test!(test_int_div, ParseToken::Int(4, 0, 0), ParseToken::Slash(0, 0), ParseToken::Int(2, 0, 0), ExprVal::Int(2));
    gen_binary_test!(test_int_mul, ParseToken::Int(2, 0, 0), ParseToken::Star(0, 0), ParseToken::Int(2, 0, 0), ExprVal::Int(4));
    gen_binary_test!(test_int_rem, ParseToken::Int(5, 0, 0), ParseToken::Percent(0, 0), ParseToken::Int(2, 0, 0), ExprVal::Int(1));

    gen_binary_test!(test_eqeq_float_int, ParseToken::Int(1, 0, 0), ParseToken::EqEq(0, 0), ParseToken::Float(1.0, 0, 0), ExprVal::Bool(false));
    gen_binary_test!(test_neeq_float_int, ParseToken::Int(1, 0, 0), ParseToken::BangEq(0, 0), ParseToken::Float(1.0, 0, 0), ExprVal::Bool(true));

    #[test]
    fn test_var_decl_with_val() {
        let var_info = VarInfo::new("var".to_string(), 0, 0);
        let expr = Some(Expr::Primary(ParseToken::Int(5, 0, 0)));
        let mut interpreter = Interpreter::new();
        assert_eq!(interpreter.variable_stmt(var_info, expr), Ok(()));
    }

    #[test]
    fn test_var_decl() {
        let var_info = VarInfo::new("var".to_string(), 0, 0);
        let expr = None;
        let mut interpreter = Interpreter::new();
        assert_eq!(interpreter.variable_stmt(var_info, expr), Ok(()));

    }

    #[test]
    fn test_get_var() {
        let var_info = VarInfo::new("var".to_string(), 0, 0);
        let val = ExprVal::Int(5);
        let mut interpreter = Interpreter::new();
        interpreter.env.define("var".to_string(), val);
        let expr = Expr::Var(var_info);

        assert_eq!(interpreter.interpret_expr(expr), Ok(ExprVal::Int(5)));
    }
}
