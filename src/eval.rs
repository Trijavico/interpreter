use anyhow::Result;
use core::fmt;
use std::{borrow::Cow, collections::HashMap};

use crate::parser::{Op, Type, AST};

pub struct Env<'de> {
    store: HashMap<&'de str, Value<'de>>,
}

impl<'de> Env<'de> {
    pub fn new() -> Self {
        return Self {
            store: HashMap::new(),
        };
    }

    pub fn get(&self, name: &'de str) -> Option<&Value<'de>> {
        return self.store.get(name);
    }

    pub fn set(&mut self, name: &'de str, obj: Value<'de>) {
        self.store.insert(name, obj);
    }
}

pub struct Evaluator<'de> {
    env: Env<'de>,
}

impl<'de> Evaluator<'de> {
    pub fn new() -> Self {
        return Self { env: Env::new() };
    }

    pub fn eval_program(&mut self, tree: AST<'de>) -> Result<Value<'de>> {
        let to_return = match tree {
            AST::Fn { .. } => todo!(),
            AST::Call { .. } => todo!(),
            AST::Return { value } => Value::Return(Box::new(self.eval_program(*value)?)),
            AST::Type(val) => self.read_type(val)?,
            AST::Program { statements } => self.eval_stmts(statements)?,
            AST::If { condition, yes, no } => self.eval_if(*condition, yes, no)?,

            AST::Let { ident, value } => {
                let evaluated = self.eval_program(*value)?;
                self.env.set(ident, evaluated);
                Value::Idle
            }

            AST::Expr(op, mut operands) => match op {
                Op::Grouped => self.eval_program(operands.pop().unwrap())?,
                Op::Minus => {
                    let val = self.eval_program(operands.pop().unwrap())?;
                    if let Value::Number(num) = val {
                        Value::Number(-num);
                    }
                    return Err(self.err_msg(format!("type mismatch: {}", val)));
                }
                Op::Bang => {
                    let val = self.eval_program(operands.pop().unwrap())?;
                    match val {
                        Value::Bool(val) => Value::Bool(!val),
                        Value::Nil => Value::Bool(true),
                        _ => return Err(self.err_msg(format!("type mismatch: {}", val))),
                    }
                }
                Op::Assing => self.eval_program(operands.pop().unwrap())?,
                other_op => {
                    let right = self.eval_program(operands.pop().unwrap())?;
                    let left = self.eval_program(operands.pop().unwrap())?;

                    match (&left, &right) {
                        (Value::Number(l_val), Value::Number(r_val)) => {
                            return self.eval_infix_numbers(other_op, *l_val, *r_val);
                        }
                        (Value::Bool(_), Value::Bool(_)) => {
                            return self.eval_infix_booleans(
                                other_op,
                                self.is_truth(left),
                                self.is_truth(right),
                            );
                        }
                        _ => return Err(self.err_msg(format!("type mismatch: {} {}", left, right))),
                    }
                }
            },
        };

        return Ok(to_return);
    }

    fn eval_stmts(&mut self, statements: Vec<AST<'de>>) -> Result<Value<'de>> {
        let mut result = Ok(Value::Nil);
        for stmt in statements {
            if matches!(stmt, AST::Return { .. }) {
                return self.eval_program(stmt);
            }

            result = self.eval_program(stmt);

            if matches!(result, Err(_)) {
                return result;
            }

            if matches!(result, Ok(Value::Return(_))) {
                return result;
            }
        }

        return result;
    }

    fn eval_infix_numbers(&self, op: Op, left: f64, right: f64) -> Result<Value<'de>> {
        let result = match op {
            Op::Plus => Value::Number(left + right),
            Op::Minus => Value::Number(left - right),
            Op::Star => Value::Number(left * right),
            Op::Slash => {
                if right == 0.0 {
                    return Err(
                        self.err_msg(format!("unknown operator: {} {} {}", left, op, right))
                    );
                }
                Value::Number(left / right)
            }
            Op::Greater => Value::Bool(left > right),
            Op::GreaterEqual => Value::Bool(left >= right),
            Op::Less => Value::Bool(left < right),
            Op::LessEqual => Value::Bool(left <= right),
            Op::AssignEqual => Value::Bool(left == right),
            Op::BangEqual => Value::Bool(left != right),
            _ => return Err(self.err_msg(format!("unknown operator: {} {} {}", left, op, right))),
        };

        return Ok(result);
    }

    fn eval_infix_booleans(&self, op: Op, left: bool, right: bool) -> Result<Value<'de>> {
        let result = match op {
            Op::And => Value::Bool(left && right),
            Op::Or => Value::Bool(left || right),
            Op::AssignEqual => Value::Bool(left == right),
            Op::BangEqual => Value::Bool(left != right),
            _ => return Err(self.err_msg(format!("unknown operator: {} {} {}", left, op, right))),
        };

        return Ok(result);
    }

    fn eval_if(
        &mut self,
        condition: AST<'de>,
        yes: Vec<AST<'de>>,
        no: Option<Vec<AST<'de>>>,
    ) -> Result<Value<'de>> {
        let condition = self.eval_program(condition)?;
        if self.is_truth(condition) {
            return self.eval_stmts(yes);
        } else if !matches!(no, None) {
            return self.eval_stmts(no.unwrap());
        } else {
            return Ok(Value::Nil);
        }
    }

    fn is_truth(&self, val: Value<'de>) -> bool {
        match val {
            Value::Bool(false) => false,
            Value::Nil => false,
            _ => true,
        }
    }

    fn read_type(&self, value: Type<'de>) -> Result<Value<'de>> {
        let evaluated = match value {
            Type::Bool(bool) => Value::Bool(bool),
            Type::String(str) => Value::String(Cow::Borrowed(str)),
            Type::Number(num) => Value::Number(num),
            Type::Nil => Value::Nil,
            Type::Ident(ident) => match self.env.get(ident) {
                Some(val) => val.to_owned(),
                None => {
                    return Err(self.err_msg(format!("reference error: '{}' not declared.", ident)))
                }
            },
        };

        return Ok(evaluated);
    }

    fn err_msg(&self, err_type: String) -> anyhow::Error {
        return anyhow::Error::msg(err_type);
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value<'de> {
    Number(f64),
    Ident(&'de str),
    String(Cow<'de, str>),
    Bool(bool),
    Return(Box<Value<'de>>),
    Idle,
    Nil,
}

impl<'de> fmt::Display for Value<'de> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Value::Bool(val) => return write!(f, "{val}"),
                Value::Ident(ident) => return write!(f, "{ident}"),
                Value::String(val) => return write!(f, "{val}"),
                Value::Number(val) => return write!(f, "{val}"),
                Value::Return(value) => return write!(f, "{}", *value),
                Value::Idle => "\0",
                Value::Nil => "nil",
            }
        )
    }
}
