use anyhow::Result;
use core::fmt;
use std::{borrow::Borrow, collections::HashMap, rc::Rc};

use crate::parser::{Op, Type, AST};

pub struct Env {
    store: HashMap<Rc<str>, Value>,
}

impl Env {
    pub fn new() -> Self {
        return Self {
            store: HashMap::new(),
        };
    }

    pub fn get(&self, name: &Rc<str>) -> Option<&Value> {
        return self.store.get(name);
    }

    pub fn set(&mut self, name: Rc<str>, obj: Value) {
        self.store.insert(name, obj);
    }
}

pub struct Evaluator {
    env: Env,
}

impl Evaluator {
    pub fn new() -> Self {
        return Self { env: Env::new() };
    }

    pub fn eval(&mut self, tree: AST) -> Result<Value> {
        let to_return = match tree {
            AST::Program { statements } => self.eval_stmts(statements)?,
            AST::Type(val) => self.read_type(val)?,
            AST::Return { value } => Value::Return(Box::new(self.eval(*value)?)),
            AST::If { condition, yes, no } => self.eval_if(*condition, yes, no)?,

            AST::Fn { .. } => todo!(),
            AST::Call { .. } => todo!(),

            AST::Let { ident, value } => {
                let evaluated = self.eval(*value)?;
                self.env.set(ident, evaluated);
                Value::Idle
            }

            AST::Expr(op, mut operands) => {
                if operands.len() == 2 {
                    let right = self.eval(operands.pop().unwrap())?;
                    let left = self.eval(operands.pop().unwrap())?;

                    match (&left, &right) {
                        (Value::Number(l_val), Value::Number(r_val)) => {
                            return self.eval_infix_numbers(op, *l_val, *r_val);
                        }
                        (Value::Bool(_), Value::Bool(_)) => {
                            return self.eval_infix_booleans(
                                op,
                                self.is_truth(left),
                                self.is_truth(right),
                            );
                        }
                        _ => return Err(self.err_msg(format!("type mismatch: {} {}", left, right))),
                    }
                }

                match op {
                    Op::Grouped => self.eval(operands.pop().unwrap())?,
                    Op::Assing => self.eval(operands.pop().unwrap())?,
                    Op::Minus => {
                        let val = self.eval(operands.pop().unwrap())?;
                        if let Value::Number(num) = val {
                            return Ok(Value::Number(-1.0 * num));
                        }
                        return Err(self.err_msg(format!("type mismatch: {}", val)));
                    }
                    Op::Bang => {
                        let val = self.eval(operands.pop().unwrap())?;
                        match val {
                            Value::Bool(val) => Value::Bool(!val),
                            Value::Nil => Value::Bool(true),
                            _ => return Err(self.err_msg(format!("type mismatch: {}", val))),
                        }
                    }

                    _ => return Err(self.err_msg(format!("unknown opreator: {}", op))),
                }
            }
        };

        return Ok(to_return);
    }

    fn eval_stmts(&mut self, statements: Vec<AST>) -> Result<Value> {
        let mut result = Ok(Value::Nil);
        for stmt in statements {
            if matches!(stmt, AST::Return { .. }) {
                return self.eval(stmt);
            }

            result = self.eval(stmt);

            if matches!(result, Err(_)) {
                return result;
            }

            if matches!(result, Ok(Value::Return(_))) {
                return result;
            }
        }

        return result;
    }

    fn eval_block_stmt(&mut self, block: Rc<[AST]>) -> Result<Value> {
        let mut result = Ok(Value::Nil);
        for stmt in block.iter() {
            result = self.eval(stmt.clone());

            if matches!(result, Err(_)) {
                return result;
            }

            if matches!(result, Ok(Value::Return(_))) {
                return result;
            }
        }

        return result;
    }

    fn eval_infix_numbers(&self, op: Op, left: f64, right: f64) -> Result<Value> {
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

    fn eval_infix_booleans(&self, op: Op, left: bool, right: bool) -> Result<Value> {
        let result = match op {
            Op::And => Value::Bool(left && right),
            Op::Or => Value::Bool(left || right),
            Op::AssignEqual => Value::Bool(left == right),
            Op::BangEqual => Value::Bool(left != right),
            _ => return Err(self.err_msg(format!("unknown operator: {} {} {}", left, op, right))),
        };

        return Ok(result);
    }

    fn eval_if(&mut self, condition: AST, yes: Rc<[AST]>, no: Option<Rc<[AST]>>) -> Result<Value> {
        let condition = self.eval(condition)?;
        if self.is_truth(condition) {
            return self.eval_block_stmt(yes);
        } else if !matches!(no, None) {
            return self.eval_block_stmt(no.unwrap());
        } else {
            return Ok(Value::Nil);
        }
    }

    fn is_truth(&self, val: Value) -> bool {
        match val {
            Value::Bool(false) => false,
            Value::Nil => false,
            _ => true,
        }
    }

    fn read_type(&self, value: Type) -> Result<Value> {
        let evaluated = match value {
            Type::Bool(bool) => Value::Bool(bool),
            Type::String(str) => Value::String(str),
            Type::Number(num) => Value::Number(num),
            Type::Nil => Value::Nil,
            Type::Ident(ident) => match self.env.get(ident.borrow()) {
                Some(val) => val.clone(),
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
pub enum Value {
    Number(f64),
    Ident(Rc<str>),
    String(Rc<str>),
    Bool(bool),
    Return(Box<Value>),
    Idle,
    Nil,
}

impl fmt::Display for Value {
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
