use anyhow::Result;
use core::fmt;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::parser::{Op, Type, AST};

#[derive(Debug, PartialEq, Clone)]
pub struct Env {
    pub store: Rc<RefCell<HashMap<Rc<str>, Value>>>,
    pub outer: Option<Box<Env>>,
}

impl Env {
    pub fn new() -> Self {
        return Self {
            store: Rc::new(HashMap::new().into()),
            outer: None,
        };
    }

    pub fn get(&self, name: &Rc<str>) -> Option<Value> {
        if let Some(value) = self.store.borrow().get(name) {
            return Some(value.clone());
        }

        match &self.outer {
            Some(env) => {
                if let Some(value) = env.get(name) {
                    return Some(value.clone());
                }
                None
            }
            None => None,
        }
    }

    pub fn set(&mut self, name: Rc<str>, obj: Value) {
        self.store.borrow_mut().insert(name, obj);
    }
}

pub struct Evaluator {
    pub env: Env,
}

impl Evaluator {
    pub fn new(env: Env) -> Self {
        return Self { env };
    }

    pub fn eval(&mut self, statements: Vec<Result<AST>>) -> Result<Value> {
        let mut result = Ok(Value::Idle);
        for stmt in statements {
            let ast = match stmt {
                Ok(ast) => ast,
                Err(err) => {
                    eprintln!("{err}");
                    continue;
                }
            };
            if matches!(ast, AST::Return { .. }) {
                return self.eval_ast(ast);
            }

            result = self.eval_ast(ast);
            if matches!(result, Err(_)) || matches!(result, Ok(Value::Return(_))) {
                return result;
            }
        }

        return result;
    }

    fn eval_ast(&mut self, tree: AST) -> Result<Value> {
        let to_return = match tree {
            AST::Type(val) => self.eval_type(val)?,
            AST::Print(val) => {
                let evaluated = self.eval_ast(*val)?;
                println!("{evaluated}");
                Value::Idle
            }
            AST::Return { value } => {
                let to_return = self.eval_ast(*value)?;
                Value::Return(Box::new(to_return))
            }
            AST::If { condition, yes, no } => self.eval_if(*condition, yes, no)?,

            AST::Len(expr) => {
                let evaluated = self.eval_ast(*expr)?;
                match evaluated {
                    Value::String(str) => Value::Number(str.len() as f64),
                    _ => {
                        return Err(self.err_msg(format!(
                            "type mistmach: Expected string, got: {}",
                            evaluated
                        )))
                    }
                }
            }

            AST::Let { ident, value } => {
                let evaluated = self.eval_ast(*value)?;
                self.env.set(ident, evaluated);
                Value::Idle
            }

            AST::Reassign { ident, value } => {
                let evaluated = self.eval_ast(*value)?;
                let mut current_env = self.env.clone();
                loop {
                    if current_env.store.borrow().contains_key(&ident) {
                        current_env.set(ident, evaluated);
                        return Ok(Value::Idle);
                    }

                    if let Some(outer) = current_env.outer {
                        current_env = *outer.clone();
                    } else {
                        break;
                    }
                }

                return Err(
                    self.err_msg(format!("Cannot reassign to undeclared variable: {}", ident))
                );
            }

            AST::Fn { name, params, body } => {
                let env = self.env.clone();
                if let Some(fn_name) = name {
                    self.env.set(
                        fn_name.clone(),
                        Value::Fn {
                            params: params.clone(),
                            body: body.clone(),
                            env,
                        },
                    );

                    return Ok(Value::Idle);
                }

                Value::Fn {
                    params: params.clone(),
                    body: body.clone(),
                    env,
                }
            }

            AST::Call { calle, args } => {
                let function = self.eval_ast(*calle)?;
                let args = self.eval_args(args)?;

                return self.apply_fn(function, args);
            }

            AST::Expr(op, mut operands) => {
                if operands.len() == 2 {
                    let right = self.eval_ast(operands.pop().unwrap())?;
                    let left = self.eval_ast(operands.pop().unwrap())?;

                    match (&left, &right) {
                        (Value::Number(l_val), Value::Number(r_val)) => {
                            return self.eval_infix_numbers(op, *l_val, *r_val);
                        }
                        (Value::String(left), Value::String(right)) => {
                            if matches!(op, Op::Plus) {
                                let string = format!("{}{}", left, right);
                                return Ok(Value::String(string.into()));
                            }

                            return Err(
                                self.err_msg(format!("invalid operator: {}{}{}", left, op, right))
                            );
                        }
                        (Value::Bool(_), Value::Bool(_)) => {
                            return self.eval_infix_booleans(
                                op,
                                self.is_truth(left),
                                self.is_truth(right),
                            );
                        }
                        _ => {
                            return Err(self.err_msg(format!("type mismatch: {} {}", left, right)));
                        }
                    }
                }

                match op {
                    Op::Grouped => self.eval_ast(operands.pop().unwrap())?,
                    Op::Assing => self.eval_ast(operands.pop().unwrap())?,
                    Op::Fn => self.eval_ast(operands.pop().unwrap())?,
                    Op::Len => self.eval_ast(operands.pop().unwrap())?,
                    Op::Minus => {
                        let val = self.eval_ast(operands.pop().unwrap())?;
                        if let Value::Number(num) = val {
                            return Ok(Value::Number(-1.0 * num));
                        }
                        return Err(self.err_msg(format!("type mismatch: {}", val)));
                    }
                    Op::Bang => {
                        let val = self.eval_ast(operands.pop().unwrap())?;
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

    fn eval_args(&mut self, args: Rc<[AST]>) -> Result<Vec<Value>> {
        let mut result: Vec<Value> = Vec::new();
        for arg in args.iter() {
            let evaluated = self.eval_ast(arg.clone())?;
            result.push(evaluated);
        }

        return Ok(result);
    }

    fn apply_fn(&mut self, function: Value, args: Vec<Value>) -> Result<Value> {
        match function {
            Value::Fn { params, body, env } => {
                let mut env_call = Env::new();
                env_call.outer = Some(Box::new(env.clone()));

                for (i, param) in params.iter().enumerate() {
                    env_call.set(param.clone(), args[i].clone());
                }

                let current = self.env.clone();
                self.env = env_call;
                let evaluated = self.eval_block_stmt(body)?;
                self.env = current;

                match evaluated {
                    Value::Return(val) => return Ok(*val),
                    _ => return Ok(evaluated),
                };
            }
            _ => Err(self.err_msg(format!("Not a function: {}", function))),
        }
    }

    fn eval_block_stmt(&mut self, block: Rc<[AST]>) -> Result<Value> {
        let mut result = Ok(Value::Nil);

        for stmt in block.iter() {
            result = self.eval_ast(stmt.clone());

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
        let condition = self.eval_ast(condition)?;
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

    fn eval_type(&self, value: Type) -> Result<Value> {
        let evaluated = match value {
            Type::Bool(bool) => Value::Bool(bool),
            Type::String(str) => Value::String(str),
            Type::Number(num) => Value::Number(num),
            Type::Nil => Value::Nil,
            Type::Ident(ident) => {
                if let Some(val) = self.env.get(&ident) {
                    return Ok(val);
                }

                return Err(self.err_msg(format!("reference error: '{}' not declared.", ident)));
            }
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

    Fn {
        params: Rc<[Rc<str>]>,
        body: Rc<[AST]>,
        env: Env,
    },
}

impl<'e> fmt::Display for Value {
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
                Value::Nil => "nil",
                _ => "\0",
            }
        )
    }
}

#[cfg(test)]
mod test {
    use anyhow::Result;

    use crate::{eval::Value, parser::Parser};

    use super::{Env, Evaluator};

    #[test]
    fn functions_calls() -> Result<()> {
        let input: Vec<(&'static str, Value)> = [
            (
                "let identity = fn(x) { x; }; identity(5);",
                Value::Number(5.0),
            ),
            (
                "let identity = fn(x) { return x; }; identity(5);",
                Value::Number(5.0),
            ),
            (
                "let double = fn(x) { x * 2; }; double(5);",
                Value::Number(10.0),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5, 5);",
                Value::Number(10.0),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
                Value::Number(20.0),
            ),
        ]
        .into();

        for (code, expected) in input {
            let program = Parser::new(code.into()).parse();
            let result = Evaluator::new(Env::new()).eval(program).unwrap();

            println!("expected: {expected}, got: {result}");
            assert_eq!(expected, result);
        }

        anyhow::Ok(())
    }
}
