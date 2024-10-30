use anyhow::{anyhow, Result};
use core::fmt;
use std::{cell::RefCell, collections::HashMap, io::Write, rc::Rc};

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

    pub fn get(&self, name: &Rc<str>) -> Result<Value> {
        if let Some(value) = self.store.borrow().get(name) {
            return Ok(value.clone());
        }
        if let Some(env) = &self.outer {
            return Ok(env.get(name)?);
        }

        return Err(anyhow!("Reference error: '{name}' not declared"));
    }

    pub fn set(&mut self, name: Rc<str>, obj: Value) {
        self.store.borrow_mut().insert(name, obj);
    }
}

pub struct Evaluator<W1: Write, W2: Write> {
    pub env: Env,
    stdout: W1,
    stderr: W2,
}

impl<W1: Write, W2: Write> Evaluator<W1, W2> {
    pub fn new(env: Env, stdout: W1, stderr: W2) -> Self {
        return Self {
            env,
            stdout,
            stderr,
        };
    }

    pub fn eval(&mut self, statements: Vec<Result<AST>>) -> Value {
        let mut result = Value::Idle;
        for stmt in statements {
            let ast = match stmt {
                Ok(ast) => ast,
                Err(err) => {
                    writeln!(self.stderr, "{err}").unwrap();
                    continue;
                }
            };
            if matches!(ast, AST::Return { .. }) {
                match self.eval_ast(ast) {
                    Ok(value) => return value,
                    Err(err) => {
                        writeln!(self.stderr, "{err}").unwrap();
                        return Value::Idle;
                    }
                }
            }

            let val = self.eval_ast(ast);
            if let Ok(Value::Return(_)) = val {
                return val.unwrap();
            }
            if let Err(err) = val {
                writeln!(self.stderr, "{err}").unwrap();
                return Value::Idle;
            }

            result = val.unwrap();
        }

        return result;
    }

    fn eval_ast(&mut self, tree: AST) -> Result<Value> {
        let to_return = match tree {
            AST::Type(val) => self.eval_type(val)?,
            AST::If { condition, yes, no } => self.eval_if(*condition, yes, no)?,
            AST::Print(val) => {
                let evaluated = self.eval_ast(*val)?;
                writeln!(self.stdout, "{evaluated}").unwrap();
                self.stdout.flush().unwrap();
                Value::Idle
            }
            AST::Return { value } => {
                let to_return = self.eval_ast(*value)?;
                Value::Return(Box::new(to_return))
            }

            AST::Let { ident, value } => {
                let evaluated = self.eval_ast(*value)?;
                self.env.set(ident, evaluated);
                Value::Idle
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
                let mut result = Vec::new();
                for item in args.iter() {
                    result.push(self.eval_ast(item.clone())?);
                }

                return self.apply_fn(function, result);
            }

            AST::Expr(op, mut operands) => {
                if operands.len() == 2 {
                    let right = self.eval_ast(operands.pop().unwrap())?;

                    match op {
                        Op::And | Op::Or => {
                            let left = self.eval_ast(operands.pop().unwrap())?;
                            return self.eval_infix_booleans(op, left, right);
                        }

                        Op::Index => {
                            let left = self.eval_ast(operands.pop().unwrap())?;
                            return self.eval_index(left, right);
                        }

                        Op::AssignEqual => {
                            let left = self.eval_ast(operands.pop().unwrap())?;
                            if let (Value::Number(_), Value::Number(_)) = (&left, &right) {
                                return self.eval_infix_numbers(op, left, right);
                            }
                            return self.eval_infix_booleans(op, left, right);
                        }

                        Op::Plus => {
                            let left = self.eval_ast(operands.pop().unwrap())?;

                            if let (Value::Number(_), Value::Number(_)) = (&left, &right) {
                                return self.eval_infix_numbers(op, left, right);
                            }
                            if let (Value::String(lstr), Value::String(rstr)) = (&left, &right) {
                                let concatenated: Rc<str> =
                                    format!("{}{}", lstr.to_string(), rstr.to_string()).into();
                                return Ok(Value::String(concatenated));
                            }

                            return Err(anyhow!(
                                "[operation: {op} ] Type mismatch: '{left}' {op} '{right}'"
                            ));
                        }

                        Op::Minus
                        | Op::Star
                        | Op::Greater
                        | Op::GreaterEqual
                        | Op::Less
                        | Op::LessEqual
                        | Op::Slash => {
                            let left = self.eval_ast(operands.pop().unwrap())?;
                            return self.eval_infix_numbers(op, left, right);
                        }

                        Op::Push => {
                            let left = match operands.pop().unwrap() {
                                AST::Type(Type::Ident(ident)) => ident,
                                ast => {
                                    return Err(anyhow!(
                                        "[operation: PUSH] Error: '{ast}' not an IDENTIFIER"
                                    ))
                                }
                            };
                            let value = self.env.get(&left)?;
                            match value {
                                Value::Array(arr) => arr.borrow_mut().push(right),
                                Value::String(arr) => {
                                    if let Value::String(str) = right.clone() {
                                        let mut string = arr.to_string();
                                        if str.len() != 1 {
                                            return Err(anyhow!(
                                                "[operation: PUSH] Error: Argument to push must be a character, got '{right}'"
                                            ));
                                        }
                                        string.push(str.chars().nth(0).unwrap() as char);
                                        self.eval_reassign(left, Value::String(string.into()))?;
                                    }
                                    return Err(anyhow!(
                                        "[operation: PUSH] Error: Argument to push must be a character, got '{right}'"
                                    ));
                                }
                                val => {
                                    return Err(anyhow!(
                                        "[operation: PUSH] Type mismatch: '{val}' not iterable"
                                    ))
                                }
                            };

                            return Ok(Value::Idle);
                        }

                        Op::ReAssign => {
                            let left = match operands.pop().unwrap() {
                                AST::Type(Type::Ident(ident)) => ident,
                                ast => {
                                    return Err(anyhow!(
                                        "[operation: {op} ] Error: '{ast}' not an IDENTIFIER"
                                    ))
                                }
                            };

                            return self.eval_reassign(left, right);
                        }
                        operation => panic!("shoul not error, got: '{operation}'"),
                    }
                }

                match op {
                    Op::Grouped => self.eval_ast(operands.pop().unwrap())?,
                    Op::Assing => self.eval_ast(operands.pop().unwrap())?,
                    Op::Fn => self.eval_ast(operands.pop().unwrap())?,

                    Op::Rest => {
                        let value = self.eval_ast(operands.pop().unwrap())?;
                        match value {
                            Value::String(str) => {
                                if str.len() < 1 {
                                    return Ok(Value::Nil);
                                }
                                let string = str.get(1..).unwrap().to_string();
                                return Ok(Value::String(string.into()));
                            }
                            Value::Array(arr) => {
                                if arr.borrow().len() < 1 {
                                    return Ok(Value::Nil);
                                }
                                let new_arr: Vec<Value> = arr.borrow().get(1..).unwrap().into();
                                return Ok(Value::Array(Rc::new(RefCell::new(new_arr))));
                            }
                            value => {
                                return Err(anyhow!("[operation: {op} ] Type mismatch: '{value}'"))
                            }
                        }
                    }

                    Op::First => {
                        let value = self.eval_ast(operands.pop().unwrap())?;
                        if let Value::Array(arr) = value {
                            if arr.borrow().len() == 0 {
                                return Ok(Value::Nil);
                            }
                            return Ok(arr.borrow()[0].clone());
                        }
                        if let Value::String(arr) = value {
                            if arr.len() == 0 {
                                return Ok(Value::Nil);
                            }
                            return Ok(Value::String(
                                arr.chars().nth(0).unwrap().to_string().into(),
                            ));
                        }

                        return Err(anyhow!("[operation: {op} ] Type mismatch: '{value}'"));
                    }

                    Op::Last => {
                        let value = self.eval_ast(operands.pop().unwrap())?;
                        if let Value::Array(arr) = value {
                            if arr.borrow().len() == 0 {
                                return Ok(Value::Nil);
                            }
                            return Ok(arr.borrow().last().unwrap().clone());
                        }
                        if let Value::String(arr) = value {
                            if arr.len() == 0 {
                                return Ok(Value::Nil);
                            }
                            return Ok(Value::String(
                                arr.chars().last().unwrap().to_string().into(),
                            ));
                        }

                        return Err(anyhow!("[operation: {op} ] Type mismatch: '{value}'"));
                    }

                    Op::Len => {
                        let value = self.eval_ast(operands.pop().unwrap())?;
                        if let Value::Array(arr) = value {
                            return Ok(Value::Number(arr.borrow().len() as f64));
                        }
                        if let Value::String(arr) = value {
                            return Ok(Value::Number(arr.len() as f64));
                        }

                        return Err(anyhow!("[operation: {op} ] Type mismatch: '{value}'"));
                    }
                    Op::Minus => {
                        let val = self.eval_ast(operands.pop().unwrap())?;
                        if let Value::Number(num) = val {
                            return Ok(Value::Number(-1.0 * num));
                        }
                        return Err(anyhow!("[operation: {op} ] Type mismatch: '{val}'"));
                    }
                    Op::Bang => {
                        let val = self.eval_ast(operands.pop().unwrap())?;
                        match val {
                            Value::Bool(val) => Value::Bool(!val),
                            Value::Nil => Value::Bool(true),
                            value => {
                                return Err(anyhow!("[operation: {op} ] Type mismatch: '{value}'"))
                            }
                        }
                    }

                    _ => return Err(anyhow!("Unknown operator: {op}")),
                }
            }
        };

        return Ok(to_return);
    }

    fn eval_expressions(&mut self, arr: Vec<AST>) -> Result<Vec<Value>> {
        let mut result: Vec<Value> = Vec::new();
        for item in arr.into_iter() {
            let evaluated = self.eval_ast(item)?;
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
            _ => Err(anyhow!(
                "[operation: FUNCTION CALL] Error: not a function '{function}'"
            )),
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

    fn eval_infix_numbers(&self, op: Op, l_val: Value, r_val: Value) -> Result<Value> {
        let left = match l_val {
            Value::Number(num) => num,
            value => {
                return Err(anyhow!(
                    "[operation: {op} ] Type mismatch: '{value}' expected number"
                ))
            }
        };

        let right = match r_val {
            Value::Number(num) => num,
            value => {
                return Err(anyhow!(
                    "[operation: {op} ] Type mismatch: '{value}' expected number"
                ))
            }
        };

        let result = match op {
            Op::Plus => Value::Number(left + right),
            Op::Minus => Value::Number(left - right),
            Op::Star => Value::Number(left * right),
            Op::Slash => {
                if right == 0.0 {
                    return Err(anyhow!("Dividing by zero error: {left} '{op}' {right}"));
                }
                Value::Number(left / right)
            }
            Op::Greater => Value::Bool(left > right),
            Op::GreaterEqual => Value::Bool(left >= right),
            Op::Less => Value::Bool(left < right),
            Op::LessEqual => Value::Bool(left <= right),
            Op::AssignEqual => Value::Bool(left == right),
            Op::BangEqual => Value::Bool(left != right),
            _ => return Err(anyhow!("Unknown operator: {left} '{op}' {right}")),
        };

        return Ok(result);
    }

    fn eval_infix_booleans(&self, op: Op, l_val: Value, r_val: Value) -> Result<Value> {
        let left = match l_val {
            Value::Bool(num) => num,
            value => return Err(anyhow!("[operation: {op} ] Type mismatch: '{value}'")),
        };

        let right = match r_val {
            Value::Bool(num) => num,
            value => return Err(anyhow!("[operation: {op} ] Type mismatch: '{value}'")),
        };

        let result = match op {
            Op::And => Value::Bool(left && right),
            Op::Or => Value::Bool(left || right),
            Op::AssignEqual => Value::Bool(left == right),
            Op::BangEqual => Value::Bool(left != right),
            _ => return Err(anyhow!("Unknown operator: {left} '{op}' {right}")),
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

    fn eval_index(&mut self, arr: Value, num: Value) -> Result<Value> {
        let index = match num {
            Value::Number(n) => n as usize,
            value => {
                return Err(anyhow!(
                    "[operation: INDEX] Type mismatch: '{value}' not a number"
                ))
            }
        };

        match arr {
            Value::Array(arr) => {
                if index >= arr.borrow().len() {
                    return Err(anyhow!("[operation: INDEX] Error: index out of bounds"));
                }
                Ok(arr.borrow()[index].clone())
            }
            Value::String(str) => {
                if index >= str.len() {
                    return Err(anyhow!("[operation: INDEX] Error: index out of bounds"));
                }
                let retorno = str.chars().nth(index).unwrap().to_string();

                Ok(Value::String(retorno.into()))
            }
            value => Err(anyhow!(
                "[operation: INDEX] Type mismatch: '{value}' not iterable"
            )),
        }
    }

    fn eval_reassign(&mut self, ident: Rc<str>, value: Value) -> Result<Value> {
        let mut current_env = self.env.clone();
        loop {
            if current_env.store.borrow().contains_key(&ident) {
                current_env.set(ident.clone(), value);
                return Ok(Value::Idle);
            }

            if let Some(outer) = current_env.outer {
                current_env = *outer.clone();
            } else {
                break;
            }
        }

        return Err(anyhow!("Reference error: '{ident}' not declared"));
    }

    fn eval_type(&mut self, value: Type) -> Result<Value> {
        let evaluated = match value {
            Type::Bool(bool) => Value::Bool(bool),
            Type::String(str) => Value::String(str),
            Type::Number(num) => Value::Number(num),
            Type::Arr(vec) => {
                let list = RefCell::new(self.eval_expressions(*vec)?);
                Value::Array(Rc::new(list))
            }
            Type::Nil => Value::Nil,
            Type::Ident(ident) => self.env.get(&ident)?,
        };

        return Ok(evaluated);
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Number(f64),
    Array(Rc<RefCell<Vec<Value>>>),
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

impl fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Bool(val) => write!(f, "{val}"),
            Value::Ident(ident) => write!(f, "{ident}"),
            Value::String(val) => write!(f, "{val}"),
            Value::Number(val) => write!(f, "{val}"),
            Value::Return(value) => write!(f, "{}", *value),
            Value::Nil => write!(f, "nil"),
            Value::Array(arr) => {
                write!(f, "[")?;
                for (i, element) in arr.borrow().iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{element}")?;
                }
                write!(f, "]")
            }
            _ => write!(f, "\0"),
        }
    }
}

#[cfg(test)]
mod test {
    use std::io;

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
            let result = Evaluator::new(Env::new(), io::stdout(), io::stderr()).eval(program);

            println!("expected: {expected}, got: {result}");
            assert_eq!(expected, result);
        }

        anyhow::Ok(())
    }
}
