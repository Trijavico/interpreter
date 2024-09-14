use core::fmt;
use std::{iter::Peekable, rc::Rc};

use crate::lexer::{Lexer, Token};

pub struct Parser {
    lexer: Peekable<Lexer>,
}

impl Parser {
    pub fn new(input: String) -> Self {
        return Self {
            lexer: Lexer::new(input).peekable(),
        };
    }

    pub fn parse(&mut self) -> AST {
        let statements = self.parse_statement();

        return AST::Program { statements };
    }

    fn expect_peek(&mut self, tok: Token) {
        let next_tok = match self.lexer.peek() {
            Some(Ok(next)) => next,
            _ => {
                //panic!("expected {}", tok)
                todo!()
            }
        };

        if *next_tok == tok {
            self.lexer.next();
        }
    }

    fn parse_statement(&mut self) -> Vec<AST> {
        let mut statements: Vec<AST> = Vec::new();

        while let Some(tok_result) = self.lexer.peek() {
            let tok = match tok_result {
                Ok(tok) => tok,
                Err(err) => panic!("Error: {}", err),
            };

            let token = match tok {
                Token::Let => self.parse_var(),
                Token::Fn => self.parse_fun(),
                Token::Return => self.parse_return(),
                Token::If => self.parse_if(),
                Token::Print => self.parse_print(),
                Token::EOF => break,
                Token::LParen
                | Token::Number(_, _)
                | Token::String(_)
                | Token::Ident(_)
                | Token::Bang
                | Token::Minus
                | Token::True
                | Token::False => self.parse_expression_statements(),
                _ => break,
            };

            statements.push(token);
        }

        return statements;
    }

    fn parse_expression(&mut self, prev_binding: u8) -> AST {
        let l_side = match self.lexer.next() {
            Some(Ok(tok)) => tok,
            _ => return AST::Type(Type::Nil),
        };

        let mut to_return = match l_side {
            Token::String(val) => AST::Type(Type::String(val)),
            Token::Number(_, num) => AST::Type(Type::Number(num)),
            Token::Ident(ident) => AST::Type(Type::Ident(ident)),
            Token::True => AST::Type(Type::Bool(true)),
            Token::False => AST::Type(Type::Bool(false)),
            Token::Fn => self.parse_fun(),
            Token::If => self.parse_if(),

            Token::Assign | Token::LParen => {
                let r_side = self.parse_expression(0);
                if matches!(l_side, Token::LParen) {
                    self.expect_peek(Token::RParen);
                    AST::Expr(Op::Grouped, vec![r_side])
                } else {
                    AST::Expr(Op::Assing, vec![r_side])
                }
            }

            Token::Bang | Token::Minus => {
                let op = match l_side {
                    Token::Bang => Op::Bang,
                    Token::Minus => Op::Minus,
                    _ => return AST::Type(Type::Nil),
                };

                let (_, r_binding) = self.prefix_binding_power(op);
                let r_side = self.parse_expression(r_binding);
                AST::Expr(op, vec![r_side])
            }

            _ => return AST::Type(Type::Nil),
        };

        loop {
            let tok = match self.lexer.peek() {
                Some(Ok(tok)) => tok,
                Some(Err(err)) => panic!("err: {}", err),
                None => {
                    break;
                }
            };

            let op = match tok {
                Token::Plus => Op::Plus,
                Token::Minus => Op::Minus,
                Token::Star => Op::Star,
                Token::Slash => Op::Slash,
                Token::LParen => Op::Fn,
                Token::AssignEqual => Op::AssignEqual,
                Token::Bang => Op::Bang,
                Token::BangEqual => Op::BangEqual,
                Token::Less => Op::Less,
                Token::LessEqual => Op::LessEqual,
                Token::Greater => Op::Greater,
                Token::GreaterEqual => Op::GreaterEqual,
                Token::And => Op::And,
                Token::Or => Op::Or,
                _ => break,
            };

            if let Some((l_binding, _)) = self.postfix_binding_power(op) {
                if l_binding < prev_binding {
                    break;
                }
                self.expect_peek(Token::LParen);
                to_return = AST::Call {
                    calle: Box::new(to_return),
                    args: self.parse_params(),
                };
                self.expect_peek(Token::RParen);
                to_return = AST::Expr(op, vec![to_return, AST::Type(Type::Nil)]);

                continue;
            }

            if let Some((l_binding, r_binding)) = self.infix_binding_power(op) {
                if l_binding < prev_binding {
                    break;
                }
                self.lexer.next();
                let r_side = self.parse_expression(r_binding);
                to_return = AST::Expr(op, vec![to_return, r_side]);

                continue;
            }

            break;
        }

        return to_return;
    }

    fn postfix_binding_power(&self, op: Op) -> Option<(u8, ())> {
        match op {
            Op::Fn => Some((13, ())),
            _ => None,
        }
    }

    fn prefix_binding_power(&self, op: Op) -> ((), u8) {
        match op {
            Op::Plus | Op::Minus | Op::Bang => ((), 11),
            _ => panic!("bad operation"),
        }
    }

    fn infix_binding_power(&self, op: Op) -> Option<(u8, u8)> {
        let binding_power = match op {
            Op::And | Op::Or => (3, 4),
            Op::BangEqual
            | Op::AssignEqual
            | Op::Less
            | Op::LessEqual
            | Op::Greater
            | Op::GreaterEqual => (5, 6),
            Op::Plus | Op::Minus => (7, 8),
            Op::Star | Op::Slash => (9, 10),
            _ => return None,
        };

        return Some(binding_power);
    }

    fn parse_var(&mut self) -> AST {
        self.lexer.next();

        let ident = match self.lexer.next() {
            Some(Ok(Token::Ident(val))) => val,
            Some(Err(err)) => panic!("Error: {}", err),
            _ => panic!("Expected an indentifier"),
        };

        let value = match self.lexer.peek() {
            Some(Ok(Token::Assign)) => self.parse_expression(0),
            Some(Err(err)) => panic!("Error: {}", err),
            _ => AST::Type(Type::Nil),
        };

        self.expect_peek(Token::Semicolon);

        return AST::Let {
            ident,
            value: Box::new(value),
        };
    }

    fn parse_return(&mut self) -> AST {
        self.lexer.next();
        let value = self.parse_expression(0);
        self.expect_peek(Token::Semicolon);

        return AST::Return {
            value: Box::new(value),
        };
    }

    fn parse_if(&mut self) -> AST {
        if matches!(self.lexer.peek(), Some(Ok(Token::If))) {
            self.lexer.next();
        }
        let condition = self.parse_expression(0);

        self.expect_peek(Token::LBrace);
        let yes = self.parse_block();
        self.expect_peek(Token::RBrace);

        let no = match self.lexer.peek() {
            Some(Ok(Token::Else)) => {
                self.lexer.next();
                self.expect_peek(Token::LBrace);
                let no = self.parse_block();
                self.expect_peek(Token::RBrace);
                Some(no)
            }
            _ => None,
        };

        return AST::If {
            condition: Box::new(condition),
            yes,
            no,
        };
    }

    fn parse_block(&mut self) -> Rc<[AST]> {
        return self.parse_statement().into();
    }

    fn parse_fun(&mut self) -> AST {
        if matches!(self.lexer.peek(), Some(Ok(Token::Fn))) {
            self.lexer.next();
        }
        let name = if let Some(Ok(Token::Ident(val))) = self.lexer.peek() {
            let name = val.clone();
            self.lexer.next();
            Some(name)
        } else {
            None
        };
        self.expect_peek(Token::LParen);
        let params = self.parse_params();
        self.expect_peek(Token::RParen);

        self.expect_peek(Token::LBrace);
        let body = self.parse_block();
        self.expect_peek(Token::RBrace);

        return AST::Fn { name, params, body };
    }

    fn parse_params(&mut self) -> Rc<[AST]> {
        let mut params: Vec<AST> = Vec::new();

        if !matches!(self.lexer.peek(), Some(Ok(Token::RParen))) {
            dbg!(self.lexer.peek());
            params.push(self.parse_expression(0));
        }

        while matches!(self.lexer.peek(), Some(Ok(Token::Comma))) {
            self.lexer.next();
            params.push(self.parse_expression(0));
        }

        return params.into();
    }

    fn parse_print(&self) -> AST {
        todo!()
    }

    fn parse_expression_statements(&mut self) -> AST {
        let expr = self.parse_expression(0);
        if matches!(self.lexer.peek(), Some(Ok(Token::Semicolon))) {
            self.lexer.next();
        }

        return expr;
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Op {
    Plus,
    Minus,
    Star,
    Slash,
    Bang,
    Grouped,
    Assing,
    Fn,
    BangEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    AssignEqual,
    Or,
    And,
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Op::Minus => "-",
                Op::Plus => "+",
                Op::Star => "*",
                Op::Assing => "=",
                Op::BangEqual => "!=",
                Op::AssignEqual => "==",
                Op::LessEqual => "<=",
                Op::GreaterEqual => ">=",
                Op::Less => "<",
                Op::Greater => ">",
                Op::Slash => "/",
                Op::Bang => "!",
                Op::And => "and",
                Op::Or => "or",
                Op::Fn => "call",
                Op::Grouped => "",
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    String(Rc<str>),
    Number(f64),
    Ident(Rc<str>),
    Bool(bool),
    Nil,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // NOTE: this feels more correct:
            // Type::String(s) => write!(f, "\"{s}\""),
            Type::String(s) => write!(f, "{s}"),
            Type::Number(n) => {
                if *n == n.trunc() {
                    // tests require that integers are printed as N.0
                    write!(f, "{n}.0")
                } else {
                    write!(f, "{n}")
                }
            }
            Type::Nil => write!(f, "nil"),
            Type::Bool(b) => write!(f, "{b:?}"),
            Type::Ident(i) => write!(f, "{i}"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AST {
    Program {
        statements: Vec<AST>,
    },

    Type(Type),

    Expr(Op, Vec<AST>),

    Let {
        ident: Rc<str>,
        value: Box<AST>, // Expr
    },

    Fn {
        name: Option<Rc<str>>,
        params: Rc<[AST]>,
        body: Rc<[AST]>,
    },

    Call {
        calle: Box<AST>,
        args: Rc<[AST]>,
    },

    Return {
        value: Box<AST>, // Expr
    },

    If {
        condition: Box<AST>,
        yes: Rc<[AST]>,
        no: Option<Rc<[AST]>>,
    },
}

impl fmt::Display for AST {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AST::Type(i) => write!(f, "{}", i),
            AST::Program { statements } => {
                for stmt in statements {
                    write!(f, "{stmt}")?;
                }

                write!(f, "")
            }
            AST::Expr(head, rest) => {
                write!(f, "({}", head)?;
                for s in rest {
                    write!(f, " {s}")?
                }
                write!(f, ")")
            }
            AST::Fn { name, params, body } => {
                if let Some(val) = name {
                    write!(f, "{val}")?;
                } else {
                    write!(f, "(fun")?;
                }

                for p in params.iter() {
                    write!(f, " {p}")?;
                }
                for stmt in body.iter() {
                    write!(f, " {}", stmt)?;
                }
                write!(f, "}}")
            }
            AST::Call { calle, args } => {
                write!(f, "({calle}")?;
                for a in args.iter() {
                    write!(f, " {a}")?
                }
                write!(f, ")")
            }
            AST::If { condition, yes, no } => {
                write!(f, "if {condition} {{")?;
                for stmt in yes.iter() {
                    write!(f, " {stmt}")?;
                }

                write!(f, " }} ")?;

                if let Some(no) = no {
                    write!(f, "else {{")?;
                    for stmt in no.iter() {
                        write!(f, " {stmt}")?;
                    }
                    write!(f, " }}")?;
                }
                write!(f, "")
            }
            AST::Return { value } => {
                write!(f, "return {value}")
            }
            AST::Let { ident, value } => {
                write!(f, "{ident}")?;
                write!(f, "{value}")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use anyhow::{Ok, Result};

    use super::{Op, Parser, Type, AST};

    #[test]
    fn test_expressions_st() -> Result<()> {
        let input = "(1 + 3) * 2;";
        let mut parser = Parser::new(input);

        let result = parser.parse();
        println!("{:?}", result);

        Ok(())
    }

    #[test]
    fn test_var() -> Result<()> {
        let input = r#"let num = 1;
        let num2 = 2;
        let num3 = 3;
        "#;
        let expected = vec![
            AST::Let {
                ident: "num",
                value: Box::new(AST::Expr(Op::Assing, vec![AST::Type(Type::Number(1.0))])),
            },
            AST::Let {
                ident: "num2",
                value: Box::new(AST::Expr(Op::Assing, vec![AST::Type(Type::Number(2.0))])),
            },
            AST::Let {
                ident: "num3",
                value: Box::new(AST::Expr(Op::Assing, vec![AST::Type(Type::Number(3.0))])),
            },
        ];

        let mut parser = Parser::new(input);
        let statements = match parser.parse() {
            AST::Program { statements } => statements,
            _ => panic!("expected PROGRAM AST"),
        };

        assert_test(expected, statements)
    }

    #[test]
    fn test_return() -> Result<()> {
        let input = r#"return 1;
        return 2 + 2;
        return 3;
        "#;

        let expected = vec![
            AST::Return {
                value: Box::new(AST::Type(Type::Number(1.0))),
            },
            AST::Return {
                value: Box::new(AST::Expr(
                    Op::Plus,
                    vec![AST::Type(Type::Number(2.0)), AST::Type(Type::Number(2.0))],
                )),
            },
            AST::Return {
                value: Box::new(AST::Type(Type::Number(3.0))),
            },
        ];

        let mut parser = Parser::new(input);
        let statements = match parser.parse() {
            AST::Program { statements } => statements,
            _ => panic!("expected PROGRAM AST"),
        };

        assert_test(expected, statements)
    }

    #[test]
    fn test_if() -> Result<()> {
        let input = r#"if true {
            let numero = 1;
            let dos = "dos";
        }
        "#;

        let mut parser = Parser::new(input);
        let statements = match parser.parse() {
            AST::Program { statements } => statements,
            _ => panic!("expected PROGRAM AST"),
        };

        let expected = vec![AST::If {
            condition: Box::new(AST::Type(Type::Bool(true))),
            yes: vec![
                AST::Let {
                    ident: "numero",
                    value: Box::new(AST::Expr(Op::Assing, vec![AST::Type(Type::Number(1.0))])),
                },
                AST::Let {
                    ident: "dos",
                    value: Box::new(AST::Expr(Op::Assing, vec![AST::Type(Type::String("dos"))])),
                },
            ],
            no: None,
        }];

        assert_test(expected, statements)
    }

    #[test]
    fn test_fun_literal() -> Result<()> {
        let input = r#"fn quick_sort(){
            let array = 0;
            let pivot = 1;
            
            return 0; 
        }"#;

        let mut parser = Parser::new(input);
        let statements = match parser.parse() {
            AST::Program { statements } => statements,
            _ => panic!("expected PROGRAM AST"),
        };

        let expected = vec![AST::Fn {
            name: Some("quick_sort"),
            params: vec![],
            body: vec![
                AST::Let {
                    ident: "array",
                    value: Box::new(AST::Expr(Op::Assing, vec![AST::Type(Type::Number(0.0))])),
                },
                AST::Let {
                    ident: "pivot",
                    value: Box::new(AST::Expr(Op::Assing, vec![AST::Type(Type::Number(1.0))])),
                },
                AST::Return {
                    value: Box::new(AST::Type(Type::Number(0.0))),
                },
            ],
        }];

        assert_test(expected, statements)
    }

    #[test]
    fn test_fun_expr() -> Result<()> {
        let input = r#"let quick = fn(param1, param2){
            return 0; 
        };"#;

        let mut parser = Parser::new(input);
        let statements = match parser.parse() {
            AST::Program { statements } => statements,
            _ => panic!("expected PROGRAM AST"),
        };

        let expected = vec![AST::Let {
            ident: "quick",
            value: Box::new(AST::Expr(
                Op::Assing,
                vec![AST::Fn {
                    name: None,
                    params: vec![
                        AST::Type(Type::Ident("param1")),
                        AST::Type(Type::Ident("param2")),
                    ],
                    body: vec![AST::Return {
                        value: Box::new(AST::Type(Type::Number(0.0))),
                    }],
                }],
            )),
        }];

        assert_test(expected, statements)
    }

    #[test]
    fn test_call_expr() -> Result<()> {
        let input = r#"add(1, 2);
        minus(1 + 2, 0);
        "#;

        let mut parser = Parser::new(input);
        let statements = match parser.parse() {
            AST::Program { statements } => statements,
            _ => panic!("expected PROGRAM AST"),
        };

        let expected = vec![
            AST::Expr(
                Op::Fn,
                vec![
                    AST::Call {
                        calle: Box::new(AST::Type(Type::Ident("add"))),
                        args: vec![AST::Type(Type::Number(1.0)), AST::Type(Type::Number(2.0))],
                    },
                    AST::Type(Type::Nil),
                ],
            ),
            AST::Expr(
                Op::Fn,
                vec![
                    AST::Call {
                        calle: Box::new(AST::Type(Type::Ident("minus"))),
                        args: vec![
                            AST::Expr(
                                Op::Plus,
                                vec![AST::Type(Type::Number(1.0)), AST::Type(Type::Number(2.0))],
                            ),
                            AST::Type(Type::Number(0.0)),
                        ],
                    },
                    AST::Type(Type::Nil),
                ],
            ),
        ];

        assert_test(expected, statements)
    }

    #[test]
    fn test_primary_expr() -> Result<()> {
        let input = r#"variable
        10;
        "verbo"
        "#;

        let mut parser = Parser::new(input);
        let statements = match parser.parse() {
            AST::Program { statements } => statements,
            _ => panic!("expected PROGRAM AST"),
        };

        let expected = vec![
            AST::Type(Type::Ident("variable")),
            AST::Type(super::Type::Number(10.0)),
            AST::Type(Type::String("verbo")),
        ];

        assert_test(expected, statements)
    }

    #[test]
    fn test_bang_expr() -> Result<()> {
        let input = r#"!true"#;

        let mut parser = Parser::new(input);
        let statements = match parser.parse() {
            AST::Program { statements } => statements,
            _ => panic!("expected PROGRAM AST"),
        };

        let expected = vec![AST::Expr(
            Op::Bang,
            vec![AST::Type(Type::Bool(true)), AST::Type(Type::Nil)],
        )];

        dbg!(expected);
        dbg!(statements);

        Ok(())
    }

    #[test]
    fn test_prueba() -> Result<()> {
        let input = "let d = if 10 > 1 { 99 } else { 100 };";
        let mut parser = Parser::new(input);

        println!("{:?}", parser.parse());

        Ok(())
    }

    fn assert_test(expected: Vec<AST<'_>>, statements: Vec<AST<'_>>) -> Result<()> {
        assert_eq!(
            expected.len(),
            statements.len(),
            "expected: {}, got: {}",
            expected.len(),
            statements.len()
        );

        for (i, stmt) in statements.iter().enumerate() {
            assert_eq!(
                expected[i], *stmt,
                "expected: {:?}, got: {:?}",
                expected[i], stmt
            )
        }

        Ok(())
    }
}
