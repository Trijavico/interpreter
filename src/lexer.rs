use anyhow::{anyhow, Result};
use core::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Comma,
    Dot,
    Minus,
    Plus,
    Slash,
    Colon,
    Semicolon,
    Star,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    Bang,
    BangEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Assign,
    AssignEqual,

    Ident(Rc<str>),
    String(Rc<str>),
    Number(Rc<str>, f64),

    And,
    Print,
    Else,
    Return,
    True,
    Let,
    Or,
    Fn,
    False,
    Len,
    If,
    Nil,
    Push,
    First,
    Last,
    Rest,

    EOF,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Token::LBrace => "LEFT_BRACE {",
                Token::RBrace => "RIGHT_BRACE }",
                Token::LParen => "LEFT_PAREN (",
                Token::RParen => "RIGHT_PAREN )",
                Token::LBracket => "LEFT_BRACKET [",
                Token::RBracket => "RIGHT_BRACKET ]",

                Token::Dot => "DOT .",
                Token::Minus => "MINUS -",
                Token::Plus => "PLUS +",
                Token::Star => "STAR *",
                Token::Semicolon => "SEMICOLON ;",
                Token::Colon => "COLON :",
                Token::Comma => "COMMA ,",
                Token::Bang => "BANG !",
                Token::BangEqual => "BANG_EQUAL !=",
                Token::Assign => "EQUAL =",
                Token::AssignEqual => "EQUAL_EQUAL ==",
                Token::Greater => "GREATER >",
                Token::GreaterEqual => "GREATER_EQUAL >=",
                Token::Less => "LESS <",
                Token::LessEqual => "LESS_EQUAL <=",
                Token::Slash => "SLASH /",

                Token::And => "AND and",
                Token::Or => "OR or",
                Token::False => "FALSE false",
                Token::True => "TRUE true",
                Token::If => "IF if",
                Token::Else => "ELSE else",
                Token::Print => "PRINT print",
                Token::Let => "LET let",
                Token::Fn => "FN fun",
                Token::Nil => "NIL nil",
                Token::Len => "LEN len",
                Token::Return => "RETURN return",
                Token::Push => "PUSH push",
                Token::First => "FIRST first",
                Token::Last => "LAST last",
                Token::Rest => "REST rest",

                Token::Ident(val) => return write!(f, "IDENTIFIER {val}"),
                Token::String(val) => return write!(f, "STRING \"{val}\" {val}"),
                Token::Number(lit, num) => {
                    if *num == num.trunc() {
                        return write!(f, "NUMBER {lit} {:.1}", num);
                    } else {
                        return write!(f, "NUMBER {lit} {num}");
                    }
                }

                Token::EOF => "EOF ",
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TokenKind {
    pub token: Token,
    pub line: usize,
}

pub struct Lexer {
    input: String,
    char: char,
    line: usize,
    current_pos: usize,
    next_pos: usize,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Self {
            input,
            char: '\0',
            line: 1,
            current_pos: 0,
            next_pos: 0,
        };

        lexer.next_char();
        return lexer;
    }

    fn next_char(&mut self) {
        if self.next_pos >= self.input.len() {
            self.current_pos = self.next_pos;
            self.next_pos += 1;
            self.char = '\0';
            return;
        }

        self.current_pos = self.next_pos;
        self.next_pos += 1;
        self.char = self.input.as_bytes()[self.current_pos] as char;

        if self.char == '\n' && self.current_pos != self.input.len() - 1 {
            self.line += 1;
        }
    }

    fn peek(&mut self) -> char {
        if self.next_pos >= self.input.len() {
            return '\0';
        }

        return self.input.as_bytes()[self.next_pos] as char;
    }

    fn skip_whitespace(&mut self) {
        loop {
            if self.char == '/' && self.peek() == '/' {
                self.next_char();
                self.skip_comments();
            }

            if !self.char.is_whitespace() {
                break;
            }

            self.next_char();
        }
    }

    fn skip_comments(&mut self) {
        loop {
            self.next_char();
            if self.char == '\n' || self.char == '\0' {
                break;
            }
        }
    }

    fn is_post_equal(&mut self, yes: Token, no: Token) -> Token {
        if self.peek() == '=' {
            self.next_char();

            return yes;
        }

        return no;
    }

    fn read_string(&mut self) -> Option<Result<TokenKind>> {
        let start_pos = self.current_pos + 1;
        while self.peek() != '"' && self.peek() != '\0' {
            self.next_char();
        }

        if self.peek() != '"' {
            let msg = Err(anyhow::Error::msg(format!(
                "[line: {}] Error: Unterminated string.",
                self.line
            )));

            return Some(msg);
        }

        if start_pos == self.current_pos + 1 {
            let str: Rc<str> = self.input[start_pos..start_pos].into();
            return Some(Ok(TokenKind {
                token: Token::String(str),
                line: self.line,
            }));
        }

        let str: Rc<str> = self.input[start_pos..=self.current_pos].into();
        return Some(Ok(TokenKind {
            token: Token::String(str),
            line: self.line,
        }));
    }

    fn read_number(&mut self) -> Rc<str> {
        let start_pos = self.current_pos;

        while self.peek().is_ascii_digit() {
            self.next_char();
        }

        if self.peek() == '.' {
            self.next_char();
            if self.peek().is_ascii_digit() {
                while self.peek().is_ascii_digit() {
                    self.next_char();
                }
            }

            return self.input[start_pos..=self.current_pos].into();
        }

        return self.input[start_pos..=self.current_pos].into();
    }

    fn read_ident(&self, literal: &str) -> Token {
        return match literal {
            "fn" => Token::Fn,
            "return" => Token::Return,
            "let" => Token::Let,
            "and" => Token::And,
            "or" => Token::Or,
            "if" => Token::If,
            "else" => Token::Else,
            "true" => Token::True,
            "false" => Token::False,
            "nil" => Token::Nil,
            "print" => Token::Print,
            "len" => Token::Len,
            "push" => Token::Push,
            "first" => Token::First,
            "last" => Token::Last,
            "rest" => Token::Rest,
            _ => Token::Ident(literal.into()),
        };
    }
}

impl Iterator for Lexer {
    type Item = Result<TokenKind>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_pos > self.input.len() {
            return None;
        }

        self.skip_whitespace();

        let token = match self.char {
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '[' => Token::LBracket,
            ']' => Token::RBracket,

            '.' => Token::Dot,
            ',' => Token::Comma,
            '-' => Token::Minus,
            '*' => Token::Star,
            ';' => Token::Semicolon,
            ':' => Token::Colon,
            '+' => Token::Plus,
            '/' => Token::Slash,
            '!' => self.is_post_equal(Token::BangEqual, Token::Bang),
            '=' => self.is_post_equal(Token::AssignEqual, Token::Assign),
            '>' => self.is_post_equal(Token::GreaterEqual, Token::Greater),
            '<' => self.is_post_equal(Token::LessEqual, Token::Less),

            '"' => {
                let item = self.read_string();
                self.next_char(); // consume '"'
                self.next_char(); // consume the next after '"'
                return item;
            }

            '0'..='9' => {
                let literal = self.read_number();
                let number = literal.parse::<f64>().unwrap();

                Token::Number(literal, number)
            }
            'a'..='z' | '_' | 'A'..='Z' => {
                let start_pos = self.current_pos;
                while self.peek().is_alphanumeric() || self.peek() == '_' {
                    self.next_char();
                }
                let tok = self.read_ident(&self.input[start_pos..=self.current_pos]);

                tok
            }

            '\0' => Token::EOF,

            _ => {
                let error = Some(Err(anyhow!(
                    "[line: {}] Error: Unexpected character: {}",
                    self.line,
                    self.char
                )));
                self.next_char();
                return error;
            }
        };

        self.next_char();

        return Some(Ok(TokenKind {
            token,
            line: self.line,
        }));
    }
}

#[cfg(test)]
mod test {

    use anyhow::Result;

    use super::{Lexer, Token};

    #[test]
    fn test_numbers() -> Result<()> {
        let input = r#"123
        123.456
        .456
        123."#
            .to_string();

        let expected = vec![
            Token::Number("123".into(), 123.0),
            Token::Number("123.456".into(), 123.456),
            Token::Dot,
            Token::Number("456".into(), 456.0),
            Token::Number("123.".into(), 123.0),
            Token::EOF,
        ];

        let lexer = Lexer::new(input);

        for (i, tok_result) in lexer.enumerate() {
            let tok = match tok_result {
                Ok(tok) => tok,
                Err(_) => panic!("does not expected to error"),
            };

            assert_eq!(
                expected[i], tok.token,
                "expected: {:?}, got: {:?}",
                expected[i], tok
            );
        }

        Ok(())
    }

    #[test]
    fn test_string() -> Result<()> {
        let input = r#" ""
        "string" "#
            .to_string();

        let expected = vec![
            Token::String("".into()),
            Token::String("string".into()),
            Token::EOF,
        ];

        let lexer = Lexer::new(input);

        for (i, tok_result) in lexer.enumerate() {
            let tok = match tok_result {
                Ok(tok) => tok,
                Err(_) => panic!("does not expected to error"),
            };

            assert_eq!(
                expected[i], tok.token,
                "expected: {:?}, got: {:?}",
                expected[i], tok
            );
        }

        Ok(())
    }

    #[test]
    fn test_keyword() -> Result<()> {
        let input = r#"and else false fn if nil or return true let"#.to_string();

        let expected = vec![
            Token::And,
            Token::Else,
            Token::False,
            Token::Fn,
            Token::If,
            Token::Nil,
            Token::Or,
            Token::Return,
            Token::True,
            Token::Let,
            Token::EOF,
        ];

        let lexer = Lexer::new(input);

        for (i, tok_result) in lexer.enumerate() {
            let tok = match tok_result {
                Ok(tok) => tok,
                Err(_) => panic!("does not expected to error"),
            };

            assert_eq!(
                expected[i], tok.token,
                "expected: {:?}, got: {:?}",
                expected[i], tok
            );
        }

        Ok(())
    }

    #[test]
    fn test_whitespace() -> Result<()> {
        let input = r#"space    tabs				newlines




        end"#
            .to_string();

        let expected = vec![
            Token::Ident("space".into()),
            Token::Ident("tabs".into()),
            Token::Ident("newlines".into()),
            Token::Ident("end".into()),
            Token::EOF,
        ];

        let lexer = Lexer::new(input);

        for (i, tok_result) in lexer.enumerate() {
            let tok = match tok_result {
                Ok(tok) => tok,
                Err(_) => panic!("does not expected to error"),
            };

            assert_eq!(
                expected[i], tok.token,
                "expected: {:?}, got: {:?}",
                expected[i], tok
            );
        }

        Ok(())
    }

    #[test]
    fn test_punctuation() -> Result<()> {
        let input = "(){};,+-*!===<=>=!=<>/.".to_string();

        let expected = vec![
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Semicolon,
            Token::Comma,
            Token::Plus,
            Token::Minus,
            Token::Star,
            Token::BangEqual,
            Token::AssignEqual,
            Token::LessEqual,
            Token::GreaterEqual,
            Token::BangEqual,
            Token::Less,
            Token::Greater,
            Token::Slash,
            Token::Dot,
            Token::EOF,
        ];

        let lexer = Lexer::new(input);

        for (i, tok_result) in lexer.enumerate() {
            let tok = match tok_result {
                Ok(tok) => tok,
                Err(err) => panic!("does not expected to error: {}", err),
            };

            assert_eq!(
                expected[i], tok.token,
                "expected: {:?}, got: {:?}",
                expected[i], tok
            );
        }

        Ok(())
    }
}
