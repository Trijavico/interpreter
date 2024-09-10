use anyhow::Result;
use core::fmt;

#[derive(Debug, PartialEq)]
pub enum Token<'de> {
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

    Bang,
    BangEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Assign,
    AssignEqual,

    Ident(&'de str),
    String(&'de str),
    Number(&'de str, f64),

    And,
    Print,
    Else,
    Return,
    True,
    Let,
    Or,
    Fn,
    False,
    If,
    Nil,

    EOF,
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Token::LBrace => "LEFT_BRACE { null",
                Token::RBrace => "RIGHT_BRACE } null",
                Token::LParen => "LEFT_PAREN ( null",
                Token::RParen => "RIGHT_PAREN ) null",
                Token::Dot => "DOT . null",
                Token::Minus => "MINUS - null",
                Token::Plus => "PLUS + null",
                Token::Star => "STAR * null",
                Token::Semicolon => "SEMICOLON ; null",
                Token::Colon => "COLON : null",
                Token::Comma => "COMMA , null",
                Token::Bang => "BANG ! null",
                Token::BangEqual => "BANG_EQUAL != null",
                Token::Assign => "EQUAL = null",
                Token::AssignEqual => "EQUAL_EQUAL == null",
                Token::Greater => "GREATER > null",
                Token::GreaterEqual => "GREATER_EQUAL >= null",
                Token::Less => "LESS < null",
                Token::LessEqual => "LESS_EQUAL <= null",
                Token::Slash => "SLASH / null",

                Token::And => "AND and null",
                Token::Or => "OR or null",
                Token::False => "FALSE false null",
                Token::True => "TRUE true null",
                Token::If => "IF if null",
                Token::Else => "ELSE else null",
                Token::Print => "PRINT print null",
                Token::Let => "LET let null",
                Token::Fn => "FN fun null",
                Token::Nil => "NIL nil null",
                Token::Return => "RETURN return null",

                Token::Ident(val) => return write!(f, "IDENTIFIER {val} null"),
                Token::String(val) => return write!(f, "STRING \"{val}\" {val}"),
                Token::Number(lit, num) => {
                    if *num == num.trunc() {
                        return write!(f, "NUMBER {lit} {:.1}", num);
                    } else {
                        return write!(f, "NUMBER {lit} {num}");
                    }
                }

                Token::EOF => "EOF  null",
            }
        )
    }
}

pub struct Lexer<'de> {
    input: &'de str,
    char: char,
    line: usize,
    current_pos: usize,
    next_pos: usize,
}

impl<'de> Lexer<'de> {
    pub fn new(input: &'de str) -> Self {
        let input = if input.ends_with('\0') {
            &input[..input.len() - 1]
        } else {
            input
        };

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

            if self.char == '\n' {
                self.line += 1;
            }
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

    fn is_post_equal(&mut self, yes: Token<'de>, no: Token<'de>) -> Token<'de> {
        if self.peek() == '=' {
            self.next_char();

            return yes;
        }

        return no;
    }

    fn read_string(&mut self) -> Option<Result<Token<'de>>> {
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
            let str = &self.input[start_pos..start_pos];
            return Some(Ok(Token::String(str)));
        }

        let str = &self.input[start_pos..=self.current_pos];
        return Some(Ok(Token::String(str)));
    }

    fn read_number(&mut self) -> &'de str {
        let start_pos = self.current_pos;

        while self.peek().is_ascii_digit() && self.peek() != '\0' {
            self.next_char();
        }

        if self.peek() == '.' {
            self.next_char();
            if self.peek().is_ascii_digit() {
                while self.peek().is_ascii_digit() && self.peek() != '\0' {
                    self.next_char();
                }
            }

            return &self.input[start_pos..=self.current_pos];
        }

        return &self.input[start_pos..=self.current_pos];
    }

    fn read_ident(&self, literal: &'de str) -> Token<'de> {
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
            _ => Token::Ident(literal),
        };
    }
}

impl<'de> Iterator for Lexer<'de> {
    type Item = Result<Token<'de>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_pos > self.input.len() {
            return None;
        }

        self.skip_whitespace();

        let tok = match self.char {
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '.' => Token::Dot,
            ',' => Token::Comma,
            '-' => Token::Minus,
            '*' => Token::Star,
            ';' => Token::Semicolon,
            ':' => Token::Colon,
            '+' => Token::Plus,
            '/' => Token::Slash,
            '"' => {
                let item = self.read_string();
                self.next_char(); // consume '"'
                self.next_char(); // consume the next after '"'
                return item;
            }

            '!' => self.is_post_equal(Token::BangEqual, Token::Bang),
            '=' => self.is_post_equal(Token::AssignEqual, Token::Assign),
            '>' => self.is_post_equal(Token::GreaterEqual, Token::Greater),
            '<' => self.is_post_equal(Token::LessEqual, Token::Less),

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
                return Some(Err(anyhow::Error::msg(format!(
                    "[line: {}] Error: Unexpected character: {}",
                    self.line, self.char
                ))))
            }
        };

        self.next_char();

        return Some(Ok(tok));
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
        123."#;

        let expected = vec![
            Token::Number("123", 123.0),
            Token::Number("123.456", 123.456),
            Token::Dot,
            Token::Number("456", 456.0),
            Token::Number("123.", 123.0),
            Token::EOF,
        ];

        let lexer = Lexer::new(input);

        for (i, tok_result) in lexer.enumerate() {
            let tok = match tok_result {
                Ok(tok) => tok,
                Err(_) => panic!("does not expected to error"),
            };

            assert_eq!(
                expected[i], tok,
                "expected: {:?}, got: {:?}",
                expected[i], tok
            );
        }

        Ok(())
    }

    #[test]
    fn test_string() -> Result<()> {
        let input = r#" ""
        "string" "#;

        let expected = vec![Token::String(""), Token::String("string"), Token::EOF];

        let lexer = Lexer::new(input);

        for (i, tok_result) in lexer.enumerate() {
            let tok = match tok_result {
                Ok(tok) => tok,
                Err(_) => panic!("does not expected to error"),
            };

            assert_eq!(
                expected[i], tok,
                "expected: {:?}, got: {:?}",
                expected[i], tok
            );
        }

        Ok(())
    }

    #[test]
    fn test_keyword() -> Result<()> {
        let input = r#"and else false fn if nil or return true let"#;

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
                expected[i], tok,
                "expected: {:?}, got: {:?}",
                expected[i], tok
            );
        }

        Ok(())
    }

    #[test]
    fn test_whitespace() -> Result<()> {
        let input = r#"space    tabs				newlines




        end"#;

        let expected = vec![
            Token::Ident("space"),
            Token::Ident("tabs"),
            Token::Ident("newlines"),
            Token::Ident("end"),
            Token::EOF,
        ];

        let lexer = Lexer::new(input);

        for (i, tok_result) in lexer.enumerate() {
            let tok = match tok_result {
                Ok(tok) => tok,
                Err(_) => panic!("does not expected to error"),
            };

            assert_eq!(
                expected[i], tok,
                "expected: {:?}, got: {:?}",
                expected[i], tok
            );
        }

        Ok(())
    }

    #[test]
    fn test_punctuation() -> Result<()> {
        let input = "(){};,+-*!===<=>=!=<>/.";

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
                expected[i], tok,
                "expected: {:?}, got: {:?}",
                expected[i], tok
            );
        }

        Ok(())
    }
}
