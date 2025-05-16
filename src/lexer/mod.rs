#![allow(dead_code)]

use tokens::{Token, TokenKind};

pub mod tokens;

pub struct Lexer {
    content: String,
    position: usize,
}

impl Lexer {
    pub fn new(program: &str) -> Self {
        Lexer {
            content: program.to_string(),
            position: 0,
        }
    }

    pub fn next_token(&mut self) -> Token {
        use tokens::TokenKind::*;

        self.skip_whitespace();

        let Some(cur_char) = self.peek(0) else {
            return Token::new(Eof, &self.content, self.position, 1);
        };

        match cur_char {
            // single-char tokens
            '#' => {
                self.advance(1);
                while let Some(c) = self.peek(0) {
                    if c == '\n' {
                        break;
                    }
                    self.advance(1);
                }
                self.next_token()
            }
            '!' => match self.peek(1) {
                Some('=') => self.make_char_token(NotEqual, 2),
                _ => self.make_char_token(Not, 1),
            },
            '+' => self.make_char_token(Plus, 1),
            '-' => self.make_char_token(Minus, 1),
            '/' => self.make_char_token(Slash, 1),
            '*' => self.make_char_token(Asterisk, 1),
            '^' => self.make_char_token(Exponent, 1),
            '=' => match self.peek(1) {
                Some('=') => self.make_char_token(DoubleEqual, 2),
                _ => self.make_char_token(Equal, 1),
            },
            '<' => match self.peek(1) {
                Some('=') => self.make_char_token(LessThanOrEqual, 2),
                _ => self.make_char_token(LessThan, 1),
            },
            '>' => match self.peek(1) {
                Some('=') => self.make_char_token(GreaterThanOrEqual, 2),
                _ => self.make_char_token(GreaterThan, 1),
            },
            '(' => self.make_char_token(LParen, 1),
            ')' => self.make_char_token(RParen, 1),
            '{' => self.make_char_token(LBrace, 1),
            '}' => self.make_char_token(RBrace, 1),
            '~' => self.make_char_token(Tilde, 1),
            ':' => self.make_char_token(Colon, 1),
            ',' => self.make_char_token(Comma, 1),
            ';' => self.make_char_token(SemiColon, 1),
            'a'..='z' | 'A'..='Z' | '_' => {
                let initial_position = self.position;
                let ident = self.read_ident();

                match ident.as_str() {
                    "fun" => Token::new(Fun, &self.content, initial_position, 3),
                    "unit" => Token::new(Unit, &self.content, initial_position, 4),
                    "true" => Token::new(True, &self.content, initial_position, 4),
                    "false" => Token::new(False, &self.content, initial_position, 5),
                    s => Token::new(
                        Identifier(s.to_string()),
                        &self.content,
                        initial_position,
                        s.len(),
                    ),
                }
            }
            '0'..='9' => {
                let initial_position = self.position;
                let int = self.read_int();
                Token::new(
                    IntLiteral(int),
                    &self.content,
                    initial_position,
                    self.position - initial_position + 1,
                )
            }
            c => panic!("illegal token: {c}"),
        }
    }

    fn peek(&self, distance: usize) -> Option<char> {
        self.content.chars().nth(self.position + distance)
    }

    pub fn next(&mut self) -> Option<char> {
        let char = self.peek(0);
        self.position += 1;
        char
    }

    fn advance(&mut self, distance: usize) {
        self.position += distance;
    }

    fn make_char_token(&mut self, kind: TokenKind, size: usize) -> Token {
        let tok = Token::new(kind, &self.content, self.position, size);
        self.advance(size);
        tok
    }

    fn read_ident(&mut self) -> String {
        let initial_position = self.position;
        self.advance(1);

        while matches!(self.peek(0), Some('a'..='z' | 'A'..='Z' | '_' | '0'..='9')) {
            self.advance(1);
        }

        self.content[initial_position..self.position].to_string()
    }

    fn read_int(&mut self) -> isize {
        let initial_position = self.position;
        self.advance(1);

        while matches!(self.peek(0), Some('0'..='9')) {
            self.advance(1);
        }

        self.content[initial_position..self.position]
            .to_string()
            .parse::<isize>()
            .unwrap()
    }

    fn skip_whitespace(&mut self) {
        while matches!(self.peek(0), Some(' ' | '\t' | '\r' | '\n')) {
            self.advance(1);
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_token();
        if token.kind == TokenKind::Eof {
            None
        } else {
            Some(token)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use tokens::{TokenKind, TokenKind::*};

    fn expect_tok(lexer: &mut Lexer, expected: Vec<TokenKind>) {
        let token_kinds = lexer
            .into_iter()
            .map(|t| t.kind.clone())
            .collect::<Vec<_>>();
        assert_eq!(token_kinds, expected);
    }

    #[test]
    fn all_syntax() {
        let input = r#"
            foo :: 4;
            bar : Int = 33;

            unit;

            calc :: fun (~x, ~y: Int) Int {
             z :: x / y;
             true;
             false;
             z^2
            };

            calc(foo, bar);
        "#;
        let mut lexer = Lexer::new(input);
        expect_tok(
            &mut lexer,
            vec![
                Identifier("foo".to_string()),
                Colon,
                Colon,
                IntLiteral(4),
                SemiColon,
                Identifier("bar".to_string()),
                Colon,
                Identifier("Int".to_string()),
                Equal,
                IntLiteral(33),
                SemiColon,
                Unit,
                SemiColon,
                Identifier("calc".to_string()),
                Colon,
                Colon,
                Fun,
                LParen,
                Tilde,
                Identifier("x".to_string()),
                Comma,
                Tilde,
                Identifier("y".to_string()),
                Colon,
                Identifier("Int".to_string()),
                RParen,
                Identifier("Int".to_string()),
                LBrace,
                Identifier("z".to_string()),
                Colon,
                Colon,
                Identifier("x".to_string()),
                Slash,
                Identifier("y".to_string()),
                SemiColon,
                True,
                SemiColon,
                False,
                SemiColon,
                Identifier("z".to_string()),
                Exponent,
                IntLiteral(2),
                RBrace,
                SemiColon,
                Identifier("calc".to_string()),
                LParen,
                Identifier("foo".to_string()),
                Comma,
                Identifier("bar".to_string()),
                RParen,
                SemiColon,
            ],
        );
    }
}
