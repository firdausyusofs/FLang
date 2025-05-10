use crate::span::Span;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    // keywords:
    Val, // val
    Mut, // mut
    Fun, // fun

    // syntax:
    LArrow,  // <-
    Colon,   // :
    LParen,  // (
    RParen,  // )
    LBrace,  // {
    RBrace,  // }
    Comma,   // ,
    RArrow,  // ->
    Newline, // \n

    // operators:
    Not,                // !
    Plus,               // +
    Minus,              // -
    Divide,             // /
    Multiply,           // *
    Exponent,           // ^
    Equal,              // ==
    NotEqual,           // !=
    LessThan,           // <
    GreaterThan,        // >
    LessThanOrEqual,    // <=
    GreaterThanOrEqual, // >=

    // idents
    Identifier(String), // variable/type names

    IntLiteral(usize),

    Eof,
}

pub struct Token {
    pub kind: TokenType,
    pub position: Span,
}

impl Token {
    pub fn new(kind: TokenType, start: usize, size: usize) -> Self {
        Token {
            kind,
            position: Span {
                start,
                end: start + size - 1,
            },
        }
    }
}
