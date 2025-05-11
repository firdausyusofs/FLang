#![allow(dead_code)]

use ast::{Expression, InfixOperator, Precedence, Program, Statement};
use lexer::tokens::{
    Token,
    TokenKind::{self, *},
};

mod ast;

type PrefixParseFn = fn(&mut Parser) -> Result<Expression, Error>;

#[derive(Debug, PartialEq, Eq)]
enum Error {
    NoToken,
    NoPrefixParseFn(TokenKind),
    ExpectedToken(String),
    SyntaxError(String),
}

impl Error {
    pub fn expected(s: &str) -> Self {
        Self::ExpectedToken(s.to_string())
    }

    pub fn syntax_error(s: &str) -> Self {
        Self::SyntaxError(format!("Syntax error: {}", s))
    }
}

struct Parser {
    tokens: Vec<Token>,
    position: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            position: 0,
        }
    }

    pub fn parse(&mut self) -> Result<Program, Error> {
        let stmt = self.parse_statement()?;

        Ok(Program(vec![stmt]))
    }

    fn parse_statement(&mut self) -> Result<Statement, Error> {
        match self.cur_token() {
            Fun => todo!(),
            _ => self.parse_expr_statement(),
        }
    }

    fn parse_expr_statement(&mut self) -> Result<Statement, Error> {
        Ok(Statement::Expression(self.parse_expr(Precedence::Lowest)?))
    }

    fn parse_expr(&mut self, precedence: Precedence) -> Result<Expression, Error> {
        let mut expr = self.prefix_parse_fn()?(self)?;

        while self.cur_precedence()? > precedence {
            match self.cur_token() {
                IntLiteral(_) => return Err(Error::syntax_error("consecutive int literals")),
                Eof => return Ok(expr),
                Plus => {
                    expr = self.parse_infix_expr(expr, InfixOperator::Plus, Precedence::Sum)?;
                }
                Minus => {
                    expr = self.parse_infix_expr(expr, InfixOperator::Minus, Precedence::Sum)?;
                }
                Slash => {
                    expr =
                        self.parse_infix_expr(expr, InfixOperator::Divide, Precedence::Product)?;
                }
                Asterisk => {
                    expr =
                        self.parse_infix_expr(expr, InfixOperator::Multiply, Precedence::Product)?;
                }
                tok => return Err(Error::syntax_error(&format!("invalid operator: {}", tok))),
            }
        }

        Ok(expr)
    }

    fn prefix_parse_fn(&mut self) -> Result<PrefixParseFn, Error> {
        let cur_token = self.cur_token();
        match cur_token {
            IntLiteral(_) => Ok(Self::parse_int_literal),
            LParen => Ok(Self::parse_grouped_expr),
            _ => Err(Error::NoPrefixParseFn(cur_token.clone())),
        }
    }

    fn parse_infix_expr(
        &mut self,
        lhs: Expression,
        operator: InfixOperator,
        precedence: Precedence,
    ) -> Result<Expression, Error> {
        self.advance();
        let rhs = self.parse_expr(precedence)?;
        Ok(Expression::InfixExpr {
            left: Box::new(lhs),
            operator,
            right: Box::new(rhs),
        })
    }

    fn parse_int_literal(&mut self) -> Result<Expression, Error> {
        Ok(Expression::IntLiteral(self.expect_int()?))
    }

    fn parse_grouped_expr(&mut self) -> Result<Expression, Error> {
        self.advance(); // consume LParen
        let expr = self.parse_expr(Precedence::Lowest)?;
        match self.cur_token() {
            RParen => {
                self.advance(); // consume RParen
                Ok(expr)
            }
            _ => Err(Error::syntax_error("syntax error: expected )")),
        }
    }

    fn expect_int(&mut self) -> Result<isize, Error> {
        if let IntLiteral(int) = self.cur_token() {
            self.advance();
            Ok(int)
        } else {
            Err(Error::expected("int literal"))
        }
    }

    fn cur_precedence(&self) -> Result<Precedence, Error> {
        Ok(match self.cur_token() {
            Plus | Minus => Precedence::Sum,
            Slash | Asterisk => Precedence::Product,
            LParen => Precedence::Group,
            _ => Precedence::Lowest,
        })
    }

    fn cur_token(&self) -> TokenKind {
        self.tokens
            .get(self.position)
            .map_or(Eof, |t| t.kind.clone())
    }

    fn advance(&mut self) {
        self.position += 1;
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        Parser,
        ast::{Expression, InfixOperator, Program, Statement},
    };
    use lexer::{Lexer, tokens::Token};
    use pretty_assertions::assert_eq;

    fn expect_ast(input: &str, ast: Program) {
        let lexer = Lexer::new(input);
        let tokens: Vec<Token> = lexer.collect();
        let mut parser = Parser::new(tokens);
        let parsed = parser.parse();
        assert_eq!(parsed, Ok(ast));
    }

    #[test]
    fn single_int() {
        expect_ast(
            "122",
            Program(vec![Statement::Expression(Expression::IntLiteral(122))]),
        )
    }

    #[test]
    fn bacis_exprs() {
        expect_ast(
            "1 + (2 * 3)",
            Program(vec![Statement::Expression(Expression::InfixExpr {
                left: Box::new(Expression::IntLiteral(1)),
                operator: InfixOperator::Plus,
                right: Box::new(Expression::InfixExpr {
                    left: Box::new(Expression::IntLiteral(2)),
                    operator: InfixOperator::Multiply,
                    right: Box::new(Expression::IntLiteral(3)),
                }),
            })]),
        );
    }
}
