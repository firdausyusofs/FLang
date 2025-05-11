#![allow(dead_code)]

use ast::{Expression, ExpressionStatement, InfixOperator, Precedence, Program, Type};
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
        let mut stmts = vec![];
        while self.cur_token() != Eof {
            stmts.push(self.parse_expr_statement()?);
        }

        Ok(Program(stmts))
    }

    fn parse_expr_statement(&mut self) -> Result<ExpressionStatement, Error> {
        let expr = self.parse_expr(Precedence::Lowest)?;
        let has_semicolon = self.check_semicolon();

        if has_semicolon {
            self.advance()
        }

        Ok(ExpressionStatement {
            expr,
            discarded: has_semicolon,
        })
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
            Identifier(_) => match self.peek_token() {
                Colon => Ok(Self::parse_variable_decl),
                _ => Ok(Self::parse_variable_ident),
            },
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

    fn parse_variable_ident(&mut self) -> Result<Expression, Error> {
        Ok(Expression::Ident(self.expect_ident()?))
    }

    fn parse_variable_decl(&mut self) -> Result<Expression, Error> {
        let name = self.expect_ident()?;
        self.expect_tok(Colon)?;

        let mut type_ = None;
        if let Identifier(name) = self.cur_token() {
            self.advance();
            type_ = Some(Type::Ident(name));
        }

        #[allow(clippy::needless_late_init)]
        let value: Expression;
        let mutable: bool;

        match self.cur_token() {
            Colon => {
                self.expect_tok(Colon)?;
                mutable = false;
                value = self.parse_expr(Precedence::Lowest)?;
            } // const
            Equal => {
                self.expect_tok(Equal)?;
                mutable = true;
                value = self.parse_expr(Precedence::Lowest)?;
            } // mutable
            _ => return Err(Error::expected(": or =")),
        }

        Ok(Expression::VariableDecl {
            name,
            value: Box::new(value),
            mutable,
            type_,
        })
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

    fn check_semicolon(&mut self) -> bool {
        self.cur_token() == SemiColon
    }

    fn expect_int(&mut self) -> Result<isize, Error> {
        if let IntLiteral(int) = self.cur_token() {
            self.advance();
            Ok(int)
        } else {
            Err(Error::expected("int literal"))
        }
    }

    fn expect_tok(&mut self, kind: TokenKind) -> Result<(), Error> {
        let tok = self.cur_token();
        if tok != kind {
            Err(Error::expected(&format!("expected token: {}", kind)))
        } else {
            self.advance();
            Ok(())
        }
    }

    fn expect_ident(&mut self) -> Result<String, Error> {
        if let Identifier(ident) = self.cur_token() {
            self.advance();
            Ok(ident)
        } else {
            Err(Error::expected("identifier"))
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

    fn peek_token(&self) -> TokenKind {
        self.tokens
            .get(self.position + 1)
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
        ast::{Expression, ExpressionStatement, InfixOperator, Program, Type},
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
    fn single_value() {
        expect_ast(
            "122",
            Program(vec![ExpressionStatement {
                expr: Expression::IntLiteral(122),
                discarded: false,
            }]),
        );

        expect_ast(
            "122;",
            Program(vec![ExpressionStatement {
                expr: Expression::IntLiteral(122),
                discarded: true,
            }]),
        );

        expect_ast(
            "foo;",
            Program(vec![ExpressionStatement {
                expr: Expression::Ident("foo".to_string()),
                discarded: true,
            }]),
        );
    }

    #[test]
    fn bacis_exprs() {
        expect_ast(
            "1 + (2 * 3)",
            Program(vec![ExpressionStatement {
                expr: Expression::InfixExpr {
                    left: Box::new(Expression::IntLiteral(1)),
                    operator: InfixOperator::Plus,
                    right: Box::new(Expression::InfixExpr {
                        left: Box::new(Expression::IntLiteral(2)),
                        operator: InfixOperator::Multiply,
                        right: Box::new(Expression::IntLiteral(3)),
                    }),
                },
                discarded: false,
            }]),
        );

        expect_ast(
            "6 - 3 - 2",
            Program(vec![ExpressionStatement {
                expr: Expression::InfixExpr {
                    left: Box::new(Expression::InfixExpr {
                        left: Box::new(Expression::IntLiteral(6)),
                        operator: InfixOperator::Minus,
                        right: Box::new(Expression::IntLiteral(3)),
                    }),
                    operator: InfixOperator::Minus,
                    right: Box::new(Expression::IntLiteral(2)),
                },
                discarded: false,
            }]),
        );

        expect_ast(
            "(1 - (3 + 2)) * (122 - 9);",
            Program(vec![ExpressionStatement {
                expr: Expression::InfixExpr {
                    left: Box::new(Expression::InfixExpr {
                        left: Box::new(Expression::IntLiteral(1)),
                        operator: InfixOperator::Minus,
                        right: Box::new(Expression::InfixExpr {
                            left: Box::new(Expression::IntLiteral(3)),
                            operator: InfixOperator::Plus,
                            right: Box::new(Expression::IntLiteral(2)),
                        }),
                    }),
                    operator: InfixOperator::Multiply,
                    right: Box::new(Expression::InfixExpr {
                        left: Box::new(Expression::IntLiteral(122)),
                        operator: InfixOperator::Minus,
                        right: Box::new(Expression::IntLiteral(9)),
                    }),
                },
                discarded: true,
            }]),
        );
    }

    #[test]
    fn variable_decls() {
        expect_ast(
            "foo :: 1606",
            Program(vec![ExpressionStatement {
                expr: Expression::VariableDecl {
                    name: "foo".to_string(),
                    value: Box::new(Expression::IntLiteral(1606)),
                    mutable: false,
                    type_: None,
                },
                discarded: false,
            }]),
        );

        expect_ast(
            "bar := 1606;",
            Program(vec![ExpressionStatement {
                expr: Expression::VariableDecl {
                    name: "bar".to_string(),
                    value: Box::new(Expression::IntLiteral(1606)),
                    mutable: true,
                    type_: None,
                },
                discarded: true,
            }]),
        );

        expect_ast(
            "typed_const : Int : 1606",
            Program(vec![ExpressionStatement {
                expr: Expression::VariableDecl {
                    name: "typed_const".to_string(),
                    value: Box::new(Expression::IntLiteral(1606)),
                    mutable: false,
                    type_: Some(Type::Ident("Int".to_string())),
                },
                discarded: false,
            }]),
        );

        expect_ast(
            "typed_mut : Int = 1606;",
            Program(vec![ExpressionStatement {
                expr: Expression::VariableDecl {
                    name: "typed_mut".to_string(),
                    value: Box::new(Expression::IntLiteral(1606)),
                    mutable: true,
                    type_: Some(Type::Ident("Int".to_string())),
                },
                discarded: true,
            }]),
        );
    }

    #[test]
    fn multiple_expr_statements() {
        expect_ast(
            "foo :: 3; foo - 4",
            Program(vec![
                ExpressionStatement {
                    expr: Expression::VariableDecl {
                        name: "foo".to_string(),
                        value: Box::new(Expression::IntLiteral(3)),
                        mutable: false,
                        type_: None,
                    },
                    discarded: true,
                },
                ExpressionStatement {
                    expr: Expression::InfixExpr {
                        left: Box::new(Expression::Ident("foo".to_string())),
                        operator: InfixOperator::Minus,
                        right: Box::new(Expression::IntLiteral(4)),
                    },
                    discarded: false,
                },
            ]),
        );
    }
}
