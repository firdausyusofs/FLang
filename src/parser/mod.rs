#![allow(dead_code)]

use crate::lexer::tokens::{
    Token,
    TokenKind::{self, *},
};
use ast::{
    Argument, Expression, ExpressionStatement, FunctionParam, InfixOperator, Precedence, Program,
    Type,
};

pub mod ast;

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
            True | False => Ok(Self::parse_bool_literal),
            Unit => Ok(Self::parse_unit),
            LParen => Ok(Self::parse_grouped_expr),
            Identifier(_) => match self.peek_token() {
                Colon => Ok(Self::parse_variable_decl),
                LParen => Ok(Self::parse_function_call),
                _ => Ok(Self::parse_variable_ident),
            },
            Fun => Ok(Self::parse_function_expr),
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

    fn parse_bool_literal(&mut self) -> Result<Expression, Error> {
        match self.cur_token() {
            True => {
                self.expect_tok(True)?;
                Ok(Expression::BoolLiteral(true))
            }
            False => {
                self.expect_tok(False)?;
                Ok(Expression::BoolLiteral(false))
            }
            _ => Err(Error::expected("true or false")),
        }
    }

    fn parse_unit(&mut self) -> Result<Expression, Error> {
        self.expect_tok(Unit)?;
        Ok(Expression::Unit)
    }

    fn parse_variable_ident(&mut self) -> Result<Expression, Error> {
        Ok(Expression::Ident(self.expect_ident()?))
    }

    fn parse_type(&mut self) -> Result<Type, Error> {
        Ok(Type::Ident(self.expect_ident()?))
    }

    fn parse_function_expr(&mut self) -> Result<Expression, Error> {
        self.expect_tok(Fun)?;
        self.expect_tok(LParen)?;

        let mut params = vec![];

        while self.cur_token() != RParen {
            let param = self.parse_function_param()?;
            params.push(param);
            if self.cur_token() == Comma {
                self.expect_tok(Comma)?;
                continue;
            }

            break;
        }

        self.expect_tok(RParen)?;

        let mut return_type = None;

        if let Identifier(_) = self.cur_token() {
            return_type = Some(self.parse_type()?);
        }

        self.expect_tok(LBrace)?;

        let mut body = vec![];

        while self.cur_token() != RBrace {
            let stmt = self.parse_expr_statement()?;
            body.push(stmt);
        }

        self.expect_tok(RBrace)?;

        Ok(Expression::Function {
            params,
            return_type,
            body,
        })
    }

    fn parse_function_call(&mut self) -> Result<Expression, Error> {
        let name = self.expect_ident()?;
        self.expect_tok(LParen)?;

        let mut args = vec![];

        while self.cur_token() != RParen {
            let arg = self.parse_function_arg()?;
            args.push(arg);
            if self.cur_token() == Comma {
                self.expect_tok(Comma)?;
                continue;
            }

            break;
        }

        self.expect_tok(RParen)?;

        Ok(Expression::FunctionCall { name, args })
    }

    fn parse_function_arg(&mut self) -> Result<Argument, Error> {
        let mut label = None;
        let value: Expression;

        if let Identifier(name) = self.cur_token() {
            self.expect_ident()?;
            if self.cur_token() == Colon {
                self.expect_tok(Colon)?;
                label = Some(name);
                value = self.parse_expr(Precedence::Lowest)?;
            } else {
                value = Expression::Ident(name);
            }
        } else {
            value = self.parse_expr(Precedence::Lowest)?;
        }

        Ok(Argument { label, value })
    }

    fn parse_variable_decl(&mut self) -> Result<Expression, Error> {
        let name = self.expect_ident()?;
        self.expect_tok(Colon)?;

        let mut type_ = None;
        if let Identifier(_) = self.cur_token() {
            type_ = Some(self.parse_type()?);
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

    fn parse_function_param(&mut self) -> Result<FunctionParam, Error> {
        match self.cur_token() {
            Tilde => {
                self.expect_tok(Tilde)?;
                let name = self.expect_ident()?;
                self.expect_tok(Colon)?;
                let type_ = self.parse_type()?;

                Ok(FunctionParam::UnlabeledAtCallsite { name, type_ })
            }
            Identifier(_) => {
                let first = self.expect_ident()?;
                let mut second = None;
                if let Identifier(name) = self.cur_token() {
                    self.expect_ident()?;
                    second = Some(name);
                }
                self.expect_tok(Colon)?;
                let type_ = self.parse_type()?;

                Ok(FunctionParam::LabeledAtCallsite {
                    name: if let Some(ref second_ident) = second {
                        second_ident.to_string()
                    } else {
                        first.clone()
                    },
                    label: if second.is_some() { Some(first) } else { None },
                    type_,
                })
            }
            _ => Err(Error::expected("parameter name")),
        }
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
            Err(Error::syntax_error(&format!("unexpected token: {}", tok)))
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
        lexer::{Lexer, tokens::Token},
        parser::{
            Parser,
            ast::{
                Argument, Expression, ExpressionStatement, FunctionParam, InfixOperator, Program,
                Type,
            },
        },
    };
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

        expect_ast(
            "unit",
            Program(vec![ExpressionStatement {
                expr: Expression::Unit,
                discarded: false,
            }]),
        );

        expect_ast(
            "true; false",
            Program(vec![
                ExpressionStatement {
                    expr: Expression::BoolLiteral(true),
                    discarded: true,
                },
                ExpressionStatement {
                    expr: Expression::BoolLiteral(false),
                    discarded: false,
                },
            ]),
        );
    }

    #[test]
    fn math_exprs() {
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

    #[test]
    fn function_exprs() {
        expect_ast(
            "fun () { unit }",
            Program(vec![ExpressionStatement {
                expr: Expression::Function {
                    params: vec![],
                    return_type: None,
                    body: vec![ExpressionStatement {
                        expr: Expression::Unit,
                        discarded: false,
                    }],
                },
                discarded: false,
            }]),
        );

        expect_ast(
            "fun () Int { 8 }",
            Program(vec![ExpressionStatement {
                expr: Expression::Function {
                    params: vec![],
                    return_type: Some(Type::Ident("Int".to_string())),
                    body: vec![ExpressionStatement {
                        expr: Expression::IntLiteral(8),
                        discarded: false,
                    }],
                },
                discarded: false,
            }]),
        );

        expect_ast(
            "fun (x: Int) Int { x }",
            Program(vec![ExpressionStatement {
                expr: Expression::Function {
                    params: vec![FunctionParam::LabeledAtCallsite {
                        name: "x".to_string(),
                        label: None,
                        type_: Type::Ident("Int".to_string()),
                    }],
                    return_type: Some(Type::Ident("Int".to_string())),
                    body: vec![ExpressionStatement {
                        expr: Expression::Ident("x".to_string()),
                        discarded: false,
                    }],
                },
                discarded: false,
            }]),
        );

        expect_ast(
            "fun (external internal: Int) Int { internal }",
            Program(vec![ExpressionStatement {
                expr: Expression::Function {
                    params: vec![FunctionParam::LabeledAtCallsite {
                        name: "internal".to_string(),
                        label: Some("external".to_string()),
                        type_: Type::Ident("Int".to_string()),
                    }],
                    return_type: Some(Type::Ident("Int".to_string())),
                    body: vec![ExpressionStatement {
                        expr: Expression::Ident("internal".to_string()),
                        discarded: false,
                    }],
                },
                discarded: false,
            }]),
        );

        expect_ast(
            "fun (~x: Int) Int { x }",
            Program(vec![ExpressionStatement {
                expr: Expression::Function {
                    params: vec![FunctionParam::UnlabeledAtCallsite {
                        name: "x".to_string(),
                        type_: Type::Ident("Int".to_string()),
                    }],
                    return_type: Some(Type::Ident("Int".to_string())),
                    body: vec![ExpressionStatement {
                        expr: Expression::Ident("x".to_string()),
                        discarded: false,
                    }],
                },
                discarded: false,
            }]),
        );

        expect_ast(
            "fun () Int { x :: 8; x }",
            Program(vec![ExpressionStatement {
                expr: Expression::Function {
                    params: vec![],
                    return_type: Some(Type::Ident("Int".to_string())),
                    body: vec![
                        ExpressionStatement {
                            expr: Expression::VariableDecl {
                                name: "x".to_string(),
                                value: Box::new(Expression::IntLiteral(8)),
                                mutable: false,
                                type_: None,
                            },
                            discarded: true,
                        },
                        ExpressionStatement {
                            expr: Expression::Ident("x".to_string()),
                            discarded: false,
                        },
                    ],
                },
                discarded: false,
            }]),
        );
    }

    #[test]
    fn function_calls() {
        expect_ast(
            "print(3)",
            Program(vec![ExpressionStatement {
                expr: Expression::FunctionCall {
                    name: "print".to_string(),
                    args: vec![Argument {
                        label: None,
                        value: Expression::IntLiteral(3),
                    }],
                },
                discarded: false,
            }]),
        );

        expect_ast(
            "print(3, 4)",
            Program(vec![ExpressionStatement {
                expr: Expression::FunctionCall {
                    name: "print".to_string(),
                    args: vec![
                        Argument {
                            label: None,
                            value: Expression::IntLiteral(3),
                        },
                        Argument {
                            label: None,
                            value: Expression::IntLiteral(4),
                        },
                    ],
                },
                discarded: false,
            }]),
        );

        expect_ast(
            "print(3, and: 4)",
            Program(vec![ExpressionStatement {
                expr: Expression::FunctionCall {
                    name: "print".to_string(),
                    args: vec![
                        Argument {
                            label: None,
                            value: Expression::IntLiteral(3),
                        },
                        Argument {
                            label: Some("and".to_string()),
                            value: Expression::IntLiteral(4),
                        },
                    ],
                },
                discarded: false,
            }]),
        );
    }
}
