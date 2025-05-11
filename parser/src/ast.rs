#[derive(PartialEq, Eq, Debug)]
pub struct Program(pub Vec<Statement>);

#[derive(PartialEq, Eq, Debug)]
pub enum Statement {
    Expression(Expression),
    VariableDecl(VariableDecl),
    FunctionDecl(FunctionDecl),
}

#[derive(PartialEq, Eq, Debug)]
pub enum Expression {
    IntLiteral(isize),
    Ident(String),

    PrefixExpr {
        perator: PrefixOperator,
        expr: Box<Expression>,
    },

    InfixExpr {
        left: Box<Expression>,
        operator: InfixOperator,
        right: Box<Expression>,
    },

    FunctionCall {
        name: String,
        args: Vec<Argument>,
    },
}

#[derive(PartialEq, Eq, Debug)]
pub enum PrefixOperator {
    Not,
    Positive,
    Negative,
}

#[derive(PartialEq, Eq, Debug)]
pub enum InfixOperator {
    Plus,
    Minus,
    Divide,
    Multiply,
    Exponent,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
}

#[derive(PartialEq, Eq, Debug)]
pub struct Argument {
    pub label: Option<String>,
    pub value: Expression,
}

#[derive(PartialEq, Eq, Debug)]
pub struct VariableDecl {
    pub name: String,
    pub value: Expression,
    pub mutable: bool,
    pub type_: Type,
}

#[derive(PartialEq, Eq, Debug)]
pub struct FunctionDecl {
    pub name: String,
    pub params: Vec<FunctionParam>,
    pub return_type: Type,
    pub body: Vec<Statement>,
}

#[derive(PartialEq, Eq, Debug)]
pub struct FunctionParam {
    pub external_name: Option<String>,
    pub internal_name: String,
    pub labeled: bool,
    pub type_: Type,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Type {
    Ident(String),
}

#[derive(Ord, Eq, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Sum,     // + or -
    Product, // * or /
    Group,   // ( )
}
