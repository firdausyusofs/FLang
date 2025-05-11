#[derive(PartialEq, Eq, Debug)]
pub struct Program(pub Vec<ExpressionStatement>);

#[derive(PartialEq, Eq, Debug)]
pub struct ExpressionStatement {
    pub expr: Expression,
    pub discarded: bool,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Expression {
    IntLiteral(isize),
    Ident(String),

    Unit,

    PrefixExpr {
        perator: PrefixOperator,
        expr: Box<Expression>,
    },

    InfixExpr {
        left: Box<Expression>,
        operator: InfixOperator,
        right: Box<Expression>,
    },

    VariableDecl {
        name: String,
        value: Box<Expression>,
        mutable: bool,
        type_: Option<Type>,
    },

    Function {
        params: Vec<FunctionParam>,
        return_type: Option<Type>,
        body: Vec<ExpressionStatement>,
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
pub enum FunctionParam {
    LabeledAtCallsite {
        name: String,
        label: Option<String>,
        type_: Type,
    },
    UnlabeledAtCallsite {
        name: String,
        type_: Type,
    },
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
