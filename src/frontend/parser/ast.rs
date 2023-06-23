use std::io::Write;

use istd::index_map;

use crate::frontend::{SourceRange, Literal, lexer::{Token, TokenKind}, SymbolMap};


index_map!(NodeMap, NodeIndex, Node);


#[derive(Debug)]
pub struct AbstractSyntaxTree {
    tree: Block,
    node_map: NodeMap,
}


impl AbstractSyntaxTree {
    pub(super) fn new(tree: Block, node_map: NodeMap) -> Self {
        Self {
            tree,
            node_map,
        }
    }
}


#[derive(Debug, PartialEq)]
pub struct Node {
    pub node_type: NodeType,
    pub source_range: SourceRange,
}


#[derive(Debug, PartialEq)]
pub enum NodeType {
    Declaration(Declaration),
    Statement(Statement),
    Expression(Expression),
}


#[derive(Debug, PartialEq)]
pub enum Declaration {
}


#[derive(Debug, PartialEq)]
pub enum Statement {
        
}


#[derive(Debug, PartialEq)]
pub enum Expression {
    Literal(Literal),


    BinaryOperation {
        operator: BinaryOperator,
        left: NodeIndex,
        right: NodeIndex,
    },


    UnaryOperation {
        operator: UnaryOperator,
        value: NodeIndex,
    },

    
    Block {
        block: Block
    },


    IfExpression {
        condition: NodeIndex,
        block: Block,
        else_block: Option<NodeIndex>,
    },


    Loop {
        body: Block,
    },


    AsCast {
        value: NodeIndex,
    },


    Unsafe {
        block: Block,
    }
}


#[derive(Debug, PartialEq)]
pub struct BinaryOperator {
    kind: BinaryOperatorKind,
    source_range: SourceRange,
}


#[derive(Debug, PartialEq)]
enum BinaryOperatorKind {
    Add,
    Sub,
    Mul,
    Div,
    Rem,

    GreaterThan,
    LessThan,
    LessEquals,
    GreaterEquals,
    EqualsTo,
    NotEqualsTo,

    BitwiseOR,
    BitwiseAND,
    BitwiseXOR,

    LeftShift,
    RightShift,
    RightShiftZero,
}


impl BinaryOperator {
    /// # Panics
    /// if the `token.token_kind` is not a valid
    /// binary operator.
    pub fn from_token(token: &Token) -> Self {
        let kind = match token.token_kind {
            TokenKind::Plus           => BinaryOperatorKind::Add,
            TokenKind::Minus          => BinaryOperatorKind::Sub,
            TokenKind::Star           => BinaryOperatorKind::Mul,
            TokenKind::Slash          => BinaryOperatorKind::Div,
            TokenKind::Percent        => BinaryOperatorKind::Rem,


            TokenKind::RightAngle     => BinaryOperatorKind::GreaterThan,
            TokenKind::LeftAngle      => BinaryOperatorKind::LessThan,
            TokenKind::GreaterEquals  => BinaryOperatorKind::GreaterEquals,
            TokenKind::LesserEquals   => BinaryOperatorKind::LessEquals,
            TokenKind::EqualsTo       => BinaryOperatorKind::EqualsTo,
            TokenKind::NotEqualsTo    => BinaryOperatorKind::NotEqualsTo,

            TokenKind::BitwiseOR      => BinaryOperatorKind::BitwiseOR,
            TokenKind::BitwiseAND     => BinaryOperatorKind::BitwiseAND,
            TokenKind::BitwiseXOR     => BinaryOperatorKind::BitwiseXOR,

            TokenKind::LeftShift      => BinaryOperatorKind::LeftShift,
            TokenKind::RightShift     => BinaryOperatorKind::RightShift,
            TokenKind::RightShiftZero => BinaryOperatorKind::RightShiftZero,

            _ => panic!("unexpeted token kind")
        };


        Self {
            kind,
            source_range: token.source_range,
        }
    }
}



#[derive(Debug, PartialEq)]
pub struct UnaryOperator {
    kind: UnaryOperatorKind,
    source_range: SourceRange,
}


#[derive(Debug, PartialEq)]
enum UnaryOperatorKind {
    Not,
    Neg,
}


impl UnaryOperator {
    /// # Panics
    /// if the `token.token_kind` is not a valid
    /// binary operator.
    pub fn from_token(token: &Token) -> Self {
        let kind = match token.token_kind {
            TokenKind::Bang  => UnaryOperatorKind::Not,
            TokenKind::Minus => UnaryOperatorKind::Neg,

            _ => panic!("unexpeted token kind")
        };


        Self {
            kind,
            source_range: token.source_range,
        }
    }
}


/// A wrapper type around `Vec<NodeIndex>`
/// guaranteeing that the map will never be empty
#[derive(Debug, PartialEq)]
pub struct Block(Vec<NodeIndex>);

impl Block {
    /// The user must ensure that `vec` is not empty
    /// or this function will panic
    pub fn new(vec: Vec<NodeIndex>) -> Self {
        assert!(!vec.is_empty());
        Self(vec)
    }
}


impl std::ops::Deref for Block {
    type Target = Vec<NodeIndex>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}


impl std::ops::DerefMut for Block {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
