use std::io::Write;

use istd::index_map;

use crate::frontend::{SourceRange, Literal, lexer::{Token, TokenKind}, SymbolMap, SymbolIndex};


index_map!(NodeMap, NodeIndex, Node);

impl NodeMap {
    pub fn trim(&mut self) {
        self.vec.shrink_to_fit()
    }
}


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
    FunctionDeclaration {
        ident: SymbolIndex,
        arguments: Vec<(SymbolIndex, DataTypeNode)>,
        return_type: DataTypeNode,
        block: Block,
    },


    StructureDeclaration {
        ident: SymbolIndex,
        fields: Vec<(SymbolIndex, DataTypeNode)>,
    },


    NamespaceDeclaration {
        ident: SymbolIndex,
        block: Block,
    },


    ExternFunctionDeclaration {
        ident: SymbolIndex,
        extern_type: SymbolIndex,
        arguments: Vec<(SymbolIndex, DataTypeNode)>,
        return_type: DataTypeNode,
        custom_name: SymbolIndex,
    },


    ConstItemDeclaration {
        ident: SymbolIndex,
        type_hint: Option<DataTypeNode>,
        value: NodeIndex,
    },


    StaticItemDeclaration {
        ident: SymbolIndex,
        type_hint: Option<DataTypeNode>,
        value: NodeIndex,
        mutability: bool,
    },


    UsingDeclaration {
        path: PathSymbol,
    }
}


#[derive(Debug, PartialEq)]
pub enum Statement {
    Continue,
    Break(NodeIndex),
    Return(NodeIndex),

    VarDeclaration {
        name: SymbolIndex,
        type_hint: Option<DataTypeNode>,
        value: NodeIndex,
        mutability: bool,
    }
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
        block: Block,
    },


    AsCast {
        value: NodeIndex,
    },


    Unsafe {
        block: Block,
    },


    Identifier {
        ident: PathSymbol,
    },


    FunctionCall {
        path: NodeIndex,
        arguments: Vec<NodeIndex>,
    },


    StructureCreation {
        path: PathSymbol,
        fields: Vec<(SymbolIndex, NodeIndex)>,
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

            _ => panic!("unexpected token kind")
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


    pub fn source_range(&self, node_map: &NodeMap) -> SourceRange {
        SourceRange::with(node_map.get(*self.0.first().unwrap()).unwrap().source_range, node_map.get(*self.0.last().unwrap()).unwrap().source_range)
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


#[derive(Debug, PartialEq)]
pub struct DataTypeNode {
    pub source_range: SourceRange,
    pub data_type: DataType,
    pub condition: Option<DataTypeConditionNode>,
}


#[derive(Debug, PartialEq)]
pub enum DataType {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    Bool,

    Empty,

    ConstPointer(Box<DataTypeNode>),
    MutPointer(Box<DataTypeNode>),

    Custom(PathSymbol)
}


#[derive(Debug, PartialEq)]
pub struct DataTypeConditionNode {
    pub source_range: SourceRange,
    pub binding_name: SymbolIndex,
    pub condition: NodeIndex,
}


#[derive(Debug, PartialEq)]
pub struct PathSymbol {
    pub source_range: SourceRange,
    pub value: Vec<SymbolIndex>,
}


impl PathSymbol {
    pub fn new(source_range: SourceRange, value: Vec<SymbolIndex>) -> Self { Self { value, source_range } }
}