use gccjit::BinaryOp;
use istd::bump_box;

bump_box!(ir_type_scope, IRTypeMap, IRTypeBox, crate::ir::IRType);
bump_box!(ir_expr_scope, IRExprMap, IRExprBox, crate::ir::IRExpr);
bump_box!(ir_value_scope, IRValueMap, IRValueBox, crate::ir::IRValue);

pub fn init_maps() {
    IRTypeMap::init(1000);
    IRExprMap::init(1000);
    IRValueMap::init(1000);
}

#[derive(Debug, Clone)]
pub enum IRType {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    Size,
    Ref(IRTypeBox),
    Array(IRTypeBox, usize),
    Custom(String, Vec<(String, IRType)>),
    ZeroSized,
}

#[derive(Debug, Clone)]
pub struct IRFunction {
    pub name: String,
    pub return_type: IRType,
    pub args: Vec<(String, IRType)>,
    pub blocks: Option<Vec<IRBasicBlock>>,
    pub linkage: IRLinkage,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum IRLinkage {
    Public,
    Private,
    External,
}

#[derive(Debug, Clone)]
pub struct IRBasicBlock {
    pub name: String,
    pub instrs: Vec<IRInstr>,
    pub terminator: IRTerminator,
}

#[derive(Clone, Debug)]
pub enum IRValue {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    Size(usize),
    Ref(IRValueBox),
    Array(Vec<IRValue>, usize),
    Custom(String, Vec<(String, IRValue)>),
}

impl IRValue {
    pub fn to_type(&self) -> IRType {
        match self {
            IRValue::I8(_) => IRType::I8,
            IRValue::I16(_) => IRType::I16,
            IRValue::I32(_) => IRType::I32,
            IRValue::I64(_) => IRType::I64,
            IRValue::U8(_) => IRType::U8,
            IRValue::U16(_) => IRType::U16,
            IRValue::U32(_) => IRType::U32,
            IRValue::U64(_) => IRType::U64,
            IRValue::Size(_) => IRType::Size,
            IRValue::Array(vals, len) => {
                if *len > 0 {
                    vals[0].to_type()
                } else {
                    IRType::ZeroSized
                }
            }
            IRValue::Custom(name, vals) => {
                if vals.len() > 0 {
                    let mut types = Vec::new();
                    for (n, val) in vals {
                        types.push((n.clone(), val.to_type()))
                    }
                    IRType::Custom(name.clone(), types)
                } else {
                    IRType::ZeroSized
                }
            }
            IRValue::Ref(val) => IRType::Ref(IRTypeBox::new(val.to_type())),
        }
    }
}

#[derive(Debug, Clone)]
pub enum IRInstr {
    NewVar(String, IRType),
    SetVar(String, IRExpr),
    Expr(IRExpr),
}

#[derive(Clone, Debug)]
pub enum IRExpr {
    GetVar(String),
    Value(IRValue),
    BiOp(BiOp, IRExprBox, IRExprBox),
    Not(IRExprBox),
    FnCall(String, Vec<IRExpr>),
}

#[derive(Debug, Clone)]
pub enum BiOp {
    Add,
    Sub,
    Mod,
    Div,
    Mul,
    And,
    Or,
    Xor,
}

impl BiOp {
    pub fn to_gccjit_op(&self) -> BinaryOp {
        match self {
            BiOp::Add => BinaryOp::Plus,
            BiOp::And => BinaryOp::BitwiseAnd,
            BiOp::Div => BinaryOp::Divide,
            BiOp::Mod => BinaryOp::Modulo,
            BiOp::Mul => BinaryOp::Mult,
            BiOp::Or => BinaryOp::BitwiseOr,
            BiOp::Sub => BinaryOp::Minus,
            BiOp::Xor => BinaryOp::BitwiseXor
        }
    }
}

#[derive(Debug, Clone)]
pub enum IRTerminator {
    Jmp(String),
    Branch(IRExpr, String, String),
    Ret(IRExpr),
}

#[derive(Debug, Clone)]
pub struct IRModule {
    pub functions: Vec<IRFunction>,
}
