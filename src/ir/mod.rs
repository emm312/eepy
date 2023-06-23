use inkwell::module::Linkage;

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
    SignedPtr,
    UnsignedPtr,
    Ref(Box<IRType>),
    Array(Box<IRType>, usize),
    Custom(Vec<IRType>),
    ZeroSized,
}

pub struct IRFunction {
    pub name: String,
    pub return_type: IRType,
    pub args: Vec<(String, IRType)>,
    pub blocks: Option<Vec<IRBasicBlock>>,
    pub linkage: IRLinkage,
}

pub enum IRLinkage {
    Public,
    Private,
    External,
}

impl IRLinkage {
    pub fn to_llvm(&self) -> Option<Linkage> {
        match self {
            IRLinkage::External => Some(Linkage::External),
            IRLinkage::Public => None,
            IRLinkage::Private => Some(Linkage::Private),
        }
    }
}

pub struct IRBasicBlock {
    pub name: String,
    pub instrs: Vec<IRInstr>,
    pub terminator: IRTerminator,
}

pub enum IRValue {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    SignedPtr(isize),
    UnsignedPtr(usize),
    Ref(Box<IRValue>),
    Array(Vec<IRValue>, usize),
    Custom(Vec<IRValue>),
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
            IRValue::SignedPtr(_) => IRType::SignedPtr,
            IRValue::UnsignedPtr(_) => IRType::UnsignedPtr,
            IRValue::Array(vals, len) => {
                if *len > 0 {
                    vals[0].to_type()
                } else {
                    IRType::ZeroSized
                }
            }
            IRValue::Custom(vals) => {
                if vals.len() > 0 {
                    let mut types = Vec::new();
                    for val in vals {
                        types.push(val.to_type())
                    }
                    IRType::Custom(types)
                } else {
                    IRType::ZeroSized
                }
            }
            IRValue::Ref(val) => IRType::Ref(Box::new(val.to_type())),
        }
    }
}

pub enum IRInstr {
    NewVar(String, IRType),
    SetVar(String, IRExpr),
    Expr(IRExpr),
}

pub enum IRExpr {
    GetVar(String),
    Value(IRValue),
    Add(Box<IRExpr>, Box<IRExpr>),
    Sub(Box<IRExpr>, Box<IRExpr>),
    Mod(Box<IRExpr>, Box<IRExpr>),
    Div(Box<IRExpr>, Box<IRExpr>),
    Mul(Box<IRExpr>, Box<IRExpr>),
    And(Box<IRExpr>, Box<IRExpr>),
    Or(Box<IRExpr>, Box<IRExpr>),
    Xor(Box<IRExpr>, Box<IRExpr>),
    Not(Box<IRExpr>),
    FnCall(String, Vec<IRExpr>),
}

pub enum IRTerminator {
    Jmp(String),
    Branch(IRExpr, String, String),
    Ret(IRExpr),
}

pub struct IRModule {
    pub functions: Vec<IRFunction>,
}
