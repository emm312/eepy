use std::{collections::HashMap, io::Write, path::Path};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine},
    types::{AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum},
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue},
    AddressSpace, OptimizationLevel,
};

use crate::{
    envs::{env_flag, DUMP_LLVM_IR},
    ir::{IRBasicBlock, IRExpr, IRFunction, IRModule, IRType, IRValue, IRInstr, IRTerminator},
};

pub struct Codegen<'ctx> {
    module: Module<'ctx>,
    ctx: &'ctx Context,
    builder: Builder<'ctx>,

    cur_fn: Option<FunctionValue<'ctx>>,
    vars: HashMap<String, PointerValue<'ctx>>,
}

type Main = unsafe extern "C" fn() -> i8;

impl<'ctx> Codegen<'ctx> {
    pub fn compile(ir_module: IRModule, jit: bool) {
        let ctx = Context::create();
        let module = ctx.create_module("taube");
        let builder = ctx.create_builder();

        let mut exec_engine = None;
        if jit {
            exec_engine = Some(
                module
                    .create_jit_execution_engine(OptimizationLevel::Aggressive)
                    .unwrap(),
            );
        }

        let mut codegen = Codegen {
            module,
            ctx: &ctx,
            builder,

            cur_fn: None,
            vars: HashMap::new(),
        };

        codegen.compile_ast(ir_module);
        if env_flag(DUMP_LLVM_IR) {
            codegen.module.print_to_stderr();
        }

        if jit {
            let main;
            unsafe {
                main = exec_engine
                    .unwrap()
                    .get_function::<Main>("main")
                    .expect("main fn needs to be defined");
                println!("Process exited with code {}", main.call())
            }
        } else {
            codegen.write_object(&mut std::fs::File::create("out.o").unwrap());
        }
    }

    fn compile_ast(&mut self, ast: IRModule) {
        for function in ast.functions {
            self.add_function(&function);
            if let Some(blocks) = function.blocks {
                for block in blocks {
                    self.compile_block(block);
                }
            }
        }
    }

    fn compile_block(&mut self, ir_block: IRBasicBlock) {
        let block = self
            .ctx
            .append_basic_block(self.cur_fn.expect("no fn selected"), &ir_block.name);

        self.builder.position_at_end(block);

        for instr in ir_block.instrs {
            match instr {
                IRInstr::Expr(e) => { self.compile_expr(e); },
                _ => todo!(),
            }
        }

        match ir_block.terminator {
            IRTerminator::Ret(expr) => {
                let val = self.compile_expr(expr);
                self.builder.build_return(Some(&val));
            }
            _ => todo!()
        }
    }

    fn compile_expr(&self, expr: IRExpr) -> BasicValueEnum {
        match expr {
            IRExpr::Value(n) => self.compile_value(n),
            IRExpr::FnCall(name, args) => {
                let func = self
                    .module
                    .get_function(&name)
                    .expect("fn call to a fn that doesnt exist");

                let mut arg_vals = Vec::new();

                for arg in args {
                    arg_vals.push(self.compile_expr(arg));
                }

                let arg_enum = arg_vals
                    .into_iter()
                    .map(|e| e.into())
                    .collect::<Vec<BasicMetadataValueEnum>>();

                self.builder
                    .build_call(func, &arg_enum, "fn_call")
                    .try_as_basic_value()
                    .unwrap_left()
            }
            _ => todo!(),
        }
    }

    fn compile_value(&self, val: IRValue) -> BasicValueEnum {
        match val {
            IRValue::I8(val) => self
                .ctx
                .i8_type()
                .const_int(val as u64, true)
                .as_basic_value_enum(),
            IRValue::I16(val) => self
                .ctx
                .i16_type()
                .const_int(val as u64, true)
                .as_basic_value_enum(),
            IRValue::I32(val) => self
                .ctx
                .i32_type()
                .const_int(val as u64, true)
                .as_basic_value_enum(),
            IRValue::I64(val) => self
                .ctx
                .i64_type()
                .const_int(val as u64, true)
                .as_basic_value_enum(),
            IRValue::U8(val) => self
                .ctx
                .i8_type()
                .const_int(val as u64, false)
                .as_basic_value_enum(),
            IRValue::U16(val) => self
                .ctx
                .i8_type()
                .const_int(val as u64, false)
                .as_basic_value_enum(),
            IRValue::U32(val) => self
                .ctx
                .i8_type()
                .const_int(val as u64, false)
                .as_basic_value_enum(),
            IRValue::U64(val) => self
                .ctx
                .i8_type()
                .const_int(val as u64, false)
                .as_basic_value_enum(),
            _ => todo!(),
        }
    }

    fn add_function(&mut self, func: &IRFunction) -> FunctionValue {
        let args = func
            .args
            .iter()
            .map(|e| self.ir_type_to_metadata(&e.1))
            .collect::<Vec<BasicMetadataTypeEnum>>();

        let fn_type = match func.return_type {
            IRType::ZeroSized => self.ctx.void_type().fn_type(&args, false),
            _ => self
                .ir_type_to_basic(&func.return_type)
                .fn_type(&args, false),
        };

        let val = self
            .module
            .add_function(&func.name, fn_type, func.linkage.to_llvm());

        self.cur_fn = Some(val);

        val
    }

    fn ir_type_to_basic(&self, typ: &IRType) -> BasicTypeEnum<'ctx> {
        match typ {
            IRType::I8 | IRType::U8 => self.ctx.i8_type().as_basic_type_enum(),
            IRType::I16 | IRType::U16 => self.ctx.i16_type().as_basic_type_enum(),
            IRType::I32 | IRType::U32 => self.ctx.i32_type().as_basic_type_enum(),
            IRType::I64 | IRType::U64 => self.ctx.i64_type().as_basic_type_enum(),
            IRType::Ref(typ) => self
                .ir_type_to_basic(typ)
                .into_pointer_type()
                .as_basic_type_enum(),
            IRType::Array(typ, size) => self
                .ir_type_to_basic(typ)
                .array_type(*size as u32)
                .as_basic_type_enum(),
            _ => todo!(),
        }
    }

    fn ir_type_to_metadata(&self, typ: &IRType) -> BasicMetadataTypeEnum<'ctx> {
        BasicMetadataTypeEnum::try_from(self.ir_type_to_basic(typ)).unwrap()
    }

    fn write_object(&self, file: &mut impl Write) {
        Target::initialize_all(&InitializationConfig::default());
        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple).unwrap();
        let cpu = TargetMachine::get_host_cpu_name();
        let features = TargetMachine::get_host_cpu_features();
        let reloc = RelocMode::Default;
        let model = CodeModel::Default;
        let opt = OptimizationLevel::Aggressive;
        let target_machine = target
            .create_target_machine(
                &triple,
                cpu.to_str().unwrap(),
                features.to_str().unwrap(),
                opt,
                reloc,
                model,
            )
            .unwrap();

        target_machine
            .write_to_file(&self.module, FileType::Object, Path::new("out.o"))
            .unwrap();
    }
}
