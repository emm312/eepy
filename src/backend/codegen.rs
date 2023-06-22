use std::{collections::HashMap, io::Write, path::Path};

use inkwell::{module::Module, context::Context, builder::Builder, values::{PointerValue, FunctionValue}, targets::{Target, InitializationConfig, TargetMachine, RelocMode, CodeModel, FileType}, OptimizationLevel, types::{BasicTypeEnum, BasicType, BasicMetadataTypeEnum}};

use crate::ir::{IRModule, IRFunction, IRType};

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
            exec_engine = Some(module.create_jit_execution_engine(OptimizationLevel::Aggressive).unwrap());
        }

        let mut codegen = Codegen {
            module,
            ctx: &ctx,
            builder,

            cur_fn: None,
            vars: HashMap::new()
        };

        for function in ir_module.functions {
            codegen.compile_function(function);
        }

        if jit {
            let main;
            unsafe {
                main = exec_engine.unwrap().get_function::<Main>("main").expect("main fn needs to be defined");
                println!("Process exited with code {}", main.call())
            }
        } else {
            codegen.write_object(&mut std::fs::File::create("out.o").unwrap());
        }
    }

    fn compile_function(&mut self, func: IRFunction) {
        let fn_type = self.ir_type_to_basic(func.return_type)
            .fn_type(func.args.into_iter().map(|e| self.ir_type_to_metadata(e.1).as_basic_type_enum()).collect(), false);
    }

    fn ir_type_to_basic(&self, typ: IRType) -> BasicTypeEnum<'ctx> {
        match typ {
            IRType::I8 => self.ctx.i8_type().as_basic_type_enum(),
            _ => todo!()
        }
    }

    fn ir_type_to_metadata(&self, typ: IRType) -> BasicMetadataTypeEnum<'ctx> {

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