use std::{fs::File, mem};

use gccjit::{CompileResult, Context, Field, OutputKind, Parameter, Type};

use crate::ir::{IRFunction, IRModule, IRType};

pub struct Codegen<'gcc> {
    ctx: Context<'gcc>,
}

impl<'gcc> Codegen<'gcc> {
    fn new() -> Codegen<'gcc> {
        let ctx = Context::default();
        ctx.set_dump_code_on_compile(true);
        ctx.set_optimization_level(gccjit::OptimizationLevel::Limited);
        Codegen { ctx }
    }

    pub fn compile(jit: bool, module: IRModule) {
        let codegen = Codegen::new();

        let res = codegen.ctx.compile();
        if jit {
            let main: extern "C" fn() -> i8 = if !res.get_function("main").is_null() {
                unsafe { mem::transmute(res.get_function("main")) }
            } else {
                panic!("main fn not found");
            };
            main();
            println!("Process exited with code {}", main());
        } else {
            codegen.ctx.compile_to_file(OutputKind::Executable, "a.out");
        }
    }

    fn decl_fn(&self, func: &IRFunction) {
        let args = func
            .args
            .iter()
            .map(|e| {
                self.ctx.new_parameter(
                    None,
                    self.convert_ir_to_gccjit_type(e.1.clone()),
                    e.0.clone(),
                )
            })
            .collect::<Vec<Parameter>>();
        self.ctx.new_function(
            None,
            gccjit::FunctionType::AlwaysInline,
            self.ctx.new_type::<i8>(),
            &args,
            &func.name,
            false,
        );
    }

    fn convert_ir_to_gccjit_type(&self, typ: IRType) -> Type {
        match typ {
            IRType::I8 => self.ctx.new_type::<i8>(),
            IRType::I16 => self.ctx.new_type::<i16>(),
            IRType::I32 => self.ctx.new_type::<i32>(),
            IRType::I64 => self.ctx.new_type::<i64>(),
            IRType::U8 => self.ctx.new_type::<u8>(),
            IRType::U16 => self.ctx.new_type::<u16>(),
            IRType::U32 => self.ctx.new_type::<u32>(),
            IRType::U64 => self.ctx.new_type::<u64>(),
            IRType::Size => self.ctx.new_type::<usize>(),
            IRType::ZeroSized => self.ctx.new_type::<()>(),
            IRType::Array(arr_typ, size) => self.ctx.new_array_type(
                None,
                self.convert_ir_to_gccjit_type((*arr_typ).clone()),
                size as i32,
            ),
            IRType::Ref(ref_t) => self
                .convert_ir_to_gccjit_type((*ref_t).clone())
                .make_pointer(),
            IRType::Custom(name, types) => self
                .ctx
                .new_struct_type(
                    None,
                    &name,
                    types
                        .into_iter()
                        .map(|e| {
                            self.ctx
                                .new_field(None, self.convert_ir_to_gccjit_type(e.1), e.0)
                        })
                        .collect::<Vec<Field>>()
                        .as_slice(),
                )
                .as_type(),
            _ => todo!("unimplemented type"),
        }
    }
}
