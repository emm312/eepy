use std::{collections::HashMap, mem};

use gccjit::{CompileResult, Context, Field, Function, OutputKind, Parameter, RValue, Type};

use crate::{ir::{IRExpr, IRFunction, IRModule, IRType, IRValue, IRInstr, IRTerminator, IRLinkage}, envs::env_flag};

pub struct Codegen<'gcc> {
    ctx: Context<'gcc>,
}

impl<'gcc> Codegen<'gcc> {
    fn new() -> Codegen<'gcc> {
        let ctx = Context::default();
        ctx.set_dump_code_on_compile(env_flag("TAUBE_DUMP_ASM"));
        ctx.set_optimization_level(gccjit::OptimizationLevel::Limited);
        Codegen { ctx }
    }

    pub fn compile(jit: bool, module: IRModule) {
        let codegen = Codegen::new();
        let mut functions: HashMap<String, Function<'_>> = HashMap::new();

        for ir_func in module.functions.iter() {
            let args = ir_func
                .args
                .iter()
                .map(|e| {
                    codegen.ctx.new_parameter(
                        None,
                        codegen.convert_ir_to_gccjit_type(e.1.clone()),
                        e.0.clone(),
                    )
                })
                .collect::<Vec<Parameter>>();
            let func = codegen.ctx.new_function(
                None,
                gccjit::FunctionType::Extern,
                codegen.ctx.new_type::<i8>(),
                &args,
                &ir_func.name,
                false,
            );
            functions.insert(ir_func.name.clone(), func);
        }

        codegen.compile_inner(module, &mut functions);

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
            codegen.ctx.set_dump_code_on_compile(false);
            codegen.ctx.compile_to_file(OutputKind::Executable, "a.out");
        }
    }

    fn compile_inner(&self, module: IRModule, functions: &mut HashMap<String, Function>) {
        for ir_func in module.functions {
            if let None = ir_func.blocks { continue; }
            let args = ir_func
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
            let func = self.ctx.new_function(
                None,
                gccjit::FunctionType::Exported,
                self.ctx.new_type::<i8>(),
                &args,
                &ir_func.name,
                false,
            );
            if let Some(blocks) = ir_func.blocks {
                for block in blocks {
                    let bb = func.new_block(block.name);
                    for instr in block.instrs {
                        match instr {
                            IRInstr::Expr(e) => bb.add_eval(None, self.compile_expr(e, functions)),
                            _ => todo!()
                        }
                    }
                    match block.terminator {
                        IRTerminator::Ret(val) => {
                            bb.end_with_return(None, self.compile_expr(val, functions));
                        }
                        _ => todo!()
                    }
                }    
            }
        }
    }

    fn compile_expr(
        &'gcc self,
        expr: IRExpr,
        functions: &HashMap<String, Function<'gcc>>,
    ) -> RValue {
        match expr {
            IRExpr::FnCall(func, args) => self.ctx.new_call(
                None,
                functions[&func],
                args.into_iter()
                    .map(|e| self.compile_expr(e, functions))
                    .collect::<Vec<RValue>>()
                    .as_slice(),
            ),
            IRExpr::Value(val) => {
                match val {
                    IRValue::I8(v) => self.ctx.new_rvalue_from_int(self.convert_ir_to_gccjit_type(val.to_type()), v as i32),
                    IRValue::I16(v) => self.ctx.new_rvalue_from_int(self.convert_ir_to_gccjit_type(val.to_type()), v as i32),
                    IRValue::I32(v) => self.ctx.new_rvalue_from_int(self.convert_ir_to_gccjit_type(val.to_type()), v as i32),
                    IRValue::I64(v) => self.ctx.new_rvalue_from_int(self.convert_ir_to_gccjit_type(val.to_type()), v as i32),
                    IRValue::U8(v) => self.ctx.new_rvalue_from_int(self.convert_ir_to_gccjit_type(val.to_type()), v as i32),
                    IRValue::U16(v) => self.ctx.new_rvalue_from_int(self.convert_ir_to_gccjit_type(val.to_type()), v as i32), 
                    IRValue::U32(v) => self.ctx.new_rvalue_from_int(self.convert_ir_to_gccjit_type(val.to_type()), v as i32),
                    IRValue::U64(v) => self.ctx.new_rvalue_from_int(self.convert_ir_to_gccjit_type(val.to_type()), v as i32),
                    _ => todo!()
                }
            }
            _ => todo!(),
        }
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
