use crate::compiler::ir::{IR, IRVar, Instruction, InstructionKind};
use std::collections::HashMap;
use std::fmt::Write;

struct Locals {
    var_location_map: HashMap<IRVar, String>,
    stack_used: usize,
}
impl Locals {
    fn new_from_vars(vars: Vec<IRVar>) -> Self {
        let mut map = HashMap::new();
        let mut stack_used = 0;
        for (idx, var) in vars.into_iter().enumerate() {
            // function irvars will point to labels, not data on the stack
            if let Some(dest_func) = &var.aliases_func {
                map.insert(var.clone(), dest_func.clone());
            } else {
                map.insert(var, format!("-{}(%rbp)", 8 + 8 * idx));
            }
            stack_used += 8;
        }
        Self {
            var_location_map: map,
            stack_used,
        }
    }

    fn get_ref(&self, v: &IRVar) -> Result<&String, String> {
        self.var_location_map
            .get(v)
            .ok_or(format!("failed to get stack ref for {v:?}"))
    }
}

pub fn generate_assembly(ir: Vec<IR>, vars: Vec<IRVar>) -> Result<String, String> {
    let locals = Locals::new_from_vars(vars);

    let mut output = String::new();
    writeln!(
        output,
        "    .extern print_int
    .extern print_bool
    .extern read_int
    .global main
    .type main, @function

    .section .text

main:
    pushq %rbp
    movq %rsp, %rbp
    subq ${}, %rsp
",
        locals.stack_used
    )
    .unwrap();

    for line in ir {
        writeln!(output, "# {line}").unwrap();
        match line {
            IR::Label(label) => {
                writeln!(output, ".L{}:", label.name).unwrap();
            }
            IR::Instruction(Instruction {
                location: _,
                instruction,
            }) => match instruction {
                InstructionKind::LoadBoolConst { value, dest } => {
                    let dest_loc = locals.get_ref(&dest)?;
                    writeln!(output, "movq ${}, {dest_loc}", u8::from(value)).unwrap();
                }
                InstructionKind::LoadIntConst { value, dest } => {
                    let dest_loc = locals.get_ref(&dest)?;
                    if value >= -2i64.pow(31) && value < 2i64.pow(31) {
                        writeln!(output, "movq ${value}, {dest_loc}").unwrap();
                    } else {
                        writeln!(output, "movabsq ${value}, %rax").unwrap();
                        writeln!(output, "movq %rax, {dest_loc}").unwrap();
                    }
                }
                InstructionKind::Copy { source, dest } => {
                    let src_loc = locals.get_ref(&source)?;
                    let dst_loc = locals.get_ref(&dest)?;
                    writeln!(output, "movq {src_loc}, %rax").unwrap();
                    writeln!(output, "movq %rax, {dst_loc}").unwrap();
                }
                InstructionKind::CondJump {
                    cond,
                    then_label,
                    else_label,
                } => {
                    let cond_loc = locals.get_ref(&cond)?;
                    writeln!(output, "cmp $0, {cond_loc}").unwrap();
                    writeln!(output, "jne .L{}", then_label.name).unwrap();
                    writeln!(output, "jmp .L{}", else_label.name).unwrap();
                }
                InstructionKind::Jump { label } => {
                    writeln!(output, "jmp .L{}", label.name).unwrap();
                }
                InstructionKind::Call { func, args, dest } => {
                    let dest_loc = locals.get_ref(&dest)?;
                    let mut arg_locs = vec![];
                    for arg in &args {
                        arg_locs.push(locals.get_ref(arg)?);
                    }
                    let assembly = match func.name.as_str() {
                        op @ ("Add" | "Sub" | "Mul") => {
                            let op = match op {
                                "Add" => "add",
                                "Sub" => "sub",
                                "Mul" => "imul",
                                _ => unreachable!(),
                            };
                            format!(
                                "movq {}, %rax
                                {op} {}, %rax
                                movq %rax, {dest_loc}",
                                arg_locs[0], arg_locs[1]
                            )
                        }
                        op @ ("Div" | "Rem") => {
                            let res_reg = if op == "Div" { "%rax" } else { "%rdx" };
                            format!(
                                "movq {}, %rax
                                cqto
                                idiv {}
                                movq {res_reg}, {dest_loc}",
                                arg_locs[0], arg_locs[1]
                            )
                        }
                        "Minus" => {
                            format!(
                                "movq {}, %rax
                            imul $-1, %rax
                            movq %rax, {}",
                                arg_locs[0], dest_loc
                            )
                        }
                        "Not" => {
                            format!(
                                "mov {}, %rdi
                                cmp $0, %rdi
                                sete %al
                                movzx %al, %rax
                                mov %rax, {}",
                                arg_locs[0], dest_loc
                            )
                        }
                        "Or" => {
                            format!(
                                "xor %rax, %rax
                                movq {}, %rax
                                or {}, %rax
                                movq %rax, {dest_loc}",
                                arg_locs[0], arg_locs[1]
                            )
                        }
                        "And" => {
                            format!(
                                "xor %rax, %rax
                                movq {}, %rax
                                and {}, %rax
                                movq %rax, {dest_loc}",
                                arg_locs[0], arg_locs[1]
                            )
                        }
                        op @ ("Lt" | "Gt" | "Leq" | "Geq" | "Eq" | "Neq") => {
                            let op = match op {
                                "Lt" => "l",
                                "Gt" => "g",
                                "Leq" => "le",
                                "Geq" => "ge",
                                "Eq" => "e",
                                "Neq" => "ne",
                                _ => unreachable!(),
                            };
                            format!(
                                "xor %rax, %rax
                                movq {}, %rdx
                                cmp {}, %rdx
                                set{op} %al
                                movq %rax, {}",
                                arg_locs[0], arg_locs[1], dest_loc
                            )
                        }
                        op => {
                            let registers = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
                            let mut assembly = String::new();
                            if let Ok(op_var) = locals.get_ref(&func) {
                                // read inputs into registers
                                for (var, reg) in arg_locs.iter().zip(registers) {
                                    writeln!(assembly, "movq {var}, %{reg}").unwrap();
                                }
                                // call and save result
                                writeln!(
                                    assembly,
                                    "callq {op_var}
                                                    movq %rax, {dest_loc}
                                ",
                                )
                                .unwrap();
                                assembly
                            } else {
                                return Err(format!("Function {op} is not defined."));
                            }
                        }
                    };
                    writeln!(output, "{assembly}").unwrap();
                }
            },
        }
    }

    writeln!(
        output,
        "movq $0, %rax
movq %rbp, %rsp
popq %rbp
ret"
    )
    .unwrap();
    Ok(output)
}

#[cfg(test)]
mod tests {
    use super::generate_assembly;
    use crate::compiler::{ir, parser, tokenizer, typecheck};

    fn assert_compiles(source_code: &str) {
        let tokens = tokenizer::tokenize(source_code);

        let mut ast = parser::parse(tokens).unwrap();
        let (typecheck_res, reserved_names) = typecheck::typecheck(&mut ast);
        typecheck_res.unwrap();
        let (ir, ir_vars) = ir::generate_ir(&ast, &reserved_names);
        let ass = generate_assembly(ir, ir_vars).unwrap();

        assert!(!ass.is_empty());
    }
    #[test]
    fn conditonals_40_compiles() {
        assert_compiles("if true then { print_int(2); } else { print_int(3); }");
    }

    #[test]
    fn assignment_of_fun_compiles() {
        assert_compiles("var x = print_int; x(1)");
    }
}
