use itertools::Itertools;

use crate::compiler::generator::{IR, IRVar, Instruction, InstructionKind};
use std::fmt::Write;
use std::{collections::HashMap, fmt::write};

struct Locals {
    var_location_map: HashMap<IRVar, String>,
    stack_used: usize,
}
impl Locals {
    fn new_from_vars(vars: Vec<IRVar>) -> Self {
        let mut map = HashMap::new();
        let mut stack_used = 0;
        for (idx, var) in vars.into_iter().enumerate() {
            map.insert(var, format!("-{}(%rbp)", 8 + 8 * idx));
            stack_used += 8;
        }
        Self {
            var_location_map: map,
            stack_used,
        }
    }

    fn get_ref(&self, v: &IRVar) -> Option<&String> {
        self.var_location_map.get(v)
    }
}

pub fn generate_assembly(ir: Vec<IR>, vars: Vec<IRVar>) -> String {
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
                    let dest_loc = locals.get_ref(&dest).unwrap();
                    writeln!(output, "movq ${}, {dest_loc}", u8::from(value)).unwrap();
                }
                InstructionKind::LoadIntConst { value, dest } => {
                    let dest_loc = locals.get_ref(&dest).unwrap();
                    if value >= -2i64.pow(31) && value < 2i64.pow(31) {
                        writeln!(output, "movq ${value}, {dest_loc}").unwrap();
                    } else {
                        writeln!(output, "movabsq ${value}, %rax").unwrap();
                        writeln!(output, "movq %rax, {dest_loc}").unwrap();
                    }
                }
                InstructionKind::Copy { source, dest } => {
                    let src_loc = locals.get_ref(&source).unwrap();
                    let dst_loc = locals.get_ref(&dest).unwrap();
                    writeln!(output, "movq {src_loc}, %rax").unwrap();
                    writeln!(output, "movq %rax, {dst_loc}").unwrap();
                }
                InstructionKind::CondJump {
                    cond,
                    then_label,
                    else_label,
                } => {
                    let cond_loc = locals.get_ref(&cond).unwrap();
                    writeln!(output, "cmpq $0, {cond_loc}").unwrap();
                    writeln!(output, "jne .L{}", then_label.name).unwrap();
                    writeln!(output, "jmp .L{}", else_label.name).unwrap();
                }
                InstructionKind::Jump { label } => {
                    writeln!(output, "jmp .L{}", label.name).unwrap();
                }
                InstructionKind::Call { func, args, dest } => {
                    let dest_loc = locals.get_ref(&dest).unwrap();
                    let arg_locs = args
                        .iter()
                        .map(|v| locals.get_ref(v).unwrap())
                        .collect_vec();
                    let assembly = match func.name.as_str() {
                        "Add" => {
                            format!(
                                "movq {}, %rax
                                add {}, %rax
                                movq %rax, {dest_loc}",
                                arg_locs[0], arg_locs[1]
                            )
                        }
                        "Mul" => {
                            format!(
                                "movq {}, %rax
                                imul {}, %rax
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
                                and {} %rax
                                movq %rax, {dest_loc}",
                                arg_locs[0], arg_locs[1]
                            )
                        }
                        op @ ("Lt" | "Gt" | "Leq" | "Geq") => {
                            let op = match op {
                                "Lt" => "l",
                                "Gt" => "g",
                                "Leq" => "le",
                                "Geq" => "ge",
                                _ => unreachable!(),
                            };
                            format!(
                                "xor %rax, %rax
                                movq {}, %rdx
                                cmpq {}, %rdx
                                set{op}
                                movq %rax, {}",
                                arg_locs[0], arg_locs[1], dest_loc
                            )
                        }
                        "read_int" => {
                            format!(
                                "callq print_int
                            movq %rax, {dest_loc}"
                            )
                        }
                        "print_int" => {
                            format!(
                                "movq {}, %rdi
                            callq print_int",
                                arg_locs[0]
                            )
                        }
                        _ => unimplemented!(),
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
    output
}
