use std::{
    collections::HashMap,
    fmt::{Debug, Display, Pointer},
};

use itertools::Itertools;
use std::string::ToString;
use strum::VariantNames;

use crate::compiler::{
    parser::{BinaryOp, Expr, ExprKind},
    tokenizer::CodeLoc,
};

#[derive(Clone)]
struct IRVar {
    name: String,
}
impl IRVar {
    fn unit() -> Self {
        Self {
            name: "unit".into(),
        }
    }
}

impl Debug for IRVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name.clone())
    }
}

impl Display for IRVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name.clone())
    }
}

struct Instruction {
    location: CodeLoc,
    instruction: InstructionKind,
}

#[derive(Clone)]
struct Label {
    name: String,
}

impl Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Label({})", self.name)
    }
}

#[derive(strum_macros::Display)]
enum InstructionKind {
    #[strum(to_string = "LoadBoolConst({value}, {dest})")]
    LoadBoolConst { value: bool, dest: IRVar },
    #[strum(to_string = "LoadIntConst({value}, {dest})")]
    LoadIntConst { value: i64, dest: IRVar },
    #[strum(to_string = "Copy({source}, {dest})")]
    Copy { source: IRVar, dest: IRVar },
    #[strum(to_string = "Call({func}, {args:?}, {dest})")]
    Call {
        func: IRVar,
        args: Vec<IRVar>,
        dest: IRVar,
    },
    #[strum(to_string = "Jump({label})")]
    Jump { label: Label },
    #[strum(to_string = "CondJump({cond}, {then_label}, {else_label})")]
    CondJump {
        cond: IRVar,
        then_label: Label,
        else_label: Label,
    },
}

pub enum IR {
    Instruction(Instruction),
    Label(Label),
}

impl IR {
    fn instr_with_loc(loc: CodeLoc, instruction: InstructionKind) -> Self {
        Self::Instruction(Instruction {
            location: loc,
            instruction,
        })
    }
}

impl Display for IR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IR::Instruction(instr) => instr.fmt(f),
            IR::Label(label) => label.fmt(f),
        }
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.instruction.to_string())
    }
}

struct IrGenerator {
    symbols: Vec<HashMap<String, IRVar>>,
    instructions: Vec<IR>,
    var_counter: usize,
    label_counter: usize,
}
impl IrGenerator {
    fn new() -> Self {
        let mut out = Self {
            symbols: vec![HashMap::new()],
            instructions: vec![],
            var_counter: 1,
            label_counter: 1,
        };

        for op_name in BinaryOp::VARIANTS {
            let var = IRVar {
                name: op_name.to_string(),
            };
            out.symbols[0].insert(op_name.to_string(), var);
        }
        let var = IRVar {
            name: "print_int".to_string(),
        };
        out.symbols[0].insert("print_int".to_string(), var);

        out
    }
    fn new_var(&mut self) -> IRVar {
        let out = IRVar {
            name: format!(
                "x{}",
                if self.var_counter > 1 {
                    self.var_counter.to_string()
                } else {
                    String::new()
                }
            ),
        };
        self.var_counter += 1;
        out
    }
    fn new_label(&mut self) -> Label {
        let out = Label {
            name: format!(
                "x{}",
                if self.label_counter > 1 {
                    self.label_counter.to_string()
                } else {
                    String::new()
                }
            ),
        };
        self.label_counter += 1;
        out
    }
    fn get_symbol(&self, name: &str, depth: usize) -> Result<IRVar, String> {
        // try to look up identifier with decreasing depth
        self.symbols[0..=depth]
            .iter()
            .rev()
            .find_map(|map| map.get(name).cloned())
            .ok_or_else(|| format!("Could not find type of local {name}. Is it not defined yet?"))
    }
    fn visit(&mut self, depth: usize, expr: &Expr) -> IRVar {
        let loc = expr.loc;

        match &expr.kind {
            ExprKind::Literal(value) => {
                let dest = self.new_var();
                self.instructions.push(IR::instr_with_loc(
                    loc,
                    InstructionKind::LoadIntConst {
                        value: *value,
                        dest: dest.clone(),
                    },
                ));
                dest
            }
            ExprKind::LiteralBool(value) => {
                let dest = self.new_var();
                self.instructions.push(IR::instr_with_loc(
                    loc,
                    InstructionKind::LoadBoolConst {
                        value: *value,
                        dest: dest.clone(),
                    },
                ));
                dest
            }
            ExprKind::Identifier(name) => {
                // TODO: handle err
                self.get_symbol(name, depth).unwrap()
            }
            ExprKind::Binary(op, lhs, rhs) => {
                let op_var = self.symbols[0].get(&op.to_string()).unwrap().clone();
                let lhs_var = self.visit(depth, lhs.as_ref());
                let rhs_var = self.visit(depth, rhs.as_ref());
                let dest = self.new_var();
                self.instructions.push(IR::instr_with_loc(
                    loc,
                    InstructionKind::Call {
                        func: op_var,
                        args: vec![lhs_var, rhs_var],
                        dest: dest.clone(),
                    },
                ));
                dest
            }
            ExprKind::Function(name, args) => {
                let args = args
                    .iter()
                    .map(|expr| self.visit(depth, expr))
                    .collect_vec();
                let func = self.get_symbol(name, depth).unwrap();
                let dest = self.new_var();
                self.instructions.push(IR::instr_with_loc(
                    loc,
                    InstructionKind::Call {
                        func,
                        args,
                        dest: dest.clone(),
                    },
                ));
                dest
            }
            ExprKind::If(cond, then, otherwise) => {
                let then_label = self.new_label();
                let otherwise_label = self.new_label();
                let end_label = self.new_label();

                // emit loading condition loading instructions
                let cond = self.visit(depth, cond.as_ref());

                // emit jump to either then or else
                self.instructions.push(IR::instr_with_loc(
                    loc,
                    InstructionKind::CondJump {
                        cond,
                        then_label: then_label.clone(),
                        else_label: otherwise_label.clone(),
                    },
                ));

                // emit then label and then block instrs
                self.instructions.push(IR::Label(then_label));
                self.visit(depth + 1, then.as_ref());

                // if there is an else block emit instruction to skip over it,
                // and emit its label, and the block contents
                if let Some(otherwise) = otherwise {
                    self.instructions.push(IR::instr_with_loc(
                        loc,
                        InstructionKind::Jump {
                            label: end_label.clone(),
                        },
                    ));
                    self.instructions.push(IR::Label(otherwise_label));
                    self.visit(depth + 1, otherwise.as_ref());
                }
                self.instructions.push(IR::Label(end_label));

                IRVar::unit()
            }
            ExprKind::While(cond, then) => {
                let cond_label = self.new_label();
                let then_label = self.new_label();
                let end_label = self.new_label();

                self.instructions.push(IR::Label(cond_label.clone()));
                let cond = self.visit(depth, cond.as_ref());
                self.instructions.push(IR::instr_with_loc(
                    loc,
                    InstructionKind::CondJump {
                        cond,
                        then_label: then_label.clone(),
                        else_label: end_label.clone(),
                    },
                ));

                // do block
                self.instructions.push(IR::Label(then_label));

                self.visit(depth, &then);
                self.instructions.push(IR::instr_with_loc(
                    loc,
                    InstructionKind::Jump { label: cond_label },
                ));

                self.instructions.push(IR::Label(end_label));

                IRVar::unit()
            }
            ExprKind::Local(name, rhs) => {
                let dest = self.new_var();
                let value = self.visit(depth, rhs.as_ref());
                self.instructions.push(IR::instr_with_loc(
                    loc,
                    InstructionKind::Copy {
                        source: value,
                        dest: dest.clone(),
                    },
                ));
                dest
            }
            ExprKind::Unary(op, rhs) => {
                let dest = self.new_var();
                let value = self.visit(depth, rhs.as_ref());
                let func = self.get_symbol(&op.to_string(), depth).unwrap();
                self.instructions.push(IR::instr_with_loc(
                    loc,
                    InstructionKind::Call {
                        func,
                        args: vec![value],
                        dest: dest.clone(),
                    },
                ));
                dest
            }
            ExprKind::Block(exprs, returns_last) => {
                let dests = exprs
                    .iter()
                    .map(|expr| self.visit(depth + 1, expr))
                    .collect_vec();
                if *returns_last {
                    dests.last().unwrap().clone()
                } else {
                    IRVar::unit()
                }
            }
        }
    }
}

pub fn generate_ir(ast: &Expr, _reserved_names: &[String]) -> Vec<IR> {
    let mut generator = IrGenerator::new();
    generator.visit(1, ast);
    generator.instructions
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::compiler::{
        generator::generate_ir,
        parser::{Expr, parse},
        tokenizer::tokenize,
        typecheck::typecheck,
    };

    fn prepare(source_code: &str) -> (Expr, Vec<String>) {
        let mut ast = parse(tokenize(source_code)).unwrap();
        let (tr, reserved_names) = typecheck(&mut ast);
        tr.unwrap();
        (ast, reserved_names)
    }

    fn assert_ir_eq(source_code: &str, goal: &str) {
        let (ast, reserved_names) = prepare(source_code);
        let ir = generate_ir(&ast, &reserved_names);
        let text = ir.iter().join("\n");

        assert_eq!(&text, goal);
    }

    #[test]
    fn basic_operators_work() {
        assert_ir_eq(
            "1+2",
            "LoadIntConst(1, x)
LoadIntConst(2, x2)
Call(+, [x, x2], x3)
Call(print_int, [x3], x4)",
        );
    }
}
