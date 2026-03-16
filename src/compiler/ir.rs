use std::{
    collections::HashMap,
    fmt::{Debug, Display},
};

use itertools::Itertools;
use std::string::ToString;
use strum::VariantNames;

use crate::compiler::{
    parser::{BinaryOp, Expr, ExprKind, UnaryOp},
    tokenizer::CodeLoc,
    typecheck::Type,
};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct IRVar {
    pub name: String,
    pub aliases_func: Option<String>,
}
impl IRVar {
    fn unit() -> Self {
        Self {
            name: "unit".into(),
            aliases_func: None,
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

pub struct Instruction {
    #[allow(dead_code)] // i probably should have used it in error messages
    pub location: CodeLoc,
    pub instruction: InstructionKind,
}

#[derive(Clone)]
pub struct Label {
    pub name: String,
}

#[derive(PartialEq, Eq, Hash, strum_macros::Display, Copy, Clone)]
#[strum(serialize_all = "snake_case")]
enum LabelKind {
    Then,
    Else,
    WhileStart,
    IfEnd,
    WhileEnd,
    WhileBody,
}

impl Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Label({})", self.name)
    }
}

#[derive(strum_macros::Display)]
pub enum InstructionKind {
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
        write!(f, "{}", self.instruction)
    }
}

pub fn wrap_print_call(input: Expr) -> Expr {
    let type_ = input.type_.as_ref().expect(
        "expression has no type. this function should only be called after type checking the AST.",
    );
    match *type_ {
        Type::Int => Expr::with_type(
            ExprKind::Function("print_int".into(), vec![input]),
            Type::Unit,
        ),
        Type::Bool => Expr::with_type(
            ExprKind::Function("print_bool".into(), vec![input]),
            Type::Unit,
        ),
        _ => input,
    }
}

struct IrGenerator {
    symbols: Vec<HashMap<String, IRVar>>,
    instructions: Vec<IR>,
    var_counter: usize,
    label_counters: HashMap<LabelKind, usize>,
    current_depth: usize,
    all_vars: Vec<IRVar>,
}
impl IrGenerator {
    fn new() -> Self {
        let mut out = Self {
            symbols: vec![HashMap::new()],
            instructions: vec![],
            var_counter: 1,
            label_counters: HashMap::new(),
            current_depth: 0,
            // this isn't really a value you'd ever need, but this keeps
            // assembly generation much simpler as we can just treat unit as a
            // throwaway variable
            all_vars: vec![IRVar::unit()],
        };

        for op_name in BinaryOp::VARIANTS.iter().chain(UnaryOp::VARIANTS) {
            let var = IRVar {
                name: op_name.to_string(),
                aliases_func: Some(op_name.to_string()),
            };
            out.symbols[0].insert(op_name.to_string(), var.clone());
            out.all_vars.push(var);
        }
        for std_func in ["print_int", "print_bool", "read_int"] {
            let var = IRVar {
                name: std_func.to_string(),
                aliases_func: Some(std_func.to_string()),
            };
            out.symbols[0].insert(std_func.to_string(), var.clone());
            out.all_vars.push(var);
        }

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
            aliases_func: None,
        };
        self.var_counter += 1;
        self.all_vars.push(out.clone());
        out
    }
    fn new_label(&mut self, kind: LabelKind) -> Label {
        let count = self.label_counters.entry(kind).or_insert(1);
        let out = Label {
            name: format!(
                "{}{}",
                kind,
                if *count > 1 {
                    count.to_string()
                } else {
                    String::new()
                }
            ),
        };
        *count += 1;
        out
    }
    fn get_symbol(&self, name: &str) -> Result<IRVar, String> {
        // try to look up identifier with decreasing depth
        for (idx, locals) in self.symbols.iter().enumerate().rev() {
            if idx > self.current_depth {
                continue;
            }
            if let Some(res) = locals.get(name) {
                return Ok(res.clone());
            }
        }
        Err(format!(
            "Could not find type of local {name}. Is it not defined yet?"
        ))
    }
    fn add_symbol(&mut self, name: String, var: IRVar) {
        self.symbols
            .get_mut(self.current_depth)
            .unwrap_or_else(|| {
                panic!(
                    "No locals for depth {} initialised when adding symbol {name}.",
                    self.current_depth
                )
            })
            .insert(name, var);
    }
    fn visit(&mut self, expr: &Expr) -> IRVar {
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
            ExprKind::Identifier(name) => self.get_symbol(name).unwrap(),
            ExprKind::Binary(op, lhs, rhs) => {
                if *op == BinaryOp::Assign {
                    let rhs_var = self.visit(rhs.as_ref());
                    let dest = self.visit(lhs.as_ref());
                    self.instructions.push(IR::instr_with_loc(
                        loc,
                        InstructionKind::Copy {
                            source: rhs_var,
                            dest: dest.clone(),
                        },
                    ));
                    dest
                // to implement short-circuiting we transform or operators to if-else statements
                } else if *op == BinaryOp::Or {
                    self.visit(&Expr::with_type(
                        ExprKind::If(lhs.clone(), lhs.clone(), Some(rhs.clone())),
                        expr.type_.clone().unwrap(),
                    ))
                } else if *op == BinaryOp::And {
                    self.visit(&Expr::with_type(
                        ExprKind::If(lhs.clone(), rhs.clone(), None),
                        expr.type_.clone().unwrap(),
                    ))
                } else {
                    let op_var = self.symbols[0].get(&op.to_string()).unwrap().clone();
                    let lhs_var = self.visit(lhs.as_ref());
                    let rhs_var = self.visit(rhs.as_ref());
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
            }
            ExprKind::Function(name, args) => {
                let args = args.iter().map(|expr| self.visit(expr)).collect_vec();
                let func = self.get_symbol(name).unwrap();
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
                let then_label = self.new_label(LabelKind::Then);
                let otherwise_label = self.new_label(LabelKind::Else);
                let end_label = self.new_label(LabelKind::IfEnd);

                // emit loading condition loading instructions
                let cond = self.visit(cond.as_ref());
                let dest = self.new_var();

                // emit jump to either then or else
                self.instructions.push(IR::instr_with_loc(
                    loc,
                    InstructionKind::CondJump {
                        cond,
                        then_label: then_label.clone(),
                        else_label: if otherwise.is_some() {
                            otherwise_label.clone()
                        } else {
                            end_label.clone()
                        },
                    },
                ));

                // emit then label and then block instrs
                self.instructions.push(IR::Label(then_label));
                let then_res = self.visit(then.as_ref());
                // and copy result
                self.instructions.push(IR::instr_with_loc(
                    loc,
                    InstructionKind::Copy {
                        source: then_res,
                        dest: dest.clone(),
                    },
                ));

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
                    let otherwise_res = self.visit(otherwise.as_ref());
                    // and copy result
                    self.instructions.push(IR::instr_with_loc(
                        loc,
                        InstructionKind::Copy {
                            source: otherwise_res,
                            dest: dest.clone(),
                        },
                    ));
                }
                self.instructions.push(IR::Label(end_label));

                dest
            }
            ExprKind::While(cond, then) => {
                let cond_label = self.new_label(LabelKind::WhileStart);
                let then_label = self.new_label(LabelKind::WhileBody);
                let end_label = self.new_label(LabelKind::WhileEnd);

                self.instructions.push(IR::Label(cond_label.clone()));
                let cond = self.visit(cond.as_ref());
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

                self.visit(then);
                self.instructions.push(IR::instr_with_loc(
                    loc,
                    InstructionKind::Jump { label: cond_label },
                ));

                self.instructions.push(IR::Label(end_label));

                IRVar::unit()
            }
            ExprKind::Local(name, rhs) => {
                // handle function aliasing
                if expr
                    .type_
                    .as_ref()
                    .is_some_and(|v| matches!(v, Type::Func(..)))
                {
                    let ExprKind::Identifier(rhs_name) = &rhs.kind else {
                        unreachable!()
                    };
                    let var = IRVar {
                        name: name.clone(),
                        aliases_func: Some(rhs_name.clone()),
                    };
                    self.all_vars.push(var.clone());
                    self.add_symbol(name.clone(), var.clone());
                    var
                } else {
                    let value = self.visit(rhs.as_ref());
                    let dest = self.new_var();
                    self.add_symbol(name.clone(), dest.clone());
                    self.instructions.push(IR::instr_with_loc(
                        loc,
                        InstructionKind::Copy {
                            source: value,
                            dest: dest.clone(),
                        },
                    ));
                    dest
                }
            }
            ExprKind::Unary(op, rhs) => {
                let dest = self.new_var();
                let value = self.visit(rhs.as_ref());
                let func = self.get_symbol(&op.to_string()).unwrap();
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
                let dests = self.visit_block(exprs);
                if *returns_last {
                    dests.last().unwrap().clone()
                } else {
                    IRVar::unit()
                }
            }
        }
    }
    fn visit_block(&mut self, exprs: &[Expr]) -> Vec<IRVar> {
        self.current_depth += 1;
        self.symbols.push(HashMap::new());
        let res = exprs.iter().map(|v| self.visit(v)).collect_vec();
        self.symbols.pop();
        self.current_depth -= 1;
        res
    }
}

pub fn generate_ir(ast: &Expr, _reserved_names: &[String]) -> (Vec<IR>, Vec<IRVar>) {
    let ast = wrap_print_call(ast.clone());
    let mut generator = IrGenerator::new();
    generator.visit(&ast);
    (generator.instructions, generator.all_vars)
}

#[cfg(test)]
mod tests {
    use itertools::Itertools;

    use crate::compiler::{
        ir::generate_ir,
        parser::{Expr, parse},
        tokenizer::tokenize,
        typecheck::typecheck,
    };
    use pretty_assertions::assert_eq;

    fn prepare(source_code: &str) -> (Expr, Vec<String>) {
        let mut ast = parse(tokenize(source_code)).unwrap();
        let (tr, reserved_names) = typecheck(&mut ast);
        tr.unwrap();
        (ast, reserved_names)
    }

    fn assert_ir_eq(source_code: &str, goal: &str) {
        let (ast, reserved_names) = prepare(source_code);
        let (ir, _) = generate_ir(&ast, &reserved_names);
        let text = ir.iter().join("\n");

        assert_eq!(&text, goal);
    }

    #[test]
    fn basic_operators_work() {
        assert_ir_eq(
            "1+2",
            "LoadIntConst(1, x)
LoadIntConst(2, x2)
Call(Add, [x, x2], x3)
Call(print_int, [x3], x4)",
        );
    }

    #[test]
    fn func_call_works() {
        assert_ir_eq(
            "var a = 1;
var f = print_int;
f(a + 3)",
            "LoadIntConst(1, x)
Copy(x, x2)
LoadIntConst(3, x3)
Call(Add, [x2, x3], x4)
Call(f, [x4], x5)",
        );
    }

    #[test]
    fn if_then_else_works() {
        assert_ir_eq(
            "var a = 1;
var b = 1;
var f = print_int;
var g = print_int;
if a < b then f(a) else g(b)",
            "LoadIntConst(1, x)
Copy(x, x2)
LoadIntConst(1, x3)
Copy(x3, x4)
Call(Lt, [x2, x4], x5)
CondJump(x5, Label(then), Label(else))
Label(then)
Call(f, [x2], x7)
Copy(x7, x6)
Jump(Label(if_end))
Label(else)
Call(g, [x4], x8)
Copy(x8, x6)
Label(if_end)",
        );
    }

    #[test]
    fn while_works() {
        assert_ir_eq(
            "var a = 1;
while a < 4 do {
  a = a + 1;
}
a",
            "LoadIntConst(1, x)
Copy(x, x2)
Label(while_start)
LoadIntConst(4, x3)
Call(Lt, [x2, x3], x4)
CondJump(x4, Label(while_body), Label(while_end))
Label(while_body)
LoadIntConst(1, x5)
Call(Add, [x2, x5], x6)
Copy(x6, x2)
Jump(Label(while_start))
Label(while_end)
Call(print_int, [x2], x7)",
        );
    }

    #[test]
    fn implicit_print_works() {
        assert_ir_eq(
            "true",
            "LoadBoolConst(true, x)
Call(print_bool, [x], x2)",
        );

        assert_ir_eq(
            "1",
            "LoadIntConst(1, x)
Call(print_int, [x], x2)",
        );
    }
}

// TODO: tests for short-circuiting
