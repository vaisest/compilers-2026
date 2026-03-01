use std::collections::HashMap;

use itertools::Itertools;

use crate::compiler::parser::{BinaryOp, Expr, ExprKind};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Bool,
    Unit,
    // in, out
    Func(Vec<Type>, Box<Type>),
}

impl From<&str> for Type {
    fn from(value: &str) -> Self {
        match value {
            "int" => Type::Int,
            "bool" => Type::Bool,
            "()" => Type::Unit,
            _ => todo!(),
        }
    }
}

// exists for test use. Parses functions that are like:
// +: Int, Int -> Int
fn parse_func_signature(sig: &str) -> (String, Type) {
    let (name, rest) = sig.split_once(':').unwrap();
    let (input, output) = rest.split_once("->").unwrap();
    let output_type = Type::from(output.trim());

    let input_types = input.split(',').map(|v| Type::from(v.trim())).collect_vec();

    (
        name.trim().to_string(),
        Type::Func(input_types, Box::new(output_type)),
    )
}

// fn get_binary_func_type

pub struct TypeChecker {
    // TODO: change hashmap type. The default rust hashmap is very slow for ddos
    // reasons which are not relevant here

    // vec indices correspond to block depth. locals of depth zero are globals
    locals: Vec<HashMap<String, Type>>,
}

impl TypeChecker {
    pub fn new() -> Self {
        let mut out = Self {
            locals: vec![HashMap::new()],
        };

        let func_types = [
            "Add: int, int -> int",
            "Mul: int, int -> int",
            "Sub: int, int -> int",
            "Div: int, int -> int",
            "Rem: int, int -> int",
            "Or: bool, bool -> bool",
            "And: bool, bool -> bool",
            "Lt: int, int -> bool",
            "Gt: int, int -> bool",
            "Leq: int, int -> bool",
            "Geq: int, int -> bool",
            "print_int: int -> ()",
        ];
        for (name, type_) in func_types.map(parse_func_signature) {
            out.locals[0].insert(name, type_);
        }

        out
    }

    fn get_symbol(&self, name: &str, depth: usize) -> Result<&Type, String> {
        // try to look up identifier with decreasing depth
        self.locals[0..=depth]
            .iter()
            .rev()
            .find_map(|map| map.get(name))
            .ok_or_else(|| format!("Could not find type of local {name}. Is it not defined yet?"))
    }

    pub fn typecheck(&mut self, node: &Expr, depth: usize) -> Result<Type, String> {
        if depth >= self.locals.len() {
            self.locals.push(HashMap::new());
        }
        let res = match &node.kind {
            ExprKind::Literal(_) => Type::Int,
            ExprKind::Local(name, rhs) => {
                let rhs_type = self.typecheck(rhs.as_ref(), depth)?;
                dbg!(name, &depth);
                self.locals[depth].insert(name.clone(), rhs_type.clone());
                rhs_type
            }
            ExprKind::Binary(bin_op, lhs, rhs) => {
                let t2 = self.typecheck(rhs, depth)?;
                match bin_op {
                    // equality and assignment are handled as exceptions
                    BinaryOp::Eq | BinaryOp::Neq => {
                        let t1 = self.typecheck(lhs, depth)?;
                        if t1 == t2 {
                            Type::Bool
                        } else {
                            return Err(format!(
                                "Expected types of lhs and rhs of == or != to be equal. Instead found {t1:?} and {t2:?}"
                            ));
                        }
                    }
                    BinaryOp::Assign => {
                        let ExprKind::Identifier(var_name) = &lhs.kind else {
                            unreachable!()
                        };
                        self.locals[depth].insert(var_name.clone(), t2.clone());
                        t2
                    }
                    // otherwise operator type signatures are fetched from the
                    // global table
                    _ => {
                        dbg!(bin_op.to_string());
                        let t1 = self.typecheck(lhs.as_ref(), depth)?;
                        let Type::Func(expected_inputs, output_type) =
                            self.locals[0].get(&bin_op.to_string()).unwrap()
                        else {
                            unreachable!()
                        };

                        if t1 != expected_inputs[0] || t2 != expected_inputs[1] {
                            return Err(format!(
                                "Received incorrect input types for operator {}. Expected {:?} and {:?}. Instead found {t1:?} and {t2:?}",
                                bin_op, expected_inputs[0], expected_inputs[1]
                            ));
                        }

                        (**output_type).clone()
                    }
                }
            }

            ExprKind::Unary(op, rhs) => {
                let t1 = self.typecheck(rhs, depth)?;
                let Type::Func(expected_inputs, output_type) =
                    self.locals[0].get(&op.to_string()).unwrap()
                else {
                    unreachable!()
                };

                if t1 != expected_inputs[0] {
                    return Err(format!(
                        "Received incorrect input type for operator {}. Expected {:?}. Instead found {t1:?}",
                        op, expected_inputs[0],
                    ));
                }

                *output_type.clone()
            }
            ExprKind::Function(name, inputs) => {
                // currently we have no way to define functions, so they are
                // always globals
                let Type::Func(expected_inputs, output_type) =
                    self.get_symbol(name, depth)?.clone()
                else {
                    unreachable!()
                };

                for (act, exp) in inputs.iter().zip(expected_inputs.iter()) {
                    if self.typecheck(act, depth)? != *exp {
                        return Err(format!(
                            "Incorrect input types for function {name}. Expected {inputs:?}. Instead found {expected_inputs:?}"
                        ));
                    }
                }

                *output_type.clone()
            }
            ExprKind::Block(exprs, return_last) => {
                let mut last_type = Type::Unit;
                for expr in exprs {
                    last_type = self.typecheck(expr, depth + 1)?;
                }
                let ret_type = if *return_last { last_type } else { Type::Unit };
                self.locals[depth + 1].clear();
                ret_type
            }
            ExprKind::If(cond, then, otherwise) => {
                let if_type = self.typecheck(cond.as_ref(), depth)?;
                if if_type != Type::Bool {
                    return Err(format!(
                        "Expected bool as if condition. Instead found {if_type:?}"
                    ));
                }

                let then_type = self.typecheck(then.as_ref(), depth + 1)?;
                self.locals[depth + 1].clear();
                if let Some(otherwise) = otherwise {
                    let otherwise_type = self.typecheck(otherwise.as_ref(), depth + 1)?;
                    self.locals[depth + 1].clear();
                    if otherwise_type != then_type {
                        return Err(format!(
                            "Expected then and otherwise block types to match in if clause. Instead found {then_type:?} and {otherwise_type:?}"
                        ));
                    }
                }
                then_type
            }
            ExprKind::While(cond, then) => {
                let if_type = self.typecheck(cond.as_ref(), depth)?;
                if if_type != Type::Bool {
                    return Err(format!(
                        "Expected bool as if condition. Instead found {if_type:?}"
                    ));
                }

                let then_type = self.typecheck(then.as_ref(), depth + 1)?;
                self.locals[depth + 1].clear();
                then_type
            }
            ExprKind::Identifier(name) => self.get_symbol(name, depth)?.clone(),
        };
        Ok(res)
    }
}

pub fn typecheck(input: &Expr) -> Result<Type, String> {
    let mut checker = TypeChecker::new();
    checker.typecheck(input, 0)
}

#[cfg(test)]
mod tests {
    use crate::compiler::{parser::parse, tokenizer::tokenize};

    use super::*;
    use pretty_assertions::assert_eq;

    fn parse_source(source_code: &str) -> Expr {
        parse(tokenize(source_code)).unwrap()
    }

    #[test]
    fn basic_operators_work() {
        assert!(typecheck(&parse_source("1+1")).is_ok_and(|v| v == Type::Int));
        assert_eq!(typecheck(&parse_source("var a = 2;1+a")), Ok(Type::Int));

        assert!(typecheck(&parse_source("1+a")).is_err());
    }

    #[test]
    fn function_signatures_work() {
        assert_eq!(
            typecheck(&parse_source(
                "{ var f: (Int) => Unit = print_int; f(123) }"
            )),
            Ok(Type::Unit)
        );

        assert_eq!(
            typecheck(&parse_source("{ var f: (Int) => Unit = print_int; f }")),
            Ok(Type::Func(vec![Type::Int], Box::new(Type::Unit)))
        );
    }
}
