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

    fn get_reserved_names(&self) -> Vec<String> {
        self.locals[0].keys().cloned().collect_vec()
    }

    pub fn typecheck(&mut self, node: &mut Expr, depth: usize) -> Result<Type, String> {
        if depth >= self.locals.len() {
            self.locals.push(HashMap::new());
        }
        let res = match &mut node.kind {
            ExprKind::Literal(_) => Type::Int,
            ExprKind::LiteralBool(_) => Type::Bool,
            ExprKind::Local(name, rhs) => {
                let rhs_type = self.typecheck(rhs.as_mut(), depth)?;

                if let Some(t) = &node.type_
                    && *t != rhs_type
                {
                    return Err(format!(
                        "Variable assignment does not match variable type declaration. Expected {t:?}. Instead found {rhs_type:?}"
                    ));
                }
                self.locals[depth].insert(name.clone(), rhs_type.clone());
                rhs_type
            }
            ExprKind::Binary(bin_op, lhs, rhs) => {
                let rhs_type = self.typecheck(rhs.as_mut(), depth)?;
                match bin_op {
                    // equality and assignment are handled as exceptions
                    BinaryOp::Eq | BinaryOp::Neq => {
                        let lhs_type = self.typecheck(lhs.as_mut(), depth)?;
                        if lhs_type == rhs_type {
                            Type::Bool
                        } else {
                            return Err(format!(
                                "Expected types of lhs and rhs of == or != to be equal. Instead found {lhs_type:?} and {rhs_type:?}"
                            ));
                        }
                    }
                    BinaryOp::Assign => {
                        let ExprKind::Identifier(var_name) = &lhs.kind else {
                            unreachable!()
                        };
                        let var_type = self.get_symbol(var_name, depth)?;
                        if *var_type != rhs_type {
                            return Err(format!(
                                "Assignment type does not match variable type. Expected {var_type:?}. Instead found {rhs_type:?}."
                            ));
                        }
                        rhs_type
                    }
                    // otherwise operator type signatures are fetched from the
                    // global table
                    _ => {
                        let lhs_type = self.typecheck(lhs.as_mut(), depth)?;
                        let Type::Func(expected_inputs, output_type) =
                            self.locals[0].get(&bin_op.to_string()).unwrap()
                        else {
                            unreachable!()
                        };

                        if lhs_type != expected_inputs[0] || rhs_type != expected_inputs[1] {
                            return Err(format!(
                                "Received incorrect input types for operator {}. Expected {:?} and {:?}. Instead found {lhs_type:?} and {rhs_type:?}",
                                bin_op, expected_inputs[0], expected_inputs[1]
                            ));
                        }

                        (**output_type).clone()
                    }
                }
            }

            ExprKind::Unary(op, rhs) => {
                let t1 = self.typecheck(rhs.as_mut(), depth)?;
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

                for (act, exp) in inputs.iter_mut().zip(expected_inputs.iter()) {
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
                for expr in exprs.iter_mut() {
                    last_type = self.typecheck(expr, depth + 1)?;
                }
                let ret_type = if *return_last { last_type } else { Type::Unit };
                self.locals[depth + 1].clear();
                ret_type
            }
            ExprKind::If(cond, then, otherwise) => {
                let if_type = self.typecheck(cond.as_mut(), depth)?;
                if if_type != Type::Bool {
                    return Err(format!(
                        "Expected bool as if condition. Instead found {if_type:?}"
                    ));
                }

                let then_type = self.typecheck(then.as_mut(), depth + 1)?;
                self.locals[depth + 1].clear();
                if let Some(otherwise) = otherwise {
                    let otherwise_type = self.typecheck(otherwise.as_mut(), depth + 1)?;
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
                let if_type = self.typecheck(cond.as_mut(), depth)?;
                if if_type != Type::Bool {
                    return Err(format!(
                        "Expected bool as if condition. Instead found {if_type:?}"
                    ));
                }

                let then_type = self.typecheck(then.as_mut(), depth + 1)?;
                self.locals[depth + 1].clear();
                then_type
            }
            ExprKind::Identifier(name) => self.get_symbol(name, depth)?.clone(),
        };
        node.type_.replace(res.clone());
        Ok(res)
    }
}

pub fn typecheck(input: &mut Expr) -> (Result<Type, String>, Vec<String>) {
    let mut checker = TypeChecker::new();
    (checker.typecheck(input, 0), checker.get_reserved_names())
}

#[cfg(test)]
mod tests {
    use crate::compiler::{parser::parse, tokenizer::tokenize};

    use super::*;
    use pretty_assertions::assert_eq;

    fn parse_source(source_code: &str) -> Expr {
        parse(tokenize(source_code)).unwrap()
    }

    #[allow(clippy::needless_pass_by_value)]
    fn check_and_assert_eq(source_code: &str, goal: Type) {
        let mut ast = parse_source(source_code);
        let _ = typecheck(&mut ast);
        assert_eq!(ast.type_.unwrap(), goal);
    }

    fn check_and_assert_err(source_code: &str) {
        let mut ast = parse_source(source_code);
        assert!(typecheck(&mut ast).0.is_err());
    }

    #[test]
    fn basic_operators_work() {
        check_and_assert_eq("1+1", Type::Int);
        check_and_assert_eq("var a = 2;1+a", Type::Int);
        check_and_assert_eq("var x = false; x or true", Type::Bool);

        check_and_assert_err("1+a");
        // check_and_assert_err("var a: Bool = 0;1<a");
    }

    #[test]
    fn function_signatures_work() {
        check_and_assert_eq("{ var f: (Int) => Unit = print_int; f(123) }", Type::Unit);

        check_and_assert_eq(
            "{ var f: (Int) => Unit = print_int; f }",
            Type::Func(vec![Type::Int], Box::new(Type::Unit)),
        );
    }

    #[test]
    fn local_type_checking_works() {
        check_and_assert_err("{ var f: (Int) => Int = print_int; f }");
        check_and_assert_err("var x: Int = 0; x = x < 2");
        check_and_assert_eq("var x: Bool = false; x = 1 < 2; x", Type::Bool);
    }
}
