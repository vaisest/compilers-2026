mod generator;
mod parser;
mod tokenizer;
mod typecheck;

pub fn compile(source_code: &str, _file_name: Option<String>) -> Vec<u8> {
    let out = vec![];
    let tokens = tokenizer::tokenize(source_code);

    let ast = parser::parse(tokens);
    assert!(
        ast.is_ok(),
        "Failed to parse tokens into AST: {:?}",
        ast.unwrap_err()
    );
    let mut ast = ast.unwrap();

    let (typecheck_res, reserved_names) = typecheck::typecheck(&mut ast);
    assert!(
        typecheck_res.is_ok(),
        "Failed to type check AST: {:?}",
        typecheck_res.unwrap_err()
    );
    generator::generate_ir(&ast, &reserved_names);
    out
}
