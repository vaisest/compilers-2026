mod parser;
mod tokenizer;
mod typecheck;

pub fn compile(source_code: &str, _file_name: Option<String>) -> Vec<u8> {
    let out = vec![];
    let tokens = tokenizer::tokenize(source_code);
    let ast = parser::parse(tokens);
    let _typecheck = typecheck::typecheck(&ast.unwrap());
    out
}
