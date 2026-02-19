mod parser;
mod tokenizer;

pub fn compile(source_code: &str, _file_name: Option<String>) -> Vec<u8> {
    let out = vec![];
    let tokens = tokenizer::tokenize(source_code);
    let _ast = parser::parse(tokens);
    out
}
