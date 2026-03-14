use std::{fs, process::Command};

use tempfile::NamedTempFile;

mod assembly;
mod generator;
mod parser;
mod tokenizer;
mod typecheck;

pub fn compile(source_code: &str, _file_name: Option<String>) -> Result<Vec<u8>, String> {
    let tokens = tokenizer::tokenize(source_code);

    let mut ast = parser::parse(tokens)?;

    let (typecheck_res, reserved_names) = typecheck::typecheck(&mut ast);
    typecheck_res?;
    let (ir, ir_vars) = generator::generate_ir(&ast, &reserved_names);
    let assembly = assembly::generate_assembly(ir, ir_vars)?;
    // println!("{assembly}"); // TODO: add as a cli option

    let file = NamedTempFile::new().expect("failed to create temp file");

    println!(
        "Writing assembly code to {:?}",
        file.path()
            .to_str()
            .expect("assembly file path is somehow not valid UTF-8")
    );
    fs::write(&file, assembly).expect("failed to write assembly file");

    // since the course doesn't have us write an assembler, we use the provided
    // python script to link and assemble use a binary
    let output = Command::new("python")
        .args([
            "assembler.py",
            file.path()
                .to_str()
                .expect("assembly file path is somehow not valid UTF-8"),
        ])
        .output()
        .map_err(|v| format!("python command error: {v}"))?;
    if output.status.success() {
        Ok(output.stdout)
    } else {
        Err(format!(
            "Python assembler program failed with status code {} and stderr:\n{}",
            output.status.code().expect("subprocess was terminated"),
            str::from_utf8(&output.stderr).unwrap()
        ))
    }
}
