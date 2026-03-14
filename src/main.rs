#![warn(clippy::pedantic)]
use base64::{Engine, engine::general_purpose};
use serde::Deserialize;
use std::{
    fs::{self, read_to_string},
    io::Write,
    net::{IpAddr, SocketAddr, TcpListener, TcpStream},
    str::FromStr,
};
mod compiler;

use clap::{Parser, Subcommand};
use serde_json::{json, to_writer};

use crate::compiler::compile;

#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Compile {
        #[arg(long, require_equals = true)]
        output: String,

        input_file: String,
    },
    Serve {
        #[arg(long, require_equals = true)]
        host: Option<IpAddr>,

        #[arg(long, require_equals = true)]
        port: Option<u16>,
    },
}

#[derive(Deserialize, Debug)]
struct Request {
    command: String,
    code: Option<String>,
}

fn handle(mut stream: TcpStream) -> Result<(), String> {
    let mut de = serde_json::Deserializer::from_reader(&stream);
    let req = Request::deserialize(&mut de).map_err(|v| v.to_string())?;

    match req.command.as_str() {
        "ping" => {
            stream
                .write_all("{}".as_bytes())
                .expect("failed to write response");
        }
        "compile" => {
            let compile_result = compiler::compile(
                &req.code.expect("no source code in request"),
                Some("(source code)".to_string()),
            );

            let json = match compile_result {
                Ok(program_bytes) => {
                    let b64_program = general_purpose::STANDARD.encode(program_bytes.as_slice());
                    json!({"program": b64_program})
                }
                Err(err) => json!({"error": format!("error compiling program:\n{err}")}),
            };

            let mut bytes = vec![];
            to_writer(&mut bytes, &json).unwrap();
            stream.write_all(&bytes).expect("failed to write response");
        }
        _ => stream
            .write_all("{\"error\": \"unknown command\"}".as_bytes())
            .expect("failed to write response"),
    }
    Ok(())
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Compile { output, input_file } => {
            let source_code = read_to_string(input_file).unwrap();
            let program = compile(&source_code, None).expect("compilation failure");
            println!("Writing program to {output}");
            fs::write(output, program).expect("failed to write program output");
        }
        Commands::Serve { host, port } => {
            let ip = host.unwrap_or(IpAddr::from_str("127.0.0.1").unwrap());
            let addr = SocketAddr::new(ip, port.unwrap_or(3000));
            let listener = TcpListener::bind(addr).expect("could not bind socket");
            println!("Bound to {addr:?}");

            for stream in listener.incoming() {
                handle(stream.expect("tcp connection failed")).expect("handler failed");
            }
        }
    }
}
