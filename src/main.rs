use base64::{Engine, engine::general_purpose};
use compiler::compile;
use serde::Deserialize;
use std::{
    io::Write,
    net::{IpAddr, SocketAddr, TcpListener, TcpStream},
    str::FromStr,
};

use clap::{Parser, Subcommand};
use serde_json::Error;

mod compiler;

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

fn handle(mut stream: TcpStream) -> Result<(), Error> {
    let mut de = serde_json::Deserializer::from_reader(&stream);
    let req = Request::deserialize(&mut de)?;

    match req.command.as_str() {
        "ping" => {
            stream
                .write_all("{}".as_bytes())
                .expect("failed to write response");
        }
        "compile" => {
            let out = compile(req.code.unwrap(), Some("(source code)".to_string()));
            stream
                .write_all(general_purpose::STANDARD.encode(out.as_slice()).as_bytes())
                .expect("failed to write response");
        }
        _ => unimplemented!("Unknown command"),
    }
    Ok(())
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Compile {
            output: _,
            input_file: _,
        } => {
            todo!()
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
