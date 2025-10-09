use clap::{Parser, Subcommand};
use std::{fs, process};

#[derive(Parser)]
#[command(name = "mutica")]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Run Mutica file
    Run {
        /// Code file path
        file: String,
    },
}

fn main() {
    let cli = Cli::parse();
    match cli.command {
        Command::Run { file } => {
            let code = match fs::read_to_string(&file) {
                Ok(c) => c,
                Err(e) => {
                    eprintln!("Failed to read file '{}': {}", file, e);
                    process::exit(1);
                }
            };
            // Normalize line endings to \n to avoid Ariadne position calculation issues
            // when handling UTF-8 multi-byte characters with \r\n line endings
            let normalized_code = code.replace("\r\n", "\n");
            mutica_run(&normalized_code);
        }
    }
}

fn mutica_run(code: &str) {
    mutica::parse_and_reduce(code);
}
