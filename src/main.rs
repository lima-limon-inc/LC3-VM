use std::env;

mod virtual_machine;

use crate::virtual_machine::cpu::VM;
use thiserror::Error;

#[derive(Error, Debug)]
enum CLIError {
    #[error(
        "Missing arguments. The project should be run like\n
cargo run path/to/.obj T|F"
    )]
    MissignArguments,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    let executable_path = args.get(1).ok_or(CLIError::MissignArguments)?;
    let debug = matches!(
        args.get(2)
            .ok_or(CLIError::MissignArguments)?
            .to_ascii_lowercase()
            .as_str(),
        "t" | "true"
    );

    let mut vm = VM::new(debug);

    vm.load_program(executable_path)?;
    vm.run()?;
    Ok(())
}
