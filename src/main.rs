use std::env;

mod virtual_machine;

use crate::virtual_machine::VM;

fn main() {
    let mut vm = VM::new();
    let args: Vec<String> = env::args().collect();
    let executable_path = args.get(1).unwrap();

    let a = vm.load_program(executable_path);
    if let Ok(a) = a {
        println!("Hello, world!");
        vm.run();
        println!("DONE");
    } else {
        panic!();
    }
    // vm.run();
}
