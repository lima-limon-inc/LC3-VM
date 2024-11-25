// 2^16. 65536 locations.
const MEMORY_MAX: usize = 2_usize.pow(16);

struct VM {
    memory: [u16; MEMORY_MAX],
}

impl VM {
    pub fn new() -> VM {
        let memory = [0; MEMORY_MAX];
        VM { memory }
    }
}

#[test]
fn check_memory_len() {
    let vm = VM::new();

    assert_eq!(vm.memory.len(), 65536);
}
