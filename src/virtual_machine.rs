// 2^16. 65536 locations.
const MEMORY_MAX: usize = 2_usize.pow(16);

struct VM {
    memory: [u16; MEMORY_MAX],
    r0: u16,
    r1: u16,
    r2: u16,
    r3: u16,
    r4: u16,
    r5: u16,
    r6: u16,
    r7: u16,
    rpc: u16,
    rcond: u16,
    rcount: u16,
}

impl VM {
    pub fn new() -> VM {
        let memory = [0; MEMORY_MAX];
        VM {
            memory,
            r0: 0,
            r1: 0,
            r2: 0,
            r3: 0,
            r4: 0,
            r5: 0,
            r6: 0,
            r7: 0,
            rpc: 0x300,
            rcond: 0,
            rcount: 0,
        }
    }
}

#[test]
fn check_memory_len() {
    let vm = VM::new();

    assert_eq!(vm.memory.len(), 65536);
}
