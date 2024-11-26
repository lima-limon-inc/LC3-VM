// 2^16. 65536 locations.
const MEMORY_MAX: usize = 2_usize.pow(16);

const OPSIZE: u16 = 4;
const ARGSIZE: u16 = 12;

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

enum Opcode {
    OP_BR,   /* branch */
    OP_ADD,  /* add  */
    OP_LD,   /* load */
    OP_ST,   /* store */
    OP_JSR,  /* jump register */
    OP_AND,  /* bitwise and */
    OP_LDR,  /* load register */
    OP_STR,  /* store register */
    OP_RTI,  /* unused */
    OP_NOT,  /* bitwise not */
    OP_LDI,  /* load indirect */
    OP_STI,  /* store indirect */
    OP_JMP,  /* jump */
    OP_RES,  /* reserved (unused) */
    OP_LEA,  /* load effective address */
    OP_TRAP, /* execute trap */
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

    fn decode_instruction(instruction: u16) -> Opcode {
        // Removes the arguments from the instruction, leaving only the operator
        let op = instruction >> ARGSIZE;

        match op {
            // BR
            0b0000 => {
                todo!()
            }
            // ADD
            0b0001 => {
                todo!()
            }
            // AND
            0b0101 => {
                todo!()
            }
            // JMP / RET
            0b1100 => {
                todo!()
            }
            // JSR / JSRR / RET
            0b0100 => {
                todo!()
            }
            // LD
            0b0010 => {
                todo!()
            }
            // LDI
            0b1010 => {
                todo!()
            }
            // LDR
            0b0110 => {
                todo!()
            }
            // LEA
            0b1110 => {
                todo!()
            }
            // NOT
            0b1001 => {
                todo!()
            }
            // RTI
            0b1000 => {
                todo!()
            }
            // ST
            0b0011 => {
                todo!()
            }
            // STI
            0b1011 => {
                todo!()
            }
            // STR
            0b0111 => {
                todo!()
            }
            // TRAP
            0b1111 => {
                todo!()
            }
            // Illegal?
            0b1101 => {
                todo!()
            }
            _ => panic!("Unrecognized operation code"),
        }

        debug_assert_eq!(op, 1);
        Opcode::OP_ADD
    }
}

#[test]
fn check_memory_len() {
    let vm = VM::new();

    assert_eq!(vm.memory.len(), 65536);
}

#[test]
fn check_memory_add_operation() {
    let vm = VM::new();

    let op = 0b0001000000100000;
    VM::decode_instruction(op);

    assert_eq!(vm.memory.len(), 65536);
}
