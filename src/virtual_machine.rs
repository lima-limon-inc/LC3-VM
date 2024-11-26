use std::cmp::Ordering;
// 2^16. 65536 locations.
const MEMORY_MAX: usize = 2_usize.pow(16);

const OP_SIZE: u16 = 4;
const ARG_SIZE: u16 = 12;

const ARGUMENT_MASK: u16 = 0b0000111111111111;

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
    rcond: FL,
    rcount: u16,
}

#[derive(PartialEq, Debug)]
enum AddMode {
    IMMEDIATE { imm5: u16 },
    REGISTER { sr2: u16 },
}

#[derive(PartialEq, Debug)]
enum Opcode {
    OP_BR, /* branch */
    /* add  */
    OP_ADD {
        dr: u16,
        sr1: u16,
        second_arg: AddMode,
    },
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

#[derive(PartialEq, Debug)]
enum FL {
    POS = 1 << 0,
    ZRO = 1 << 1,
    NEG = 1 << 2,
}

fn test_print(message: &str) {
    if cfg!(test) {
        println!("DEBUG: {:?}", message);
    }
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
            rcond: FL::ZRO,
            rcount: 0,
        }
    }

    fn decode_instruction(instruction: u16) -> Opcode {
        // Removes the arguments from the instruction, leaving only the operator
        let op = instruction >> ARG_SIZE;
        // Removes the operator from the instruction, leaving only the arguments
        let args = instruction & ARGUMENT_MASK;

        // NOTE: Whilst writing constants inside a code block
        // is not very orthodox, I believe it remains a good
        // balance between avoiding magic numbers and keeping
        // the logic "local".
        match op {
            // BR
            0b0000 => {
                todo!()
            }
            // ADD
            0b0001 => {
                let mode = (args & 0b0000_0000_0010_0000) >> 5;

                test_print(format!("{:?}", mode).as_str());
                let second_arg = match mode {
                    0 => {
                        let dest_register = args & 0b0000_0000_0000_0111;
                        AddMode::REGISTER { sr2: dest_register }
                    }
                    1 => {
                        let immediate = args & 0b0000_0000_0001_1111;
                        let immediate = sign_extend(immediate, 5);
                        AddMode::IMMEDIATE { imm5: immediate }
                    }
                    _ => panic!("ERROR WHILST PARSING"),
                };

                let dr = (args & 0b0000_1110_0000_0000) >> 9;

                let sr1 = (args & 0b0000_0001_1100_0000) >> 6;
                Opcode::OP_ADD {
                    dr,
                    sr1,
                    second_arg,
                }
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
    }

    fn update_flags(&mut self, value: u16) {
        self.rcond = match value.cmp(&0) {
            Ordering::Less => FL::NEG,
            Ordering::Equal => FL::ZRO,
            Ordering::Greater => FL::POS,
        };
    }
    fn fetch_value_from_register(&self, reg_num: u16) -> u16 {
        match reg_num {
            0 => self.r0,
            1 => self.r1,
            2 => self.r2,
            3 => self.r3,
            4 => self.r4,
            5 => self.r5,
            6 => self.r6,
            7 => self.r7,
            8 => self.rpc,
            10 => self.rcount,
            _ => panic!("Invalid register"),
        }
    }

    fn fetch_register(&mut self, reg_num: u16) -> &mut u16 {
        match reg_num {
            0 => &mut self.r0,
            1 => &mut self.r1,
            2 => &mut self.r2,
            3 => &mut self.r3,
            4 => &mut self.r4,
            5 => &mut self.r5,
            6 => &mut self.r6,
            7 => &mut self.r7,
            8 => &mut self.rpc,
            10 => &mut self.rcount,
            _ => panic!("Invalid register"),
        }
    }
}

mod test {
    use super::*;

    #[test]
    fn check_memory_len() {
        let vm = VM::new();

        assert_eq!(vm.memory.len(), 65536);
    }

    #[test]
    fn check_memory_add_operation_reg_mode() {
        let vm = VM::new();

        //         ADD R2, R3, R1
        let op = 0b0001010011000001;
        let result = VM::decode_instruction(op);

        assert_eq!(
            Opcode::OP_ADD {
                dr: 2,
                sr1: 3,
                second_arg: AddMode::REGISTER { sr2: 1 },
            },
            result
        );
    }

    #[test]
    fn check_memory_add_operation_imm_mode() {
        let vm = VM::new();

        //         ADD R5, R7, 2
        let op = 0b0001101111100010;
        let result = VM::decode_instruction(op);

        assert_eq!(
            Opcode::OP_ADD {
                dr: 5,
                sr1: 7,
                second_arg: AddMode::IMMEDIATE { imm5: 2 }
            },
            result
        );
    }

    #[test]
    fn sign_extension() {
        let value = sign_extend(0b11111, 5);
        assert_eq!(value, 0b1111111111111111);
    }

    // 0001001000100010 |    3 | ADD R1, R0, 2
}

fn sign_extend(number: u16, bit_count: i32) -> u16 {
    let mut result = number;
    if (number >> (bit_count - 1) & 1) == 1 {
        result = number | (u16::MAX << bit_count)
    };
    result.into()
}
