use std::cmp::Ordering;
// 2^16. 65536 locations.
const MEMORY_MAX: usize = 2_usize.pow(16);

const OP_SIZE: u16 = 4;
const ARG_SIZE: u16 = 12;

const ARGUMENT_MASK: u16 = 0b0000_1111_1111_1111;

// Keyboard status register
const MR_KBSR: u16 = 0b1111_1110_0000_0000;

// Keyboard data register
const MR_KBDR: u16 = 0b1111_1110_0000_0010;

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
}

#[derive(PartialEq, Debug)]
enum Mode {
    IMMEDIATE { imm5: u16 },
    REGISTER { sr2: u16 },
}

#[derive(PartialEq, Debug)]
enum Opcode {
    OpBr, /* branch */
    /* add  */
    OpAdd {
        dr: u16,
        sr1: u16,
        second_arg: Mode,
    },
    OpLd,  /* load */
    OpSt,  /* store */
    OpJsr, /* jump register */
    /* bitwise and */
    OpAnd {
        dr: u16,
        sr1: u16,
        second_arg: Mode,
    },
    OpLdr, /* load register */
    OpStr, /* store register */
    OpRti, /* unused */
    OpNot, /* bitwise not */
    /* load indirect */
    OpLdi {
        dr: u16,
        // NOTE: This value needs to be added to the PC at runtime
        offset: u16,
    },
    OpSti,  /* store indirect */
    OpJmp,  /* jump */
    OpRes,  /* reserved (unused) */
    OpLea,  /* load effective address */
    OpTrap, /* execute trap */
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
        }
    }

    fn memory_read(&self, addr: u16) -> u16 {
        // let addr = addr as usize;
        match addr {
            MR_KBSR => {
                todo!()
            }
            _ => {
                let addr = addr as usize;
                *self
                    .memory
                    .get(addr)
                    .expect("OUT OF MEMORY RANGE. Segmentation fault?")
            }
        }
    }

    fn decode_instruction(&self, instruction: u16) -> Opcode {
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
                        Mode::REGISTER { sr2: dest_register }
                    }
                    1 => {
                        let immediate = args & 0b0000_0000_0001_1111;
                        let immediate = sign_extend(immediate, 5);
                        Mode::IMMEDIATE { imm5: immediate }
                    }
                    _ => panic!("ERROR WHILST PARSING"),
                };

                let dr = (args & 0b0000_1110_0000_0000) >> 9;

                let sr1 = (args & 0b0000_0001_1100_0000) >> 6;

                Opcode::OpAdd {
                    dr,
                    sr1,
                    second_arg,
                }
            }
            // AND
            0b0101 => {
                let mode = (args & 0b0000_0000_0010_0000) >> 5;

                test_print(format!("{:?}", mode).as_str());
                let second_arg = match mode {
                    0 => {
                        let dest_register = args & 0b0000_0000_0000_0111;
                        Mode::REGISTER { sr2: dest_register }
                    }
                    1 => {
                        let immediate = args & 0b0000_0000_0001_1111;
                        let immediate = sign_extend(immediate, 5);
                        Mode::IMMEDIATE { imm5: immediate }
                    }
                    _ => panic!("ERROR WHILST PARSING"),
                };
                let dr = (args & 0b0000_1110_0000_0000) >> 9;

                let sr1 = (args & 0b0000_0001_1100_0000) >> 6;

                Opcode::OpAnd {
                    dr,
                    sr1,
                    second_arg,
                }
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
                let dr = (args & 0b0000_1110_0000_0000) >> 9;
                let pcoffset9 = args & 0b0000_0001_1111_1111;
                let pointer = sign_extend(pcoffset9, 9);
                Opcode::OpLdi {
                    dr,
                    offset: pointer,
                }
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

    fn update_register(&mut self, register_index: u16, value: u16) {
        let register = self.get_mut_register(register_index);
        *register = value;
        self.update_flags(value);
    }

    fn update_flags(&mut self, value: u16) {
        self.rcond = match value.cmp(&0) {
            Ordering::Less => FL::NEG,
            Ordering::Equal => FL::ZRO,
            Ordering::Greater => FL::POS,
        };
    }

    fn value_from_register(&self, reg_num: u16) -> u16 {
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
            _ => panic!("Invalid register"),
        }
    }

    fn get_mut_register(&mut self, reg_num: u16) -> &mut u16 {
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
            _ => panic!("Invalid register"),
        }
    }

    fn execute(&mut self, operation: Opcode) {
        match operation {
            Opcode::OpAdd {
                dr,
                sr1,
                second_arg,
            } => {
                let sr1_val = self.value_from_register(sr1);
                let second_value = match second_arg {
                    Mode::IMMEDIATE { imm5 } => {
                        let value = sign_extend(imm5, 5);
                        value
                    }
                    Mode::REGISTER { sr2 } => {
                        let sr2_val = self.value_from_register(sr2);
                        sr2_val
                    }
                };
                let result = second_value.wrapping_add(sr1_val);

                self.update_register(dr, result);
            }
            Opcode::OpLdi { dr, offset } => {
                let pointer = self.rpc + offset;
                let addr = self.memory_read(pointer);
                let value = self.memory_read(addr);
                self.update_register(dr, value);
            }
            Opcode::OpAnd {
                dr,
                sr1,
                second_arg,
            } => {
                let sr1_val = self.value_from_register(sr1);
                let second_value = match second_arg {
                    Mode::IMMEDIATE { imm5 } => {
                        let value = sign_extend(imm5, 5);
                        value
                    }
                    Mode::REGISTER { sr2 } => {
                        let sr2_val = self.value_from_register(sr2);
                        sr2_val
                    }
                };

                let result = sr1_val & second_value;

                //TODO: Should I do something about "he condition
                // codes are set, based on whether the binary value
                // produced, taken as a 2’s complement integer, is
                // negative, zero, or positive"
                self.update_register(dr, result);
            }
            _ => todo!(),
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
        let op = 0b0001_0100_1100_0001;
        let result = vm.decode_instruction(op);

        assert_eq!(
            Opcode::OpAdd {
                dr: 2,
                sr1: 3,
                second_arg: Mode::REGISTER { sr2: 1 },
            },
            result
        );
    }

    #[test]
    fn check_memory_add_operation_imm_mode() {
        let vm = VM::new();

        //         ADD R5, R7, 2
        let op = 0b0001_1011_1110_0010;
        let result = vm.decode_instruction(op);

        assert_eq!(
            Opcode::OpAdd {
                dr: 5,
                sr1: 7,
                second_arg: Mode::IMMEDIATE { imm5: 2 }
            },
            result
        );
    }

    #[test]
    fn sign_extension() {
        let value = sign_extend(0b11111, 5);
        assert_eq!(value, 0b1111111111111111);
    }

    #[test]
    fn add_operation() {
        //         ADD R2, R3, R1
        let mut vm = VM::new();

        let op = 0b0001010011000001;
        let operation = vm.decode_instruction(op);

        vm.r2 = 0;
        vm.r3 = 10;
        vm.r1 = 15;
        vm.execute(operation);
        assert_eq!(vm.r2, 25);
        assert_eq!(vm.rcond, FL::POS);

        //         ADD R5, R7, 2
        let op = 0b0001101111100010;
        let operation = vm.decode_instruction(op);

        let mut vm = VM::new();
        vm.r5 = 0;
        vm.r7 = 10;
        vm.execute(operation);
        assert_eq!(vm.r5, 12);
        assert_eq!(vm.rcond, FL::POS);
    }
}

fn sign_extend(number: u16, bit_count: i32) -> u16 {
    let mut result = number;
    if (number >> (bit_count - 1) & 1) == 1 {
        result = number | (u16::MAX << bit_count)
    };
    result.into()
}
