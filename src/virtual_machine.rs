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
    IMMEDIATE { value: u16 },
    REGISTER { sr2: u16 },
}

#[derive(PartialEq, Debug)]
enum Opcode {
    // OpBr, /* branch */
    /* add  */
    OpAdd {
        dr: u16,
        sr1: u16,
        second_arg: Mode,
    },
    /* load */
    OpLd {
        dr: u16,
        addr: u16,
    },
    /* store register */
    OpStr {
        sr: u16,
        base_reg: u16,
        offset: u16,
    },
    // OpJsr, /* jump register */
    /* bitwise and */
    OpAnd {
        dr: u16,
        sr1: u16,
        second_arg: Mode,
    },
    /* load register */
    OpLdr {
        dr: u16,
        base_r: u16,
        offset: u16,
    },
    /* store  */
    OpSt {
        sr: u16,
        offset: u16,
    },
    /* unused */
    OpRti,
    /* bitwise not */
    OpNot {
        dr: u16,
        sr: u16,
    },
    /* load indirect */
    OpLdi {
        dr: u16,
        // NOTE: This value needs to be added to the PC at runtime
        pointer: u16,
    },
    /* store indirect */
    OpSti {
        sr: u16,
        offset: u16,
    },
    // OpJmp,  /* jump */
    /* reserved (unused) */
    OpRes,
    /* load effective address */
    OpLea {
        dr: u16,
        offset: u16,
    },
    // OpTrap, /* execute trap */
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

    fn memory_write(&mut self, addr: u16, value: u16) {
        let addr = addr as usize;
        self.memory[addr] = value;
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
                        Mode::IMMEDIATE { value: immediate }
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
                        Mode::IMMEDIATE { value: immediate }
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
                let dr = (args & 0b0000_1110_0000_0000) >> 9;
                let offset9 = args & 0b0000_0001_1111_1111;
                let value = sign_extend(offset9, 9);
                Opcode::OpLd { dr, addr: value }
            }
            // LDI
            0b1010 => {
                let dr = (args & 0b0000_1110_0000_0000) >> 9;
                let offset9 = args & 0b0000_0001_1111_1111;
                let pointer = sign_extend(offset9, 9);
                Opcode::OpLdi { dr, pointer }
            }
            // LDR
            0b0110 => {
                let dr = (args & 0b0000_1110_0000_0000) >> 9;
                let base_r = (args & 0b0000_0001_1100_0000) >> 6;
                let offset6 = args & 0b0000_0000_0011_1111;
                let offset = sign_extend(offset6, 6);
                Opcode::OpLdr { dr, base_r, offset }
            }
            // LEA
            0b1110 => {
                let dr = (args & 0b0000_1110_0000_0000) >> 9;
                let offset9 = args & 0b0000_0001_1111_1111;
                let offset = sign_extend(offset9, 9);
                Opcode::OpLea { dr, offset }
            }
            // NOT
            0b1001 => {
                let dr = (args & 0b0000_1110_0000_0000) >> 9;
                let sr = (args & 0b0000_0001_1100_0000) >> 6;
                let spec = args & 0b0000_0000_0011_1111;

                debug_assert_eq!(spec, 0b0000_0000_0011_1111);
                Opcode::OpNot { dr, sr }
            }
            // RTI
            0b1000 => Opcode::OpRti,
            // ST
            0b0011 => {
                let sr = (args & 0b0000_1110_0000_0000) >> 9;
                let offset9 = args & 0b0000_0001_1111_1111;
                let offset = sign_extend(offset9, 9);
                Opcode::OpSt { sr, offset }
            }
            // STI
            0b1011 => {
                let sr = (args & 0b0000_1110_0000_0000) >> 9;
                let offset9 = (args & 0b0000_0001_1111_1111) >> 9;
                let offset = sign_extend(offset9, 9);
                Opcode::OpSti { sr, offset }
            }
            // STR
            0b0111 => {
                let sr = (args & 0b0000_1110_0000_0000) >> 9;
                let base_reg = (args & 0b0000_0001_1100_0000) >> 6;
                let offset6 = args & 0b0000_0000_0011_1111;
                let offset = sign_extend(offset6, 6);
                Opcode::OpStr {
                    sr,
                    base_reg,
                    offset,
                }
            }
            // TRAP
            0b1111 => {
                todo!()
            }
            // Reserved
            0b1101 => Opcode::OpRes,
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
                    Mode::IMMEDIATE { value } => value,
                    Mode::REGISTER { sr2 } => {
                        let sr2_val = self.value_from_register(sr2);
                        sr2_val
                    }
                };
                let result = second_value.wrapping_add(sr1_val);

                self.update_register(dr, result);
            }
            Opcode::OpLdi {
                dr,
                pointer: offset,
            } => {
                let pointer = self.rpc + offset;
                let addr = self.memory_read(pointer);
                let value = self.memory_read(addr);
                self.update_register(dr, value);
            }
            Opcode::OpLd { dr, addr } => {
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
                    Mode::IMMEDIATE { value } => value,
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

            Opcode::OpNot { dr, sr } => {
                let value = self.value_from_register(sr);
                let notvalue = !value;

                self.update_register(dr, notvalue);
            }

            Opcode::OpStr {
                sr,
                base_reg,
                offset,
            } => {
                let content = self.value_from_register(sr);
                let addr = self.value_from_register(base_reg) + offset;
                self.memory_write(addr, content);
            }
            Opcode::OpSti { sr, offset } => {
                let pointer = self.rpc + offset;
                let addr = self.memory_read(pointer);
                let value = self.memory_read(addr);
                self.update_register(sr, value);
            }
            Opcode::OpLdr { dr, base_r, offset } => {
                let base_value = self.value_from_register(base_r);
                let addr = base_value + offset;
                let value = self.memory_read(addr);
                self.update_register(dr, value);
            }
            Opcode::OpSt { sr, offset } => {
                let value = self.value_from_register(sr);
                let addr = self.rpc + offset;
                self.memory_write(addr, value);
            }
            Opcode::OpRti => panic!("RTI instruction not supported."),
            Opcode::OpRes => panic!("RESERVED instruction not supported."),
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
                second_arg: Mode::IMMEDIATE { value: 2 }
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

    #[test]
    fn and_test() {
        let mut vm = VM::new();

        //         AND R1, R2, R4
        let op = 0b0101001010000100;
        let operation = vm.decode_instruction(op);
        vm.r1 = 0;
        vm.r2 = 10;
        vm.r4 = 12;

        assert_eq!(
            operation,
            Opcode::OpAnd {
                dr: 1,
                sr1: 2,
                second_arg: Mode::REGISTER { sr2: 4 }
            }
        );

        //This is only done to check if the execution correcly changes
        // the value of rcond and it's not its default value.
        vm.rcond = FL::NEG;

        vm.execute(operation);
        assert_eq!(vm.r1, 8);
        assert_eq!(vm.rcond, FL::POS);
    }

    #[test]
    fn test_ld() {
        let mut vm = VM::new();
        // LD R7, 42
        let op = 0b0010111000101010;
        let operation = vm.decode_instruction(op);
        vm.r7 = 0;

        assert_eq!(operation, Opcode::OpLd { dr: 7, addr: 42 });

        vm.memory[42] = 1234;
        vm.execute(operation);
        assert_eq!(vm.r7, 1234);
        assert_eq!(vm.rcond, FL::POS);
    }

    #[test]
    fn not_test() {
        let mut vm = VM::new();
        // NOT R4, R5
        let op = 0b1001100101111111;
        let operation = vm.decode_instruction(op);

        vm.r5 = 10;
        assert_eq!(operation, Opcode::OpNot { dr: 4, sr: 5 });

        vm.execute(operation);

        assert_eq!(0b1111111111110101, vm.r4);
    }
}

fn sign_extend(number: u16, bit_count: i32) -> u16 {
    let mut result = number;
    if (number >> (bit_count - 1) & 1) == 1 {
        result = number | (u16::MAX << bit_count)
    };
    result.into()
}
