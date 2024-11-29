use nix::sys::select::select;
use nix::sys::select::FdSet;
use nix::sys::time::{TimeVal, TimeValLike};
use std::cmp::Ordering;
use std::env;
use std::file;
use std::fmt;
use std::fs;
use std::fs::File;
use std::io;
use std::io::Bytes;
use std::io::Error;
use std::io::Read;
use std::io::Write;
use std::os::fd::AsFd;
use thiserror::Error;

const ARG_SIZE: u16 = 12;
// 2^16. 65536 locations.
const MEMORY_MAX: usize = 2_usize.pow(16);

const ARGUMENT_MASK: u16 = 0b0000_1111_1111_1111;

// Keyboard status register
const MR_KBSR: u16 = 0b1111_1110_0000_0000;

// Keyboard data register
const MR_KBDR: u16 = 0b1111_1110_0000_0010;

pub struct VM {
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
    running: bool,
}

#[derive(PartialEq, Debug)]
enum Mode {
    Immediate { value: u16 },
    Register { sr2: u16 },
}

#[derive(PartialEq, Debug)]
enum Opcode {
    // branch
    Br {
        n: bool,
        z: bool,
        p: bool,
        offset: u16,
    },
    /* add  */
    Add {
        dr: u16,
        sr1: u16,
        second_arg: Mode,
    },
    /* load */
    Ld {
        dr: u16,
        addr: u16,
    },
    /* store register */
    Str {
        sr: u16,
        base_reg: u16,
        offset: u16,
    },
    /* jump register */
    Jsr {
        addr: Mode,
    },
    /* bitwise and */
    And {
        dr: u16,
        sr1: u16,
        second_arg: Mode,
    },
    /* load register */
    Ldr {
        dr: u16,
        base_r: u16,
        offset: u16,
    },
    /* store  */
    St {
        sr: u16,
        offset: u16,
    },
    /* unused */
    Rti,
    /* bitwise not */
    Not {
        dr: u16,
        sr: u16,
    },
    /* load indirect */
    Ldi {
        dr: u16,
        // NOTE: This value needs to be added to the PC at runtime
        pointer: u16,
    },
    /* store indirect */
    Sti {
        sr: u16,
        offset: u16,
    },
    /* jump */
    Jmp {
        base_r: u16,
    },
    /* reserved (unused) */
    Res,
    /* load effective address */
    Lea {
        dr: u16,
        offset: u16,
    },
    /* execute trap */
    Trap {
        code: TrapCode,
    },
}

#[derive(PartialEq, Debug)]
enum FL {
    POS = 1 << 0,
    ZRO = 1 << 1,
    NEG = 1 << 2,
}

#[derive(PartialEq, Debug)]
enum TrapCode {
    Getc,
    Out,
    Puts,
    In,
    Putsp,
    Halt,
}

#[derive(Error, Debug)]
pub enum VMError {
    #[error("File does not exist.")]
    FileDoesNotExist(#[from] std::io::Error),
    #[error("unknown data store error")]
    Unknown,
}

fn test_print(message: &str) {
    if cfg!(test) {
        println!("DEBUG: {:?}", message);
    }
}

impl fmt::Debug for VM {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f)?;
        writeln!(f, "R0: {}", self.r0)?;
        writeln!(f, "R1: {}", self.r1)?;
        writeln!(f, "R2: {}", self.r2)?;
        writeln!(f, "R3: {}", self.r3)?;
        writeln!(f, "R4: {}", self.r4)?;
        writeln!(f, "R5: {}", self.r5)?;
        writeln!(f, "R6: {}", self.r6)?;
        writeln!(f, "R7: {}", self.r7)?;
        writeln!(f, "RPC: {:x}", self.rpc)?;
        writeln!(f, "RCOND: {:?}", self.rcond)?;
        writeln!(f, "Running: {:?}", self.running)
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
            rpc: 0x3000,
            rcond: FL::ZRO,
            running: false,
        }
    }

    fn load_instruction(&mut self) -> u16 {
        self.memory_read(self.rpc)
    }

    // QUESTION: Maybe implement from?
    fn u16_to_char(tochar: u16) -> char {
        let right_bits: u8 = tochar as u8;
        let c_char: char = right_bits.into();
        c_char
    }

    pub fn run(&mut self) {
        self.running = true;

        while self.running {
            let instruction = self.load_instruction();
            self.rpc = self.rpc.wrapping_add(1);
            let operation = self.decode_instruction(instruction);
            if cfg!(debug_assertions) {
                print!("{}[2J", 27 as char);
                println!("{:?}", self);
                println!("Current instruction {:?}", operation);
                println!("Press any key for next instruction");
                let _: u16 = std::io::stdin()
                    .bytes()
                    .next()
                    .and_then(|result| result.ok())
                    .map(|byte| byte as u16)
                    .unwrap();
            }
            self.execute(operation);
        }
    }

    fn load_bytes(&mut self, bytes: &[u8]) {
        let mut instructions: [u16; MEMORY_MAX] = [0; MEMORY_MAX];

        let mut it = bytes.chunks(2);
        let orig = it.next().unwrap();
        let first = orig.get(0).unwrap();
        let second = orig.get(1).unwrap();

        let addr = u16::from_be_bytes([*first, *second]);

        for (index, byte) in it.enumerate() {
            let first_half = byte.get(0).unwrap();
            let second_half = byte.get(1).unwrap();
            let instruction = u16::from_be_bytes([*first_half, *second_half]);
            let offset = index.wrapping_add(addr as usize);
            instructions[offset] = instruction;
        }

        self.memory = instructions;
    }

    pub fn load_program(&mut self, path: &str) -> Result<(), VMError> {
        let bytes = &std::fs::read(path).map_err(VMError::from)?;
        self.load_bytes(bytes);
        Ok(())
    }

    fn check_key(&mut self) {
        let mut buffer = [0; 1];
        std::io::stdin().read_exact(&mut buffer).unwrap();
        if buffer[0] != 0 {
            self.memory_write(MR_KBSR, 1 << 15);
            self.memory_write(MR_KBDR, buffer[0] as u16);
        } else {
            self.memory_write(MR_KBSR, 0)
        }
    }

    fn memory_write(&mut self, addr: u16, value: u16) {
        let addr = addr as usize;
        self.memory[addr] = value;
    }

    fn memory_read(&mut self, addr: u16) -> u16 {
        if addr == MR_KBSR {
            self.check_key();
        }

        let addr = addr as usize;
        *self
            .memory
            .get(addr)
            .expect("OUT OF MEMORY RANGE. Segmentation fault?")
    }

    fn decode_instruction(&self, binary_repr: u16) -> Opcode {
        // Removes the arguments from the instruction, leaving only the operator
        let op = binary_repr >> ARG_SIZE;
        // Removes the operator from the instruction, leaving only the arguments
        let args = binary_repr & ARGUMENT_MASK;

        match op {
            // BR
            0b0000 => {
                // NOTE: This will check if respective bit is set.
                let n = ((args & 0b0000_1000_0000_0000) >> 11) == 1;
                let z = ((args & 0b0000_0100_0000_0000) >> 10) == 1;
                let p = ((args & 0b0000_0010_0000_0000) >> 9) == 1;

                let offset9 = args & 0b0000_0001_1111_1111;
                let offset = sign_extend(offset9, 9);
                Opcode::Br { n, z, p, offset }
            }
            // ADD
            0b0001 => {
                let mode = (args & 0b0000_0000_0010_0000) >> 5;

                test_print(format!("{:?}", mode).as_str());
                let second_arg = match mode {
                    0 => {
                        let dest_register = args & 0b0000_0000_0000_0111;
                        Mode::Register { sr2: dest_register }
                    }
                    1 => {
                        let immediate = args & 0b0000_0000_0001_1111;
                        let immediate = sign_extend(immediate, 5);
                        Mode::Immediate { value: immediate }
                    }
                    _ => panic!("ERROR WHILST PARSING"),
                };

                let dr = (args & 0b0000_1110_0000_0000) >> 9;

                let sr1 = (args & 0b0000_0001_1100_0000) >> 6;

                Opcode::Add {
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
                        Mode::Register { sr2: dest_register }
                    }
                    1 => {
                        let immediate = args & 0b0000_0000_0001_1111;
                        let immediate = sign_extend(immediate, 5);
                        Mode::Immediate { value: immediate }
                    }
                    _ => panic!("ERROR WHILST PARSING"),
                };
                let dr = (args & 0b0000_1110_0000_0000) >> 9;

                let sr1 = (args & 0b0000_0001_1100_0000) >> 6;

                Opcode::And {
                    dr,
                    sr1,
                    second_arg,
                }
            }
            // JMP / RET
            0b1100 => {
                let base_r = (args & 0b0000_0001_1100_0000) >> 6;
                Opcode::Jmp { base_r }
            }
            // JSR / JSRR
            0b0100 => {
                let mode = (args & 0b0000_1000_0000_0000) >> 11;
                let addr = match mode {
                    0 => {
                        let base_r = (args & 0b0000_0001_1100_0000) >> 6;
                        Mode::Register { sr2: base_r }
                    }
                    1 => {
                        let offset11 = args & 0b0000_0111_1111_1111;
                        let offset = sign_extend(offset11, 11);
                        Mode::Immediate { value: offset }
                    }
                    _ => panic!("ERROR WHILE PARSING"),
                };
                Opcode::Jsr { addr }
            }
            // LD
            0b0010 => {
                let dr = (args & 0b0000_1110_0000_0000) >> 9;
                let offset9 = args & 0b0000_0001_1111_1111;
                let value = sign_extend(offset9, 9);
                Opcode::Ld { dr, addr: value }
            }
            // LDI
            0b1010 => {
                let dr = (args & 0b0000_1110_0000_0000) >> 9;
                let offset9 = args & 0b0000_0001_1111_1111;
                let pointer = sign_extend(offset9, 9);
                Opcode::Ldi { dr, pointer }
            }
            // LDR
            0b0110 => {
                let dr = (args & 0b0000_1110_0000_0000) >> 9;
                let base_r = (args & 0b0000_0001_1100_0000) >> 6;
                let offset6 = args & 0b0000_0000_0011_1111;
                let offset = sign_extend(offset6, 6);
                Opcode::Ldr { dr, base_r, offset }
            }
            // LEA
            0b1110 => {
                let dr = (args & 0b0000_1110_0000_0000) >> 9;
                let offset9 = args & 0b0000_0001_1111_1111;
                let offset = sign_extend(offset9, 9);
                Opcode::Lea { dr, offset }
            }
            // NOT
            0b1001 => {
                let dr = (args & 0b0000_1110_0000_0000) >> 9;
                let sr = (args & 0b0000_0001_1100_0000) >> 6;
                let spec = args & 0b0000_0000_0011_1111;

                debug_assert_eq!(spec, 0b0000_0000_0011_1111);
                Opcode::Not { dr, sr }
            }
            // RTI
            0b1000 => Opcode::Rti,
            // ST
            0b0011 => {
                let sr = (args & 0b0000_1110_0000_0000) >> 9;
                let offset9 = args & 0b0000_0001_1111_1111;
                let offset = sign_extend(offset9, 9);
                Opcode::St { sr, offset }
            }
            // STI
            0b1011 => {
                let sr = (args & 0b0000_1110_0000_0000) >> 9;
                let offset9 = args & 0b0000_0001_1111_1111;
                let offset = sign_extend(offset9, 9);
                Opcode::Sti { sr, offset }
            }
            // STR
            0b0111 => {
                let sr = (args & 0b0000_1110_0000_0000) >> 9;
                let base_reg = (args & 0b0000_0001_1100_0000) >> 6;
                let offset6 = args & 0b0000_0000_0011_1111;
                let offset = sign_extend(offset6, 6);
                Opcode::Str {
                    sr,
                    base_reg,
                    offset,
                }
            }
            // TRAP
            0b1111 => {
                let trapcode: u16 = args & 0b0000_0000_1111_1111;
                let code = match trapcode {
                    0x20 => TrapCode::Getc,
                    0x21 => TrapCode::Out,
                    0x22 => TrapCode::Puts,
                    0x23 => TrapCode::In,
                    0x24 => TrapCode::Putsp,
                    0x25 => TrapCode::Halt,
                    _ => panic!("Non existant trap code"),
                };
                Opcode::Trap { code }
            }
            // Reserved
            0b1101 => Opcode::Res,
            _ => panic!("Unrecognized operation code"),
        }
    }

    fn update_register(&mut self, register_index: u16, value: u16) {
        let register = self.get_mut_register(register_index);
        *register = value;
        self.update_flags(value);
    }

    fn update_flags(&mut self, value: u16) {
        self.rcond = if value == 0 {
            FL::ZRO
        } else if value >> 15 == 1 {
            FL::NEG
        } else {
            FL::POS
        }
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
            Opcode::Add {
                dr,
                sr1,
                second_arg,
            } => {
                let sr1_val = self.value_from_register(sr1);
                let second_value = match second_arg {
                    Mode::Immediate { value } => value,
                    Mode::Register { sr2 } => {
                        let sr2_val = self.value_from_register(sr2);
                        sr2_val
                    }
                };
                let result = second_value.wrapping_add(sr1_val);

                self.update_register(dr, result);
            }
            Opcode::Ldi {
                dr,
                pointer: offset,
            } => {
                let pointer = self.rpc.wrapping_add(offset);
                let addr = self.memory_read(pointer);
                let value = self.memory_read(addr);
                self.update_register(dr, value);
            }
            Opcode::Ld { dr, addr } => {
                let value = self.memory_read(addr);
                self.update_register(dr, value);
            }
            Opcode::And {
                dr,
                sr1,
                second_arg,
            } => {
                let sr1_val = self.value_from_register(sr1);
                let second_value = match second_arg {
                    Mode::Immediate { value } => value,
                    Mode::Register { sr2 } => {
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

            Opcode::Not { dr, sr } => {
                let value = self.value_from_register(sr);
                let notvalue = !value;

                self.update_register(dr, notvalue);
            }

            Opcode::Str {
                sr,
                base_reg,
                offset,
            } => {
                let content = self.value_from_register(sr);
                let addr = self.value_from_register(base_reg).wrapping_add(offset);
                self.memory_write(addr, content);
            }
            Opcode::Sti { sr, offset } => {
                let pointer = self.rpc.wrapping_add(offset);
                let addr = self.memory_read(pointer);
                let value = self.memory_read(addr);
                self.update_register(sr, value);
            }
            Opcode::Ldr { dr, base_r, offset } => {
                let base_value = self.value_from_register(base_r);
                let addr = base_value.wrapping_add(offset);
                let value = self.memory_read(addr);
                self.update_register(dr, value);
            }
            Opcode::St { sr, offset } => {
                let value = self.value_from_register(sr);
                let addr = self.rpc.wrapping_add(offset);
                self.memory_write(addr, value);
            }
            Opcode::Rti => panic!("RTI instruction not supported."),
            Opcode::Res => panic!("RESERVED instruction not supported."),
            Opcode::Lea { dr, offset } => {
                let addr = self.rpc.wrapping_add(offset);
                self.update_register(dr, addr);
            }
            Opcode::Br { n, z, p, offset } => {
                let addr = self.rpc.wrapping_add(offset);
                if (n && self.rcond == FL::NEG)
                    || (z && self.rcond == FL::ZRO)
                    || (p && self.rcond == FL::POS)
                {
                    self.rpc = addr;
                }
            }
            Opcode::Jmp { base_r } => {
                let addr = self.value_from_register(base_r);
                self.rpc = addr;
            }
            Opcode::Jsr { addr } => {
                self.r7 = self.rpc;
                self.rpc = match addr {
                    Mode::Register { sr2 } => sr2,
                    Mode::Immediate { value } => {
                        let new_pos = self.rpc.wrapping_add(value);
                        new_pos
                    }
                }
            }
            Opcode::Trap { code } => match code {
                TrapCode::Getc => {
                    let input: u8 = std::io::stdin()
                        .bytes()
                        .next()
                        .and_then(|result| result.ok())
                        .unwrap();
                    self.update_register(0, input.into());
                }
                TrapCode::Out => {
                    let content = self.value_from_register(0);
                    let char_repr = Self::u16_to_char(content);
                    print!("{}", char_repr);
                    std::io::stdout().flush().expect("Hey");
                }
                TrapCode::Puts => {
                    let mut addr = self.value_from_register(0);
                    let mut content = self.memory_read(addr);
                    while content != 0x0000 {
                        let c_char: u8 = content.try_into().expect("Unable to cast");
                        let c_char: char = c_char.into();
                        print!("{}", c_char);
                        addr = addr.wrapping_add(1);
                        content = self.memory_read(addr);
                    }
                    std::io::stdout().flush().expect("Hey");
                }
                TrapCode::In => {
                    print!("Enter a character:");
                    let input: u8 = std::io::stdin()
                        .bytes()
                        .next()
                        .and_then(|result| result.ok())
                        .map(|byte| byte as u8)
                        .unwrap();
                    print!("{}", input as char);
                    std::io::stdout().flush().expect("Hey");

                    self.update_register(0, input.into());
                }
                TrapCode::Putsp => {
                    let mut addr = self.value_from_register(0);
                    let mut content = self.memory_read(addr);
                    while content != 0x0000 {
                        let first_char = (content & 0b1111_1111_0000_0000) >> 8;
                        let second_char = content & 0b0000_0000_1111_1111;
                        print!("{}", first_char);
                        print!("{}", second_char);
                        addr = addr.wrapping_add(1);
                        content = self.memory_read(addr);
                    }
                    std::io::stdout().flush().expect("Hey");
                }
                TrapCode::Halt => self.running = false,
            },
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
            Opcode::Add {
                dr: 2,
                sr1: 3,
                second_arg: Mode::Register { sr2: 1 },
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
            Opcode::Add {
                dr: 5,
                sr1: 7,
                second_arg: Mode::Immediate { value: 2 }
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
            Opcode::And {
                dr: 1,
                sr1: 2,
                second_arg: Mode::Register { sr2: 4 }
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

        assert_eq!(operation, Opcode::Ld { dr: 7, addr: 42 });

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
        assert_eq!(operation, Opcode::Not { dr: 4, sr: 5 });

        vm.execute(operation);

        assert_eq!(0b1111111111110101, vm.r4);
    }

    #[test]
    fn check_flags() {
        //This functions checks that the condition flags are correctly
        // set to negative, positive and zero when they should.
        let mut vm = VM::new();

        //  ADDR  |  HEX  |      BINARY      |  LN  |  ASSEMBLY
        //        |       |                  |    1 |           .ORIG x3000
        // x3000  | x123F | 0001001000111111 |    2 |           ADD R1, R0, -1
        // x3001  | x1221 | 0001001000100001 |    3 |           ADD R1, R0, 1
        // x3002  | x1220 | 0001001000100000 |    4 |           ADD R1, R0, 0
        // x3003  | x1234 | 0001001000110100 |    5 |           ADD R1, R0, -12
        // x3004  | x122C | 0001001000101100 |    6 |           ADD R1, R0, 12
        // x3005  | xF022 | 1111000000100010 |    7 |           PUTS
        // x3006  | xF025 | 1111000000100101 |    8 |           HALT
        //        |       |                  |    9 |           .END

        let mut operations = [
            0b0001001000111111,
            0b0001001000100001,
            0b0001001000100000,
            0b0001001000110100,
            0b0001001000101100,
            0b1111000000100010,
            0b1111000000100101,
        ]
        .iter();

        // This is only done to for testing purposes
        vm.execute(vm.decode_instruction(*operations.next().unwrap()));
        assert_eq!(vm.rcond, FL::NEG);
        vm.execute(vm.decode_instruction(*operations.next().unwrap()));
        assert_eq!(vm.rcond, FL::POS);
        vm.execute(vm.decode_instruction(*operations.next().unwrap()));
        assert_eq!(vm.rcond, FL::ZRO);
        vm.execute(vm.decode_instruction(*operations.next().unwrap()));
        assert_eq!(vm.rcond, FL::NEG);
        vm.execute(vm.decode_instruction(*operations.next().unwrap()));
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
