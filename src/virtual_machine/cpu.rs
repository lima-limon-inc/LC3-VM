use std::fmt;
use std::io::Read;
use std::io::Write;
use thiserror::Error;

use crate::virtual_machine::opcodes::Mode;
use crate::virtual_machine::opcodes::Opcode;
use crate::virtual_machine::opcodes::TrapCode;

// 2^16. 65536 locations.
const MEMORY_MAX: usize = 2_usize.pow(16);

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
    debug: bool,
}

#[derive(PartialEq, Debug)]
enum FL {
    POS = 1 << 0,
    ZRO = 1 << 1,
    NEG = 1 << 2,
}

#[derive(Error, Debug)]
pub enum VMError {
    #[error("File does not exist.")]
    FileDoesNotExist(#[from] std::io::Error),
    #[error("Failed to load the program into memory")]
    LoadError(String),
    #[error("Instruction is not 16 bits long")]
    InstructionLengthError,
    #[error("Trying to read memory out of the provided 65536 locations")]
    OutOfRangeError,
    #[error("Error while casting")]
    CastingError,
    #[error("Not supported instruction")]
    UnsupportedInstruction,
    #[error("Non existent register")]
    NonExistentRegister,
    #[error("Flush Error")]
    FlushError,
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
    pub fn new(debug: bool) -> VM {
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
            debug,
        }
    }

    fn load_instruction(&mut self) -> Result<u16, VMError> {
        self.memory_read(self.rpc)
    }

    // QUESTION: Maybe implement from?
    fn u16_to_char(tochar: u16) -> Result<char, VMError> {
        let right_bits: u8 = tochar.try_into().map_err(|_| VMError::CastingError)?;
        let c_char: char = right_bits.into();
        Ok(c_char)
    }

    pub fn run(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        self.running = true;

        while self.running {
            let instruction = self.load_instruction()?;
            self.rpc = self.rpc.wrapping_add(1);
            let operation = Opcode::decode_instruction(instruction)?;
            if self.debug {
                print!("{}[2J", 27 as char);
                println!("{:?}", self);
                println!("Current instruction {:?}", operation);
                println!("Press ENTER for next instruction");
                std::io::stdin()
                    .bytes()
                    .next()
                    .and_then(|result| result.ok())
                    .is_some();
            }
            self.execute(operation)?;
        }

        Ok(())
    }

    fn load_bytes(&mut self, bytes: &[u8]) -> Result<(), VMError> {
        let mut instructions: [u16; MEMORY_MAX] = [0; MEMORY_MAX];

        let mut it = bytes.chunks(2);
        let Some(orig) = it.next() else {
            return Err(VMError::LoadError(String::from(
                "File does not indicate initial memory location",
            )));
        };

        let Some(first) = orig.first() else {
            return Err(VMError::LoadError(String::from(
                "File does not indicate initial memory location",
            )));
        };
        let Some(second) = orig.get(1) else {
            return Err(VMError::LoadError(String::from(
                "File does not indicate initial memory location",
            )));
        };

        let addr = u16::from_be_bytes([*first, *second]);

        for (index, byte) in it.enumerate() {
            let Some(first_half) = byte.first() else {
                return Err(VMError::InstructionLengthError);
            };
            let Some(second_half) = byte.get(1) else {
                return Err(VMError::InstructionLengthError);
            };
            let instruction = u16::from_be_bytes([*first_half, *second_half]);
            let addr: usize = addr.into();
            let offset: usize = index.wrapping_add(addr);
            if let Some(memory_cell) = instructions.get_mut(offset) {
                *memory_cell = instruction;
            } else {
                return Err(VMError::InstructionLengthError);
            }
        }

        self.memory = instructions;
        Ok(())
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

    fn memory_read(&mut self, addr: u16) -> Result<u16, VMError> {
        if addr == MR_KBSR {
            self.check_key();
        }

        let addr = addr as usize;
        if let Some(value) = self.memory.get(addr) {
            Ok(*value)
        } else {
            Err(VMError::OutOfRangeError)
        }
    }

    fn update_register(&mut self, register_index: u16, value: u16) -> Result<(), VMError> {
        let register = self.get_mut_register(register_index)?;
        *register = value;
        self.update_flags(value);
        Ok(())
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

    fn value_from_register(&self, reg_num: u16) -> Result<u16, VMError> {
        match reg_num {
            0 => Ok(self.r0),
            1 => Ok(self.r1),
            2 => Ok(self.r2),
            3 => Ok(self.r3),
            4 => Ok(self.r4),
            5 => Ok(self.r5),
            6 => Ok(self.r6),
            7 => Ok(self.r7),
            8 => Ok(self.rpc),
            _ => Err(VMError::NonExistentRegister),
        }
    }

    fn get_mut_register(&mut self, reg_num: u16) -> Result<&mut u16, VMError> {
        match reg_num {
            0 => Ok(&mut self.r0),
            1 => Ok(&mut self.r1),
            2 => Ok(&mut self.r2),
            3 => Ok(&mut self.r3),
            4 => Ok(&mut self.r4),
            5 => Ok(&mut self.r5),
            6 => Ok(&mut self.r6),
            7 => Ok(&mut self.r7),
            8 => Ok(&mut self.rpc),
            _ => Err(VMError::NonExistentRegister),
        }
    }

    fn execute(&mut self, operation: Opcode) -> Result<(), VMError> {
        match operation {
            Opcode::Add {
                dr,
                sr1,
                second_arg,
            } => {
                let sr1_val = self.value_from_register(sr1)?;
                let second_value = match second_arg {
                    Mode::Immediate { value } => value,
                    Mode::Register { sr2 } => self.value_from_register(sr2)?,
                };
                let result = second_value.wrapping_add(sr1_val);

                self.update_register(dr, result)?;
            }
            Opcode::Ldi {
                dr,
                pointer: offset,
            } => {
                let pointer = self.rpc.wrapping_add(offset);
                let addr = self.memory_read(pointer)?;
                let value = self.memory_read(addr)?;
                self.update_register(dr, value)?;
            }
            Opcode::Ld { dr, offset } => {
                let addr = self.rpc.wrapping_add(offset);
                let value = self.memory_read(addr)?;
                self.update_register(dr, value)?;
            }
            Opcode::And {
                dr,
                sr1,
                second_arg,
            } => {
                let sr1_val = self.value_from_register(sr1)?;
                let second_value = match second_arg {
                    Mode::Immediate { value } => value,
                    Mode::Register { sr2 } => self.value_from_register(sr2)?,
                };

                let result = sr1_val & second_value;
                self.update_register(dr, result)?;
            }
            Opcode::Not { dr, sr } => {
                let value = self.value_from_register(sr)?;
                let notvalue = !value;

                self.update_register(dr, notvalue)?;
            }
            Opcode::Str {
                sr,
                base_reg,
                offset,
            } => {
                let content = self.value_from_register(sr)?;
                let addr = self.value_from_register(base_reg)?.wrapping_add(offset);
                self.memory_write(addr, content);
            }
            Opcode::Sti { sr, offset } => {
                let pointer = self.rpc.wrapping_add(offset);
                let addr = self.memory_read(pointer)?;
                let value = self.memory_read(addr)?;
                self.update_register(sr, value)?;
            }
            Opcode::Ldr { dr, base_r, offset } => {
                let base_value = self.value_from_register(base_r)?;
                let addr = base_value.wrapping_add(offset);
                let value = self.memory_read(addr)?;
                self.update_register(dr, value)?;
            }
            Opcode::St { sr, offset } => {
                let value = self.value_from_register(sr)?;
                let addr = self.rpc.wrapping_add(offset);
                self.memory_write(addr, value);
            }
            Opcode::Rti => return Err(VMError::UnsupportedInstruction),
            Opcode::Res => return Err(VMError::UnsupportedInstruction),
            Opcode::Lea { dr, offset } => {
                let addr = self.rpc.wrapping_add(offset);
                self.update_register(dr, addr)?;
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
                let addr = self.value_from_register(base_r)?;
                self.rpc = addr;
            }
            Opcode::Jsr { addr } => {
                self.r7 = self.rpc;
                self.rpc = match addr {
                    Mode::Register { sr2 } => sr2,
                    Mode::Immediate { value } => self.rpc.wrapping_add(value),
                };
            }
            Opcode::Trap { code } => match code {
                TrapCode::Getc => {
                    let input: u8 = std::io::stdin()
                        .bytes()
                        .next()
                        .and_then(|result| result.ok())
                        .unwrap();

                    self.update_register(0, input.into())?;
                }
                TrapCode::Out => {
                    let content = self.value_from_register(0)?;
                    let char_repr = Self::u16_to_char(content)?;

                    print!("{}", char_repr);
                    std::io::stdout().flush().map_err(|_| VMError::FlushError)?;
                }
                TrapCode::Puts => {
                    let mut addr = self.value_from_register(0)?;
                    let mut content = self.memory_read(addr)?;

                    while content != 0x0000 {
                        let c_char = Self::u16_to_char(content)?;
                        print!("{}", c_char);

                        addr = addr.wrapping_add(1);
                        content = self.memory_read(addr)?;
                    }
                    std::io::stdout().flush().map_err(|_| VMError::FlushError)?;
                }
                TrapCode::In => {
                    print!("Enter a character:");
                    let input: u8 = std::io::stdin()
                        .bytes()
                        .next()
                        .and_then(|result| result.ok())
                        .unwrap();
                    print!("{}", input as char);
                    std::io::stdout().flush().map_err(|_| VMError::FlushError)?;

                    self.update_register(0, input.into())?;
                }
                TrapCode::Putsp => {
                    let mut addr = self.value_from_register(0)?;
                    let mut content = self.memory_read(addr)?;

                    while content != 0x0000 {
                        let first_char = (content & 0b1111_1111_0000_0000) >> 8;
                        let second_char = content & 0b0000_0000_1111_1111;
                        print!("{}", first_char);
                        print!("{}", second_char);

                        addr = addr.wrapping_add(1);
                        content = self.memory_read(addr)?;
                    }
                    std::io::stdout().flush().map_err(|_| VMError::FlushError)?;
                }
                TrapCode::Halt => {
                    self.running = false;
                }
            },
        };
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::virtual_machine::opcodes::Mode;
    use crate::virtual_machine::opcodes::Opcode as Operand;
    use crate::virtual_machine::opcodes::TrapCode;

    #[test]
    fn check_memory_len() {
        let vm = VM::new(false);

        assert_eq!(vm.memory.len(), 65536);
    }

    #[test]
    pub(crate) fn check_memory_add_operation_reg_mode() {
        let vm = VM::new(false);

        //         ADD R2, R3, R1
        let op = 0b0001_0100_1100_0001;
        let result = Operand::decode_instruction(op);

        assert_eq!(
            Opcode::Add {
                dr: 2,
                sr1: 3,
                second_arg: Mode::Register { sr2: 1 },
            },
            result.unwrap()
        );
    }

    #[test]
    fn check_memory_add_operation_imm_mode() {
        let vm = VM::new(false);

        //         ADD R5, R7, 2
        let op = 0b0001_1011_1110_0010;
        let result = Operand::decode_instruction(op);

        assert_eq!(
            Opcode::Add {
                dr: 5,
                sr1: 7,
                second_arg: Mode::Immediate { value: 2 }
            },
            result.unwrap()
        );
    }

    #[test]
    fn add_operation() {
        //         ADD R2, R3, R1
        let mut vm = VM::new(false);

        let op = 0b0001010011000001;
        let operation = Operand::decode_instruction(op);

        vm.r2 = 0;
        vm.r3 = 10;
        vm.r1 = 15;
        vm.execute(operation.unwrap());
        assert_eq!(vm.r2, 25);
        assert_eq!(vm.rcond, FL::POS);

        //         ADD R5, R7, 2
        let op = 0b0001101111100010;
        let operation = Operand::decode_instruction(op);

        let mut vm = VM::new(false);
        vm.r5 = 0;
        vm.r7 = 10;
        vm.execute(operation.unwrap());
        assert_eq!(vm.r5, 12);
        assert_eq!(vm.rcond, FL::POS);
    }

    #[test]
    fn and_test() {
        let mut vm = VM::new(false);

        //         AND R1, R2, R4
        let op = 0b0101001010000100;
        let operation = Operand::decode_instruction(op);
        vm.r1 = 0;
        vm.r2 = 10;
        vm.r4 = 12;

        //This is only done to check if the execution correcly changes
        // the value of rcond and it's not its default value.
        vm.rcond = FL::NEG;

        vm.execute(operation.unwrap());
        assert_eq!(vm.r1, 8);
        assert_eq!(vm.rcond, FL::POS);
    }

    #[test]
    fn not_test() {
        let mut vm = VM::new(false);
        // NOT R4, R5
        let op = 0b1001100101111111;
        let operation = Operand::decode_instruction(op);

        vm.r5 = 10;

        vm.execute(operation.unwrap());

        assert_eq!(0b1111111111110101, vm.r4);
    }

    #[test]
    fn check_flags() {
        //This functions checks that the condition flags are correctly
        // set to negative, positive and zero when they should.
        let mut vm = VM::new(false);

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
        vm.execute(Operand::decode_instruction(*operations.next().unwrap()).unwrap());
        assert_eq!(vm.rcond, FL::NEG);
        vm.execute(Operand::decode_instruction(*operations.next().unwrap()).unwrap());
        assert_eq!(vm.rcond, FL::POS);
        vm.execute(Operand::decode_instruction(*operations.next().unwrap()).unwrap());
        assert_eq!(vm.rcond, FL::ZRO);
        vm.execute(Operand::decode_instruction(*operations.next().unwrap()).unwrap());
        assert_eq!(vm.rcond, FL::NEG);
        vm.execute(Operand::decode_instruction(*operations.next().unwrap()).unwrap());
        assert_eq!(vm.rcond, FL::POS);
    }

    #[test]
    fn ld_test() {
        // This test will store 9 in R5. It will then store R5's value
        // (9) into memory and it will then read from memory that
        // value and store it into R6. If all goes well, both
        // registers should hold the same value.

        // x3000  | x1B69 | 0001101101101001 |    3 |           ADD R5, R5, 9
        // x3001  | x3A05 | 0011101000000101 |    4 |           ST R5, 5
        // x3002  | x2C04 | 0010110000000100 |    5 |           LD R6, 4
        // x3003  | xF025 | 1111000000100101 |    6 |           HALT
        //        |       |                  |    7 |           .END
        let mut vm = VM::new(false);

        vm.load_program("examples/ld.obj").unwrap();

        vm.run().unwrap();

        assert_eq!(vm.r5, vm.r6);
    }
}
