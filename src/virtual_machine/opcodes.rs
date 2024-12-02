use thiserror::Error;

const ARG_SIZE: u16 = 12;

const ARGUMENT_MASK: u16 = 0b0000_1111_1111_1111;

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Opcode {
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
        offset: u16,
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

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum Mode {
    Immediate { value: u16 },
    Register { sr2: u16 },
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum TrapCode {
    Getc,
    Out,
    Puts,
    In,
    Putsp,
    Halt,
}

#[derive(Error, Debug)]
pub enum OperandError {
    #[error("Unrecognized operation code")]
    UnknownOperand,
    #[error("Unrecognized operation mode")]
    UnknownMode(u16),
    #[error("Unrecognized trapcode")]
    UnknownTrapcode(u16),
}

impl Opcode {
    pub fn decode_instruction(binary_repr: u16) -> Result<Opcode, OperandError> {
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
                Ok(Opcode::Br { n, z, p, offset })
            }
            // ADD
            0b0001 => {
                let mode = (args & 0b0000_0000_0010_0000) >> 5;

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
                    _ => return Err(OperandError::UnknownMode(mode)),
                };

                let dr = (args & 0b0000_1110_0000_0000) >> 9;

                let sr1 = (args & 0b0000_0001_1100_0000) >> 6;

                Ok(Opcode::Add {
                    dr,
                    sr1,
                    second_arg,
                })
            }
            // AND
            0b0101 => {
                let mode = (args & 0b0000_0000_0010_0000) >> 5;

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
                    _ => return Err(OperandError::UnknownMode(mode)),
                };
                let dr = (args & 0b0000_1110_0000_0000) >> 9;

                let sr1 = (args & 0b0000_0001_1100_0000) >> 6;

                Ok(Opcode::And {
                    dr,
                    sr1,
                    second_arg,
                })
            }
            // JMP / RET
            0b1100 => {
                let base_r = (args & 0b0000_0001_1100_0000) >> 6;
                Ok(Opcode::Jmp { base_r })
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
                    _ => return Err(OperandError::UnknownMode(mode)),
                };
                Ok(Opcode::Jsr { addr })
            }
            // LD
            0b0010 => {
                let dr = (args & 0b0000_1110_0000_0000) >> 9;
                let offset9 = args & 0b0000_0001_1111_1111;
                let offset = sign_extend(offset9, 9);
                Ok(Opcode::Ld { dr, offset })
            }
            // LDI
            0b1010 => {
                let dr = (args & 0b0000_1110_0000_0000) >> 9;
                let offset9 = args & 0b0000_0001_1111_1111;
                let pointer = sign_extend(offset9, 9);
                Ok(Opcode::Ldi { dr, pointer })
            }
            // LDR
            0b0110 => {
                let dr = (args & 0b0000_1110_0000_0000) >> 9;
                let base_r = (args & 0b0000_0001_1100_0000) >> 6;
                let offset6 = args & 0b0000_0000_0011_1111;
                let offset = sign_extend(offset6, 6);
                Ok(Opcode::Ldr { dr, base_r, offset })
            }
            // LEA
            0b1110 => {
                let dr = (args & 0b0000_1110_0000_0000) >> 9;
                let offset9 = args & 0b0000_0001_1111_1111;
                let offset = sign_extend(offset9, 9);
                Ok(Opcode::Lea { dr, offset })
            }
            // NOT
            0b1001 => {
                let dr = (args & 0b0000_1110_0000_0000) >> 9;
                let sr = (args & 0b0000_0001_1100_0000) >> 6;
                let spec = args & 0b0000_0000_0011_1111;

                debug_assert_eq!(spec, 0b0000_0000_0011_1111);
                Ok(Opcode::Not { dr, sr })
            }
            // RTI
            0b1000 => Ok(Opcode::Rti),
            // ST
            0b0011 => {
                let sr = (args & 0b0000_1110_0000_0000) >> 9;
                let offset9 = args & 0b0000_0001_1111_1111;
                let offset = sign_extend(offset9, 9);
                Ok(Opcode::St { sr, offset })
            }
            // STI
            0b1011 => {
                let sr = (args & 0b0000_1110_0000_0000) >> 9;
                let offset9 = args & 0b0000_0001_1111_1111;
                let offset = sign_extend(offset9, 9);
                Ok(Opcode::Sti { sr, offset })
            }
            // STR
            0b0111 => {
                let sr = (args & 0b0000_1110_0000_0000) >> 9;
                let base_reg = (args & 0b0000_0001_1100_0000) >> 6;
                let offset6 = args & 0b0000_0000_0011_1111;
                let offset = sign_extend(offset6, 6);
                Ok(Opcode::Str {
                    sr,
                    base_reg,
                    offset,
                })
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
                    _ => return Err(OperandError::UnknownTrapcode(trapcode)),
                };
                Ok(Opcode::Trap { code })
            }
            // Reserved
            0b1101 => Ok(Opcode::Res),
            _ => Err(OperandError::UnknownOperand),
        }
    }
}

mod test {
    use super::*;

    #[test]
    fn sign_extension() {
        let value = sign_extend(0b11111, 5);
        assert_eq!(value, 0b1111111111111111);
    }
}

fn sign_extend(number: u16, bit_count: i32) -> u16 {
    let mut result = number;
    if (number >> (bit_count - 1) & 1) == 1 {
        result = number | (u16::MAX << bit_count)
    };
    result.into()
}
