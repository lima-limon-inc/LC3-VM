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
