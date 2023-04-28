
#[derive(Debug, Copy, Clone)]
pub struct StatusRegister {
    carry: bool,
    zero: bool,
    interrupt_disable: bool,
    decimal_mode: bool,
    break_command: bool,
    overflow: bool,
    negative: bool,
}

impl StatusRegister {
    fn new() -> StatusRegister {
        StatusRegister {
            carry: false,
            zero: false,
            interrupt_disable: false,
            decimal_mode: false,
            break_command: false,
            overflow: false,
            negative: false,
        }
    }

    fn set_zn(&mut self, r: u8) {
        self.zero = r == 0;
        self.negative = r & 0x80 != 0; 
    }

    fn reset(&mut self) {
        self.carry = false;
        self.zero = false;
        self.interrupt_disable = false;
        self.decimal_mode = false;
        self.break_command = false;
        self.overflow = false;
        self.negative = false;
    }

}

impl From<u8> for StatusRegister {
    fn from(n: u8) -> Self {
        StatusRegister {
            carry: (n & 0x01) != 0,    
            zero: (n & 0x02) != 0,    
            interrupt_disable: (n & 0x04) != 0,    
            decimal_mode: (n & 0x08) != 0,    
            break_command: (n & 0x10) != 0,    
            overflow: (n & 0x40) != 0,    
            negative: (n & 0x80) != 0,    
        } 
    }
}

impl From<StatusRegister> for u8 {
    fn from(reg: StatusRegister) -> Self {
        reg.carry as u8 |
        ((reg.zero as u8) << 1) |
        ((reg.interrupt_disable as u8) << 2) |
        ((reg.decimal_mode as u8) << 3) |
        ((reg.break_command as u8) << 4) |
        ((reg.overflow as u8) << 6) |
        ((reg.negative as u8) << 7) 
    }
}

enum AddrMode {
    Imm,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Indirect,
    IndirectX,
    IndirectY,
    Implied,
    Relative,
    Accumulator,
}

enum OCName {
    ADC, AND, ASL, BCC, BCS, BEQ, BIT, BMI, BNE, 
    BPL, BRK, BVC, BVS, CLC, CLD, CLI, CLV, CMP,
    CPX, CPY, DEC, DEX, DEY, EOR, INC, INX, INY,
    JMP, JSR, LDA, LDX, LDY, LSR, NOP, ORA, PHA,
    PHP, PLA, PLP, ROL, ROR, RTI, RTS, SBC, SEC, 
    SED, SEI, STA, STX, STY, TAX, TAY, TSX, TXA, 
    TXS, TYA,
}

struct Opcode {
    hex: u8,
    name: OCName,
    mode: AddrMode,
    ncycles: u16
}


static OPCODES: [Opcode; 151] = [
    Opcode {hex: 0x69, name: OCName::ADC, mode: AddrMode::Imm, ncycles: 2},
    Opcode {hex: 0x65, name: OCName::ADC, mode: AddrMode::ZeroPage, ncycles: 3},
    Opcode {hex: 0x75, name: OCName::ADC, mode: AddrMode::ZeroPageX, ncycles: 4},
    Opcode {hex: 0x6D, name: OCName::ADC, mode: AddrMode::Absolute, ncycles: 4},
    Opcode {hex: 0x7D, name: OCName::ADC, mode: AddrMode::AbsoluteX, ncycles: 4},
    Opcode {hex: 0x79, name: OCName::ADC, mode: AddrMode::AbsoluteY, ncycles: 4},
    Opcode {hex: 0x61, name: OCName::ADC, mode: AddrMode::IndirectX, ncycles: 6},
    Opcode {hex: 0x71, name: OCName::ADC, mode: AddrMode::IndirectY, ncycles: 5},
    Opcode {hex: 0x29, name: OCName::AND, mode: AddrMode::Imm, ncycles: 2},
    Opcode {hex: 0x25, name: OCName::AND, mode: AddrMode::ZeroPage, ncycles: 3},
    Opcode {hex: 0x35, name: OCName::AND, mode: AddrMode::ZeroPageX, ncycles: 4},
    Opcode {hex: 0x2D, name: OCName::AND, mode: AddrMode::Absolute, ncycles: 4},
    Opcode {hex: 0x3D, name: OCName::AND, mode: AddrMode::AbsoluteX, ncycles: 4},
    Opcode {hex: 0x39, name: OCName::AND, mode: AddrMode::AbsoluteY, ncycles: 4},
    Opcode {hex: 0x21, name: OCName::AND, mode: AddrMode::IndirectX, ncycles: 6},
    Opcode {hex: 0x31, name: OCName::AND, mode: AddrMode::IndirectY, ncycles: 5},
    Opcode {hex: 0x0A, name: OCName::ASL, mode: AddrMode::Accumulator, ncycles: 2},
    Opcode {hex: 0x06, name: OCName::ASL, mode: AddrMode::ZeroPage,ncycles: 5},
    Opcode {hex: 0x16, name: OCName::ASL, mode: AddrMode::ZeroPageX, ncycles: 6},
    Opcode {hex: 0x0E, name: OCName::ASL, mode: AddrMode::Absolute, ncycles: 6},
    Opcode {hex: 0x1E, name: OCName::ASL, mode: AddrMode::AbsoluteX, ncycles: 7},
    Opcode {hex: 0x90, name: OCName::BCC, mode: AddrMode::Relative, ncycles: 2},
    Opcode {hex: 0xB0, name: OCName::BCS, mode: AddrMode::Relative, ncycles: 2},
    Opcode {hex: 0xF0, name: OCName::BEQ, mode: AddrMode::Relative, ncycles: 2},
    Opcode {hex: 0x24, name: OCName::BIT, mode: AddrMode::ZeroPage, ncycles: 3},
    Opcode {hex: 0x2C, name: OCName::BIT, mode: AddrMode::Absolute, ncycles: 4},
    Opcode {hex: 0x30, name: OCName::BMI, mode: AddrMode::Relative, ncycles: 2},
    Opcode {hex: 0xD0, name: OCName::BNE, mode: AddrMode::Relative, ncycles: 2},
    Opcode {hex: 0x10, name: OCName::BPL, mode: AddrMode::Relative, ncycles: 2},
    Opcode {hex: 0x00, name: OCName::BRK, mode: AddrMode::Implied, ncycles: 7},
    Opcode {hex: 0x50, name: OCName::BVC, mode: AddrMode::Relative, ncycles: 2},
    Opcode {hex: 0x70, name: OCName::BVS, mode: AddrMode::Relative, ncycles: 2},
    Opcode {hex: 0x18, name: OCName::CLC, mode: AddrMode::Implied, ncycles: 2},
    Opcode {hex: 0xD8, name: OCName::CLD, mode: AddrMode::Implied, ncycles: 2},
    Opcode {hex: 0x58, name: OCName::CLI, mode: AddrMode::Implied, ncycles: 2},
    Opcode {hex: 0xB8, name: OCName::CLV, mode: AddrMode::Implied, ncycles: 2},
    Opcode {hex: 0xC9, name: OCName::CMP, mode: AddrMode::Imm, ncycles: 2},
    Opcode {hex: 0xC5, name: OCName::CMP, mode: AddrMode::ZeroPage, ncycles: 3},
    Opcode {hex: 0xD5, name: OCName::CMP, mode: AddrMode::ZeroPageX, ncycles: 4},
    Opcode {hex: 0xCD, name: OCName::CMP, mode: AddrMode::Absolute, ncycles: 4},
    Opcode {hex: 0xDD, name: OCName::CMP, mode: AddrMode::AbsoluteX, ncycles: 4},
    Opcode {hex: 0xD9, name: OCName::CMP, mode: AddrMode::AbsoluteY, ncycles: 4},
    Opcode {hex: 0xC1, name: OCName::CMP, mode: AddrMode::IndirectX, ncycles: 6},
    Opcode {hex: 0xD1, name: OCName::CMP, mode: AddrMode::IndirectY, ncycles: 5},
    Opcode {hex: 0xE0, name: OCName::CPX, mode: AddrMode::Imm, ncycles: 2},
    Opcode {hex: 0xE4, name: OCName::CPX, mode: AddrMode::ZeroPage, ncycles: 3},
    Opcode {hex: 0xEC, name: OCName::CPX, mode: AddrMode::Absolute, ncycles: 4},
    Opcode {hex: 0xC0, name: OCName::CPY, mode: AddrMode::Imm, ncycles: 2},
    Opcode {hex: 0xC4, name: OCName::CPY, mode: AddrMode::ZeroPage, ncycles: 3},
    Opcode {hex: 0xCC, name: OCName::CPY, mode: AddrMode::Absolute, ncycles: 4},
    Opcode {hex: 0xC6, name: OCName::DEC, mode: AddrMode::ZeroPage, ncycles: 5},
    Opcode {hex: 0xD6, name: OCName::DEC, mode: AddrMode::ZeroPageX, ncycles: 6},
    Opcode {hex: 0xCE, name: OCName::DEC, mode: AddrMode::Absolute, ncycles: 6},
    Opcode {hex: 0xDE, name: OCName::DEC, mode: AddrMode::AbsoluteX, ncycles: 7},
    Opcode {hex: 0xCA, name: OCName::DEX, mode: AddrMode::Implied, ncycles: 2},
    Opcode {hex: 0x88, name: OCName::DEY, mode: AddrMode::Implied, ncycles: 2},
    Opcode {hex: 0x49, name: OCName::EOR, mode: AddrMode::Imm, ncycles: 2},
    Opcode {hex: 0x45, name: OCName::EOR, mode: AddrMode::ZeroPage, ncycles: 3},
    Opcode {hex: 0x55, name: OCName::EOR, mode: AddrMode::ZeroPageX, ncycles: 4},
    Opcode {hex: 0x4D, name: OCName::EOR, mode: AddrMode::Absolute, ncycles: 4},
    Opcode {hex: 0x5D, name: OCName::EOR, mode: AddrMode::AbsoluteX, ncycles: 4},
    Opcode {hex: 0x59, name: OCName::EOR, mode: AddrMode::AbsoluteY, ncycles: 4},
    Opcode {hex: 0x41, name: OCName::EOR, mode: AddrMode::IndirectX, ncycles: 6},
    Opcode {hex: 0x51, name: OCName::EOR, mode: AddrMode::IndirectY, ncycles: 5},
    Opcode {hex: 0xE6, name: OCName::INC, mode: AddrMode::ZeroPage, ncycles: 5},
    Opcode {hex: 0xF6, name: OCName::INC, mode: AddrMode::ZeroPageX, ncycles: 6},
    Opcode {hex: 0xEE, name: OCName::INC, mode: AddrMode::Absolute, ncycles: 6},
    Opcode {hex: 0xF6, name: OCName::INC, mode: AddrMode::AbsoluteX, ncycles: 7},
    Opcode {hex: 0xE8, name: OCName::INX, mode: AddrMode::Implied, ncycles: 2},
    Opcode {hex: 0xC8, name: OCName::INY, mode: AddrMode::Implied, ncycles: 2},
    Opcode {hex: 0x4C, name: OCName::JMP, mode: AddrMode::Absolute, ncycles: 3},
    Opcode {hex: 0x6C, name: OCName::JMP, mode: AddrMode::Indirect, ncycles: 5},
    Opcode {hex: 0x20, name: OCName::JSR, mode: AddrMode::Absolute, ncycles: 6},
    Opcode {hex: 0xA9, name: OCName::LDA, mode: AddrMode::Imm, ncycles: 2},
    Opcode {hex: 0xA5, name: OCName::LDA, mode: AddrMode::ZeroPage, ncycles: 3},
    Opcode {hex: 0xB5, name: OCName::LDA, mode: AddrMode::ZeroPageX, ncycles: 4},
    Opcode {hex: 0xAD, name: OCName::LDA, mode: AddrMode::Absolute, ncycles: 4},
    Opcode {hex: 0xBD, name: OCName::LDA, mode: AddrMode::AbsoluteX, ncycles: 4},
    Opcode {hex: 0xB9, name: OCName::LDA, mode: AddrMode::AbsoluteY, ncycles: 4},
    Opcode {hex: 0xA1, name: OCName::LDA, mode: AddrMode::IndirectX, ncycles: 6},
    Opcode {hex: 0xB1, name: OCName::LDA, mode: AddrMode::IndirectY, ncycles: 5},
    Opcode {hex: 0xA2, name: OCName::LDX, mode: AddrMode::Imm, ncycles: 2},
    Opcode {hex: 0xA6, name: OCName::LDX, mode: AddrMode::ZeroPage, ncycles: 3},
    Opcode {hex: 0xB6, name: OCName::LDX, mode: AddrMode::ZeroPageY, ncycles: 4},
    Opcode {hex: 0xAE, name: OCName::LDX, mode: AddrMode::Absolute, ncycles: 4},
    Opcode {hex: 0xBE, name: OCName::LDX, mode: AddrMode::AbsoluteY, ncycles: 4},
    Opcode {hex: 0xA0, name: OCName::LDY, mode: AddrMode::Imm, ncycles: 2},
    Opcode {hex: 0xA4, name: OCName::LDY, mode: AddrMode::ZeroPage, ncycles: 3},
    Opcode {hex: 0xB4, name: OCName::LDY, mode: AddrMode::ZeroPageX, ncycles: 4},
    Opcode {hex: 0xAC, name: OCName::LDY, mode: AddrMode::Absolute, ncycles: 4},
    Opcode {hex: 0xBC, name: OCName::LDY, mode: AddrMode::AbsoluteX, ncycles: 4},
    Opcode {hex: 0x4A, name: OCName::LSR, mode: AddrMode::Accumulator, ncycles: 2},
    Opcode {hex: 0x46, name: OCName::LSR, mode: AddrMode::ZeroPage, ncycles: 5},
    Opcode {hex: 0x56, name: OCName::LSR, mode: AddrMode::ZeroPageX, ncycles: 6},
    Opcode {hex: 0x4E, name: OCName::LSR, mode: AddrMode::Absolute, ncycles: 6},
    Opcode {hex: 0x5E, name: OCName::LSR, mode: AddrMode::AbsoluteX, ncycles: 7},
    Opcode {hex: 0xEA, name: OCName::NOP, mode: AddrMode::Implied, ncycles: 2},
    Opcode {hex: 0x09, name: OCName::ORA, mode: AddrMode::Imm, ncycles: 2},
    Opcode {hex: 0x05, name: OCName::ORA, mode: AddrMode::ZeroPage, ncycles: 3},
    Opcode {hex: 0x15, name: OCName::ORA, mode: AddrMode::ZeroPageX, ncycles: 4},
    Opcode {hex: 0x0D, name: OCName::ORA, mode: AddrMode::Absolute, ncycles: 4},
    Opcode {hex: 0x1D, name: OCName::ORA, mode: AddrMode::AbsoluteX, ncycles: 4},
    Opcode {hex: 0x19, name: OCName::ORA, mode: AddrMode::AbsoluteY, ncycles: 4},
    Opcode {hex: 0x01, name: OCName::ORA, mode: AddrMode::IndirectX, ncycles: 6},
    Opcode {hex: 0x11, name: OCName::ORA, mode: AddrMode::IndirectY, ncycles: 5},
    Opcode {hex: 0x48, name: OCName::PHA, mode: AddrMode::Implied, ncycles: 3},
    Opcode {hex: 0x08, name: OCName::PHP, mode: AddrMode::Implied, ncycles: 3},
    Opcode {hex: 0x68, name: OCName::PLA, mode: AddrMode::Implied, ncycles: 4},
    Opcode {hex: 0x28, name: OCName::PLP, mode: AddrMode::Implied, ncycles: 4},
    Opcode {hex: 0x2A, name: OCName::ROL, mode: AddrMode::Accumulator, ncycles: 2},
    Opcode {hex: 0x2A, name: OCName::ROL, mode: AddrMode::ZeroPage, ncycles: 5},
    Opcode {hex: 0x26, name: OCName::ROL, mode: AddrMode::ZeroPageX, ncycles: 6},
    Opcode {hex: 0x36, name: OCName::ROL, mode: AddrMode::Absolute, ncycles: 6},
    Opcode {hex: 0x3E, name: OCName::ROL, mode: AddrMode::AbsoluteX, ncycles: 7},
    Opcode {hex: 0x6A, name: OCName::ROR, mode: AddrMode::Accumulator, ncycles: 2},
    Opcode {hex: 0x66, name: OCName::ROR, mode: AddrMode::ZeroPage, ncycles: 5},
    Opcode {hex: 0x76, name: OCName::ROR, mode: AddrMode::ZeroPageX, ncycles: 6},
    Opcode {hex: 0x6E, name: OCName::ROR, mode: AddrMode::Absolute, ncycles: 6},
    Opcode {hex: 0x7E, name: OCName::ROR, mode: AddrMode::AbsoluteX, ncycles: 7},
    Opcode {hex: 0x40, name: OCName::RTI, mode: AddrMode::Implied, ncycles: 6},
    Opcode {hex: 0x60, name: OCName::RTS, mode: AddrMode::Implied, ncycles: 6},
    Opcode {hex: 0xE9, name: OCName::SBC, mode: AddrMode::Imm, ncycles: 2},
    Opcode {hex: 0xE5, name: OCName::SBC, mode: AddrMode::ZeroPage, ncycles: 3},
    Opcode {hex: 0xF5, name: OCName::SBC, mode: AddrMode::ZeroPageX, ncycles: 4},
    Opcode {hex: 0xED, name: OCName::SBC, mode: AddrMode::Absolute, ncycles: 4},
    Opcode {hex: 0xFD, name: OCName::SBC, mode: AddrMode::AbsoluteX, ncycles: 4},
    Opcode {hex: 0xF9, name: OCName::SBC, mode: AddrMode::AbsoluteY, ncycles: 4},
    Opcode {hex: 0xE1, name: OCName::SBC, mode: AddrMode::IndirectX, ncycles: 6},
    Opcode {hex: 0xF1, name: OCName::SBC, mode: AddrMode::IndirectY, ncycles: 5},
    Opcode {hex: 0x38, name: OCName::SEC, mode: AddrMode::Implied, ncycles: 2},
    Opcode {hex: 0xF8, name: OCName::SED, mode: AddrMode::Implied, ncycles: 2},
    Opcode {hex: 0x78, name: OCName::SEI, mode: AddrMode::Implied, ncycles: 2},
    Opcode {hex: 0x85, name: OCName::STA, mode: AddrMode::ZeroPage, ncycles: 3},
    Opcode {hex: 0x95, name: OCName::STA, mode: AddrMode::ZeroPageX, ncycles: 4},
    Opcode {hex: 0x8D, name: OCName::STA, mode: AddrMode::Absolute, ncycles: 4},
    Opcode {hex: 0x9D, name: OCName::STA, mode: AddrMode::AbsoluteX, ncycles: 5},
    Opcode {hex: 0x99, name: OCName::STA, mode: AddrMode::AbsoluteY, ncycles: 5},
    Opcode {hex: 0x81, name: OCName::STA, mode: AddrMode::IndirectX, ncycles: 6},
    Opcode {hex: 0x91, name: OCName::STA, mode: AddrMode::IndirectY, ncycles: 6},
    Opcode {hex: 0x86, name: OCName::STX, mode: AddrMode::ZeroPage, ncycles: 3},
    Opcode {hex: 0x96, name: OCName::STX, mode: AddrMode::ZeroPageY, ncycles: 4},
    Opcode {hex: 0x8E, name: OCName::STX, mode: AddrMode::Absolute, ncycles: 4},
    Opcode {hex: 0x84, name: OCName::STY, mode: AddrMode::ZeroPage, ncycles: 3},
    Opcode {hex: 0x94, name: OCName::STY, mode: AddrMode::ZeroPageY, ncycles: 4},
    Opcode {hex: 0x8C, name: OCName::STY, mode: AddrMode::Absolute, ncycles: 4},
    Opcode {hex: 0xAA, name: OCName::TAX, mode: AddrMode::Implied, ncycles: 2},
    Opcode {hex: 0xA8, name: OCName::TAY, mode: AddrMode::Implied, ncycles: 2},
    Opcode {hex: 0xBA, name: OCName::TSX, mode: AddrMode::Implied, ncycles: 2},
    Opcode {hex: 0x8A, name: OCName::TXA, mode: AddrMode::Implied, ncycles: 2},
    Opcode {hex: 0x9A, name: OCName::TXS, mode: AddrMode::Implied, ncycles: 2},
    Opcode {hex: 0x98, name: OCName::TYA, mode: AddrMode::Implied, ncycles: 2},
];

fn opcode_lookup(hex: u8) -> Option<&'static Opcode> {
        OPCODES.iter().filter(|op| op.hex == hex).next()
}

pub struct CPU {
    pub pc: u16,
    pub sp: u8,
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub status: StatusRegister,
    memory: [u8; 0xFFFF],
}

impl CPU {
    pub fn new() -> CPU {
        CPU {
            pc: 0,
            sp: 0,
            a: 0,
            x: 0,
            y: 0,
            status: StatusRegister::new(),
            memory: [0; 0xFFFF],
        }
    }

    pub fn run(&mut self) {
        self.reset();
        self.exec();
    }

    pub fn reset(&mut self) {
        self.a = 0;
        self.x = 0;
        self.y = 0;
        self.status.reset();

        self.sp = 0xFF;
        self.pc = self.read_mem16(0xFFFC);
    }

    pub fn load(&mut self, program: Vec<u8>) {
        self.memory[0x8000..(0x8000 + program.len())].copy_from_slice(&program[..]);
        self.write_mem16(0xFFFC, 0x8000);
    }

    #[inline]
    fn read_mem(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    fn read_mem16(&self, addr: u16) -> u16 {
        let l = self.read_mem(addr) as u16;
        let h = self.read_mem(addr + 1) as u16;
        h << 8 | l
    }

    #[inline]
    fn write_mem(&mut self, addr: u16, val: u8) {
        self.memory[addr as usize] = val;
    }

    fn write_mem16(&mut self, addr: u16, val: u16) {
        let l = (val & 0xFF) as u8;
        let h = (val >> 8) as u8;
        self.write_mem(addr, l);
        self.write_mem(addr + 1, h);
    }

    fn sp_push(&mut self, n: u8) {
        if self.sp == 0x00 {panic!("stack full error");}

        let addr: u16 = self.sp as u16 + 0x100;
        self.write_mem(addr, n);
        self.sp -= 1;
    }

    fn sp_push16(&mut self, n: u16) {
        let l = (n & 0xFF) as u8;
        let h = (n >> 8) as u8;
        self.sp_push(l);
        self.sp_push(h);
    }

    fn sp_pop(&mut self) -> u8 {
        if self.sp == 0xFF {panic!("stack empty error");}

        self.sp += 1;
        let addr = self.sp as u16 + 0x100;
        self.read_mem(addr)
    }

    fn sp_pop16(&mut self) -> u16 {
        let h = self.sp_pop() as u16;
        let l = self.sp_pop() as u16;
        (h << 8) | l
    }



    #[inline]
    fn get_param(&self, mode: &AddrMode) -> u8 {
        let addr = self.resolve_address(mode);
        self.read_mem(addr)
    }

    fn resolve_address(&self, mode: &AddrMode) -> u16 {
        let addr = match mode {
            AddrMode::Imm => self.pc + 1,
            AddrMode::ZeroPage => self.read_mem(self.pc + 1) as u16,
            AddrMode::ZeroPageX => {
                let addr = self.read_mem(self.pc + 1);
                let addr = addr.wrapping_add(self.x) as u16;
                addr
            }
            AddrMode::ZeroPageY => {
                let addr = self.read_mem(self.pc + 1);
                let addr = addr.wrapping_add(self.y) as u16;
                addr
            }
            AddrMode::Absolute => self.read_mem16(self.pc + 1),
            AddrMode::AbsoluteX => {
                let addr = self.read_mem16(self.pc + 1);
                let addr = addr + (self.x as u16);
                addr
            }
            AddrMode::AbsoluteY => {
                let addr = self.read_mem16(self.pc + 1);
                let addr = addr + (self.y as u16);
                addr
            }
            AddrMode::IndirectX => {
                let base = self.read_mem(self.pc + 1);
                let base: u8 = base.wrapping_add(self.x);
                let l: u16 = self.read_mem(base as u16) as u16;
                let h: u16 = self.read_mem(base.wrapping_add(1) as u16) as u16;
                h << 8 | l 
            }
            AddrMode::IndirectY => {
                let base = self.read_mem(self.pc + 1);
                let l: u16 = self.read_mem(base as u16) as u16;
                let h: u16 = self.read_mem(base.wrapping_add(1) as u16) as u16;
                let addr: u16 = (h << 8 | l) + (self.y as u16);
                addr
            }

            _ => {
                panic!("Unexpected adressing mode");
            }

        };

        addr
    } 

    fn exec(&mut self) {

        loop {
            let opcode = self.read_mem(self.pc); 
            
            let opcode: &Opcode = opcode_lookup(opcode).expect("ran into unknown opcode");
            let mode: &AddrMode = &opcode.mode;

            match opcode.name {
                OCName::ADC => self.adc(mode),
                OCName::AND => self.and(mode),
                OCName::ASL => self.asl(mode),
                OCName::BCC => self.bcc(),
                OCName::BCS => self.bcs(),
                OCName::BEQ => self.beq(),
                OCName::BIT => self.bit(mode),
                OCName::BMI => self.bmi(),
                OCName::BNE => self.bne(),
                OCName::BPL => self.bpl(),
                OCName::BRK => break,
                OCName::BVC => self.bvc(),
                OCName::BVS => self.bvs(),
                OCName::CLC => self.clc(),
                OCName::CLD => self.cld(),
                OCName::CLI => self.cli(),
                OCName::CLV => self.clv(),
                OCName::CMP => self.cmp(mode),
                OCName::CPX => self.cpx(mode),
                OCName::CPY => self.cpy(mode),
                OCName::DEC => self.dec(mode),
                OCName::DEX => self.dex(),
                OCName::DEY => self.dey(),
                OCName::EOR => self.eor(mode),
                OCName::INC => self.inc(mode),
                OCName::INX => self.inx(),
                OCName::INY => self.iny(),
                OCName::JMP => self.jmp(mode),
                OCName::JSR => self.jsr(),
                OCName::LDA => self.lda(mode),
                OCName::LDX => self.ldx(mode),
                OCName::LDY => self.ldy(mode),
                OCName::LSR => self.lsr(mode),
                OCName::NOP => {},
                OCName::ORA => self.ora(mode),
                OCName::PHA => self.pha(),
                OCName::PHP => self.php(),
                OCName::PLA => self.pla(),
                OCName::PLP => self.plp(),
                OCName::ROL => self.rol(mode),
                OCName::ROR => self.ror(mode),
                OCName::RTI => self.rti(),
                OCName::RTS => self.rts(),
                OCName::SBC => self.sbc(mode),
                OCName::SEC => self.sec(),
                OCName::SED => self.sed(),
                OCName::SEI => self.sei(),
                OCName::STA => self.sta(mode),
                OCName::STX => self.stx(mode),
                OCName::STY => self.sty(mode),
                OCName::TAX => self.tax(),
                OCName::TAY => self.tay(),
                OCName::TSX => self.tsx(),
                OCName::TXA => self.txa(),
                OCName::TXS => self.txs(),
                OCName::TYA => self.tya(),
            }

            self.pc += match mode {
               AddrMode::Implied | AddrMode::Accumulator => 1, 
               AddrMode::Absolute | AddrMode::AbsoluteX | AddrMode::AbsoluteY => 3,
               _ => 2
            }
        }
    }

    fn signed_overflow(a: u8, b: u8, result: u8) -> bool {
        let a7 = (a >> 7) != 0;
        let b7 = (b >> 7) != 0;
        let r7 = (result >> 7) != 0;
        (a7 & b7 & !r7) | (!a7 & !b7 & r7)
    }

    fn add_cv(a: u8, b: u8, cin: bool) -> (u8, bool, bool) {
        let (sum1, c1) = a.overflowing_add(b);
        let v1 = CPU::signed_overflow(a, b, sum1);
        let (sum2, c2) = sum1.overflowing_add(cin as u8);
        let cout = c1 || c2;
        let vout = v1 || ((sum1 >> 7) == 0 && (sum2 >> 7) == 1);
        (sum2, cout, vout)
    }

    fn sub_cv(a: u8, b: u8, cin: bool) -> (u8, bool, bool) {
        let nb = !b + 1;
        let (sum1, c1) = a.overflowing_add(nb);
        let v1 = CPU::signed_overflow(a, nb, sum1);
        let ncin = !((!cin) as u8) + 1;
        let (sum2, c2) = sum1.overflowing_add(ncin);
        let cout = !(c1 || c2);
        let vout = v1 || ((sum1 >> 7) == 1 && (sum2 >> 7) == 0);
        (sum2, cout, vout)
    }

    fn adc(&mut self, mode: &AddrMode) {
        let n = self.get_param(mode);
        let (sum, c, v) = CPU::add_cv(self.a, n, self.status.carry);  
        self.status.carry = c;
        self.status.overflow = v;
        self.a = sum;
        self.status.set_zn(self.a);
    }
    
    fn and(&mut self, mode: &AddrMode) {
        let n = self.get_param(mode);
        self.a = self.a & n;
        self.status.set_zn(self.a);
    }

    fn asl(&mut self, mode: &AddrMode) {
        match mode {
            AddrMode::Accumulator => {
                self.status.carry = (self.a & 0x80) != 0; 
                self.a = self.a << 1;
                self.status.set_zn(self.a);
            }
            
            _ => {
                let addr = self.resolve_address(mode);
                let n = self.read_mem(addr);
                self.status.carry = (n & 0x80) != 0;
                let n = n << 1;
                self.write_mem(addr, n);
                self.status.set_zn(n);
            }
        }
    }

    fn bcc(&mut self) {
        if !self.status.carry {self.pc += self.read_mem(self.pc + 1) as u16;}
    }

    fn bcs(&mut self) {
        if self.status.carry {self.pc += self.read_mem(self.pc + 1) as u16;}
    }

    fn beq(&mut self) {
        if self.status.zero {self.pc += self.read_mem(self.pc + 1) as u16;}
    }

    fn bit(&mut self, mode: &AddrMode) {
        let n = self.get_param(mode);
        let r = n & self.a;
        self.status.zero = r == 0;
        self.status.overflow = (r & 0x40) != 0;
        self.status.negative = (r & 0x80) != 0;
    }

    fn bmi(&mut self) {
        if self.status.negative {self.pc += self.read_mem(self.pc + 1) as u16;}
    }

    fn bne(&mut self) {
        if !self.status.zero {self.pc += self.read_mem(self.pc + 1) as u16;}
    }

    fn bpl(&mut self){
        if !self.status.negative {self.pc += self.read_mem(self.pc + 1) as u16;}
    }

    fn bvc(&mut self) {
        if !self.status.overflow {self.pc += self.read_mem(self.pc + 1) as u16;}
    }

    fn bvs(&mut self) {
        if self.status.overflow {self.pc += self.read_mem(self.pc + 1) as u16;}
    }

    fn clc(&mut self) {
        self.status.carry = false;
    }

    fn cld(&mut self) {
        self.status.decimal_mode = false;
    }

    fn cli(&mut self) {
        self.status.interrupt_disable = false;
    }

    fn clv(&mut self) {
        self.status.overflow = false;
    }

    fn cmp(&mut self, mode: &AddrMode) {
        let n = self.get_param(mode);
        self.status.carry = self.a >= n;
        self.status.zero = self.a == n;
        self.status.negative = (self.a.wrapping_sub(n)) & 0x80 != 0;
    }

    fn cpx(&mut self, mode: &AddrMode) {
        let n = self.get_param(mode);
        self.status.carry = self.x >= n;
        self.status.zero = self.x == n;
        self.status.negative = (self.x.wrapping_sub(n)) & 0x80 != 0;
    }

    fn cpy(&mut self, mode: &AddrMode) {
        let n = self.get_param(mode);
        self.status.carry = self.y >= n;
        self.status.zero = self.y == n;
        self.status.negative = (self.y.wrapping_sub(n)) & 0x80 != 0;
    }

    fn dec(&mut self, mode: &AddrMode) {
        let addr = self.resolve_address(mode);
        let mut n = self.read_mem(addr);
        n = n.wrapping_sub(1);
        self.status.set_zn(n);
        self.write_mem(addr, n);
    }

    fn dex(&mut self) {
        self.x = self.x.wrapping_sub(1);
        self.status.set_zn(self.x);
    }

    fn dey(&mut self) {
        self.y = self.y.wrapping_sub(1);
        self.status.set_zn(self.y);
    }

    fn eor(&mut self, mode: &AddrMode) {
        let n = self.get_param(mode);
        self.a = self.a ^ n;
        self.status.set_zn(self.a);
    }

    fn inc(&mut self, mode: &AddrMode) {
        let addr = self.resolve_address(mode);
        let mut n = self.read_mem(addr);
        n = n.wrapping_add(1);
        self.status.set_zn(n);
        self.write_mem(addr, n);
    }


    fn inx(&mut self) {
        self.x = self.x.wrapping_add(1);
        self.status.set_zn(self.x);
    }

    fn iny(&mut self) {
        self.y = self.y.wrapping_add(1);
        self.status.set_zn(self.y);
    }

    fn jmp(&mut self, mode: &AddrMode) {
        let param = self.read_mem16(self.pc + 1);
        self.pc = match mode {
            AddrMode::Absolute => {
                param
            }
            AddrMode::Indirect => {
                self.read_mem16(param)
            }
            _ => {
                panic!("unexpected addressing mode in JMP");
            }
        };
    }

    fn jsr(&mut self) {
        let go_to = self.read_mem16(self.pc + 1);
        let return_point = self.pc + 2;
        self.sp_push16(return_point);
        self.pc = go_to;
    }

    fn lda(&mut self, mode: &AddrMode) {
        let n = self.get_param(mode);
        self.a = n;
        self.status.set_zn(self.a);
    }

    fn ldx(&mut self, mode: &AddrMode) {
        let n = self.get_param(mode);
        self.x = n;
        self.status.set_zn(self.x);
    }

    fn ldy(&mut self, mode: &AddrMode) {
        let n = self.get_param(mode);
        self.y = n;
        self.status.set_zn(self.y);
    }

    fn lsr(&mut self, mode: &AddrMode) {
        match mode {
            AddrMode::Accumulator => {
                self.status.carry = self.a % 2 == 1; 
                self.a = self.a >> 1;
                self.status.set_zn(self.a);
            }
            
            _ => {
                let addr = self.resolve_address(mode);
                let n = self.read_mem(addr);
                self.status.carry = n % 2 == 1;
                let n = n >> 1;
                self.write_mem(addr, n);
                self.status.set_zn(n);
            }
        }
    }

    fn ora(&mut self, mode: &AddrMode) {
        let n = self.get_param(mode);
        self.a = self.a | n;
        self.status.set_zn(self.a);
    }

    fn pha(&mut self) {
        self.sp_push(self.a);
    }

    fn php(&mut self) {
        self.sp_push(self.status.into());
    }

    fn pla(&mut self) {
        self.a = self.sp_pop();
        self.status.set_zn(self.a);
    }

    fn plp(&mut self) {
        self.status = self.sp_pop().into();
    }

    fn rol(&mut self, mode: &AddrMode) {
        match mode {
            AddrMode::Accumulator => {
                let new_c = (self.a & 0x80) != 0; 
                self.a = (self.a << 1) | (self.status.carry as u8);
                self.status.carry = new_c;
                self.status.set_zn(self.a);
            }
            
            _ => {
                let addr = self.resolve_address(mode);
                let n = self.read_mem(addr);
                let new_c = (n & 0x80) != 0;
                let n = n << 1 | (self.status.carry as u8);
                self.write_mem(addr, n);
                self.status.carry = new_c;
                self.status.set_zn(n);
            }
        }
    }

    fn ror(&mut self, mode: &AddrMode) {
        match mode {
            AddrMode::Accumulator => {
                let new_c = self.a % 2 == 1;
                self.a = (self.a >> 1) | ((self.status.carry as u8) << 7);
                self.status.carry = new_c;
                self.status.set_zn(self.a);
            }
            
            _ => {
                let addr = self.resolve_address(mode);
                let n = self.read_mem(addr);
                let new_c = n % 2 == 1;
                let n = n >> 1 | ((self.status.carry as u8) << 7);
                self.write_mem(addr, n);
                self.status.carry = new_c;
                self.status.set_zn(n);
            }
        }
    }

    fn rti(&mut self) {
        self.status = self.sp_pop().into();
        self.pc = self.sp_pop16();
    }

    fn rts(&mut self) {
        self.pc = self.sp_pop16();
    }

    fn sbc(&mut self, mode: &AddrMode) {
        let n = self.get_param(mode);
        let (sum, c, v) = CPU::sub_cv(self.a, n, self.status.carry);
        self.a = sum;
        self.status.carry = c;
        self.status.overflow = v;
        self.status.set_zn(self.a);
    }

    fn sec(&mut self) {
        self.status.carry = true;
    }

    fn sed(&mut self) {
        self.status.decimal_mode = true;
    }

    fn sei(&mut self) {
        self.status.interrupt_disable = true;
    }

    fn sta(&mut self, mode: &AddrMode) {
        let addr = self.resolve_address(mode);
        self.write_mem(addr, self.a);
    }

    fn stx(&mut self, mode: &AddrMode) {
        let addr = self.resolve_address(mode);
        self.write_mem(addr, self.x);
    }

    fn sty(&mut self, mode: &AddrMode) {
        let addr = self.resolve_address(mode);
        self.write_mem(addr, self.y);
    }

    fn tax(&mut self) {
        self.x = self.a;
        self.status.set_zn(self.x);
    }

    fn tay(&mut self) {
        self.y = self.a;
        self.status.set_zn(self.y);
    }

    fn tsx(&mut self) {
        self.x = self.sp;
        self.status.set_zn(self.x);
    }

    fn txa(&mut self) {
        self.a = self.x;
        self.status.set_zn(self.a);
    }

    fn txs(&mut self) {
        self.sp = self.x;
    }

    fn tya(&mut self) {
        self.a = self.y;
        self.status.set_zn(self.a);
    }


}

#[cfg(test)]
mod cpu_tests;