mod opcode;

#[cfg(test)]
mod cpu_tests;

use crate::mem::{Memory, Bus};
use crate::mem::rom::ROM;
use self::opcode::{AddrMode, Opcode, OCName, opcode_lookup};


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

        ((reg.overflow as u8) << 6) |
        ((reg.negative as u8) << 7) 
    }
}

pub struct CPU {
    pub pc: u16,
    pub sp: u8,
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub status: StatusRegister,
    pub bus: Bus,
}

impl Memory for CPU {
    fn read_mem(&self, addr: u16) -> u8 {
        self.bus.read_mem(addr)
    }
    
    fn read_mem16(&self, addr: u16) -> u16 {
        self.bus.read_mem16(addr)
    }

    fn write_mem(&mut self, addr: u16, val: u8) {
        self.bus.write_mem(addr, val);
    }

    fn write_mem16(&mut self, addr: u16, val: u16) {
        self.bus.write_mem16(addr, val);
    }
}

impl CPU {
    
    pub fn new(rom: ROM) -> CPU {
        CPU {
            pc: 0,
            sp: 0xFF,
            a: 0,
            x: 0,
            y: 0,
            status: StatusRegister::new(),
            bus: Bus::new(rom),
        }
    }

    pub fn run_with_callback<F: FnMut(&mut CPU)>(&mut self, callback: F)
    {
        self.reset();
        self.exec(callback);
    }

    pub fn run(&mut self) {
        self.run_with_callback(|_| {});
    }

    pub fn reset(&mut self) {
        self.a = 0;
        self.x = 0;
        self.y = 0;
        self.status.reset();

        self.sp = 0xFF;
        self.pc = self.read_mem16(0xFFFC);
    }

    pub fn load_prg_rom(&mut self) -> Result<(), &'static str> {
        if self.bus.rom.prg_rom.len() > 2048 {
            return Err("program rom is too big to fit into ram");
        }
        let program = &(self.bus.rom.prg_rom)[..];
        self.bus.cpu_ram[0x0600..(0x0600 + program.len())].copy_from_slice(program);
        self.pc = 0x0600;
        Ok(())
    }

    pub fn load(&mut self, program: &[u8]) {
        self.bus.cpu_ram[0x0600..(0x0600 + program.len())].copy_from_slice(program);
        self.pc = 0x0600;
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

    pub fn print_state(&self) {
        let op: &Opcode = opcode_lookup(self.read_mem(self.pc)).expect("unknown opcode");
        print!("OPCODE: {:#4x} {:?} ", op.hex, op.name);
        print!("a: {}, x: {}, y: {}, pc: {}, sp: {}\n", self.a, self.x, self.y, self.pc, self.sp);
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

    fn exec<F: FnMut(&mut CPU)>(&mut self, mut callback: F) {

        loop {
            callback(self);

            let opcode = self.read_mem(self.pc); 
            
            let opcode: &Opcode = opcode_lookup(opcode).expect("ran into unknown opcode");
            let mode: &AddrMode = &opcode.mode;

            //println!("RUNNING {0}", self.pc);
            //println!("OPCODE: {:?}", opcode);

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
            };

        }
    }

    fn pc_add_signed(&mut self, n: i8) {
        if n < 0 { self.pc -= (n * -1) as u16;}
        else { self.pc += n as u16;}
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
        let c1 = a < b;
        let nb = !b;
        let nb = nb.wrapping_add(1);
        let sum1 = a.wrapping_add(nb);
        let v1 = CPU::signed_overflow(a, nb, sum1);
        let ncin = !((!cin) as u8);
        let ncin = ncin.wrapping_add(1);
        let c2 = !cin && sum1 < 1;
        let sum2 = sum1.wrapping_add(ncin);
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
        if !self.status.carry {self.pc_add_signed(self.read_mem(self.pc + 1) as i8);}
    }

    fn bcs(&mut self) {
        if self.status.carry {self.pc_add_signed(self.read_mem(self.pc + 1) as i8);}
    }

    fn beq(&mut self) {
        if self.status.zero {self.pc_add_signed(self.read_mem(self.pc + 1) as i8);}
    }

    fn bit(&mut self, mode: &AddrMode) {
        let n = self.get_param(mode);
        let r = n & self.a;
        self.status.zero = r == 0;
        self.status.overflow = (r & 0x40) != 0;
        self.status.negative = (r & 0x80) != 0;
    }

    fn bmi(&mut self) {
        if self.status.negative {self.pc_add_signed(self.read_mem(self.pc + 1) as i8);}
    }

    fn bne(&mut self) {
        if !self.status.zero {self.pc_add_signed(self.read_mem(self.pc + 1) as i8);}
    }

    fn bpl(&mut self){
        if !self.status.negative {self.pc_add_signed(self.read_mem(self.pc + 1) as i8);}
    }

    fn bvc(&mut self) {
        if !self.status.overflow {self.pc_add_signed(self.read_mem(self.pc + 1) as i8);}
    }

    fn bvs(&mut self) {
        if self.status.overflow {self.pc_add_signed(self.read_mem(self.pc + 1) as i8);}
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
        self.pc -= 3;
    }

    fn jsr(&mut self) {
        let go_to = self.read_mem16(self.pc + 1);
        let return_point = self.pc + 2;
        self.sp_push16(return_point);
        self.pc = go_to - 3;
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


