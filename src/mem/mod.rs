pub mod rom;

use rom::ROM;

const CPU_RAM_START: u16 = 0x0000;
const CPU_RAM_MIRRORS_END: u16 = 0x1FFF;
const PPU_REGISTERS_START: u16 = 0x2000;
const PPU_REGISTERS_MIRRORS_END: u16 = 0x3FFF;
const PRG_ROM_START: u16 = 0x8000;
const PRG_ROM_END: u16 = 0xFFFF;

pub trait Memory {
    fn read_mem(&self, addr: u16) -> u8;
    fn read_mem16(&self, addr: u16) -> u16;
    fn write_mem(&mut self, addr: u16, val: u8);
    fn write_mem16(&mut self, addr: u16, val: u16);
}

pub struct Bus {
    pub cpu_ram: [u8; 2048],
    pub rom: ROM,
}

impl Bus {

    pub fn new(rom: ROM) -> Bus {
        Bus {
            cpu_ram: [0; 2048],
            rom,
        }
    }

    fn read_prg_rom(&self, mut addr: u16) -> u8 {
        addr -= 0x8000;
        if self.rom.prg_rom.len() == 16000 && addr >= 16000 {
            addr %= 16000;
        }

        self.rom.prg_rom[addr as usize]
    }
}

impl Memory for Bus {
    fn read_mem(&self, addr: u16) -> u8 {
        match addr {
            CPU_RAM_START..=CPU_RAM_MIRRORS_END => {
                let addr = addr & 0x07FF; // map mirrors to original by keeping bottom 11 bits
                self.cpu_ram[addr as usize]
            }
            PPU_REGISTERS_START..=PPU_REGISTERS_MIRRORS_END => {
                let _addr = addr & 0x2007;
                todo!("NO PPU YET CHILL MAN");
            }
            PRG_ROM_START..=PRG_ROM_END => {
                self.read_prg_rom(addr)
            }
            _ => {
                println!("Invalid memory read");
                0
            }
        }
    }

    fn read_mem16(&self, addr: u16) -> u16 {
        let l = self.read_mem(addr) as u16;
        let h = self.read_mem(addr + 1) as u16;
        (h << 8) | l
    }

    fn write_mem(&mut self, addr: u16, val: u8) {
        match addr {
            CPU_RAM_START..=CPU_RAM_MIRRORS_END => {
                let addr = addr & 0x07FF; 
                self.cpu_ram[addr as usize] = val;
            }
            PPU_REGISTERS_START..=PPU_REGISTERS_MIRRORS_END => {
                let _addr = addr & 0x2007;
                todo!("NO PPU YET CHILL MAN");
            }
            PRG_ROM_START..=PRG_ROM_END => {
                println!("not allowed to write to ROM");

            }
            _ => {
                println!("Invalid memory write");
            }
        }
    }

    fn write_mem16(&mut self, addr: u16, val: u16) {
        let l = (val & 0x00FF) as u8;
        let h = (val >> 8) as u8;
        self.write_mem(addr, l);
        self.write_mem(addr + 1, h);
    }

}


