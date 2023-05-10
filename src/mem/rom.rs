pub enum Mirroring {
    Vertical,
    Horizontal,
    FourScreen,
}

const NES_MAGIC: [u8; 4] = [0x4E, 0x45, 0x53, 0x1A];
const PRG_ROM_PAGE_SIZE: usize = 16000;
const CHR_ROM_PAGE_SIZE: usize = 8000;

pub struct ROM {
    pub prg_rom: Vec<u8>,
    pub chr_rom: Vec<u8>,
    mapper: u8,
    mirroring: Mirroring,
}

impl ROM {
    pub fn empty() -> ROM {
        ROM {
            prg_rom: vec![],
            chr_rom: vec![],
            mapper: 0,
            mirroring: Mirroring::Vertical,
        }
    }

    pub fn program(prg: Vec<u8>) -> ROM {
        ROM {
            prg_rom: prg,
            chr_rom: vec![],
            mapper: 0,
            mirroring: Mirroring::Vertical,
        }
    }

    pub fn new(dump: Vec<u8>) -> Result<ROM, &'static str> {
        if &dump[0..4] != NES_MAGIC {
            return Err("ROM data is not in NES format!");
        }

        if ((dump[7] >> 2) & 0b11) != 0 {
            return Err("Only NES ver 1.0 is supported");
        }

        let prg_size = dump[4] as usize * PRG_ROM_PAGE_SIZE;
        let chr_size = dump[5] as usize * CHR_ROM_PAGE_SIZE;

        let vert_mir = dump[6] % 2 == 1;
        let four_scn = (dump[6] & 0x08) == 1;
        let mirroring = match (four_scn, vert_mir) {
            (true, _) => Mirroring::FourScreen,
            (false, true) => Mirroring::Vertical,
            (false, false) => Mirroring::Horizontal,
        };

        let trainer_size: usize = if (dump[6] & 0x04) == 1 {512} else {0};

        let mapper = (dump[6] >> 4) | (dump[7] & 0xF0);

        let prg_start = 16 + trainer_size;
        let chr_start = prg_start + prg_size;
        let prg_rom: Vec<u8> = dump[prg_start..(prg_start + prg_size)].to_vec();
        let chr_rom: Vec<u8> = dump[chr_start..(chr_start + chr_size)].to_vec();
        
        Ok( ROM {
            prg_rom,
            chr_rom,
            mapper,
            mirroring
        })
    }
}


