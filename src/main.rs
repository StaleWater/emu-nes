
use emu_nes::CPU;

fn main() {
    let program: Vec<u8> = vec![0xA9, 0x02, 0x69, 0x02, 0x00];

    let mut cpu = CPU::new();
    cpu.load(program);
    cpu.run();

    println!("{:?}", cpu.a);
    println!("{:?}", cpu.status);
}

