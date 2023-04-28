use super::CPU;

fn setup(program: Vec<u8>) -> CPU {
    let mut cpu = CPU::new();
    cpu.load(program);
    cpu.run();
    cpu
}

#[test]
fn simple_test() {
    let program: Vec<u8> = vec![0xA9, 0xB0, 0xAA, 0xE8, 0x00];
    let cpu = setup(program);

    assert_eq!(cpu.a, 0xB0);
    assert_eq!(cpu.x, 0xB1);
    println!("{:?}", cpu.status);
}

#[test]
fn cmp_test() {
    let program: Vec<u8> = vec![0xA9, 0xF2, 0xC9, 0xF1, 0x00];
    let mut cpu = setup(program);

    assert_eq!(cpu.status.zero, false);
    assert_eq!(cpu.status.negative, false);
    assert_eq!(cpu.status.carry, true);

    let program: Vec<u8> = vec![0xA9, 0x01, 0xC9, 0x02, 0x00];
    cpu = setup(program);

    assert_eq!(cpu.status.zero, false);
    assert_eq!(cpu.status.negative, true);
    assert_eq!(cpu.status.carry, false);

}

#[test]
fn zero_test() {
    let program: Vec<u8> = vec![0xA9, 0x00, 0x00];
    let cpu = setup(program);

    assert_eq!(cpu.a, 0x00);
    println!("{:?}", cpu.status);
}

#[test]
fn negative_test() {
    let program: Vec<u8> = vec![0xA9, 0xFF, 0x00];
    let cpu = setup(program);

    assert_eq!(cpu.a, 0xFF);
    assert_eq!(cpu.status.negative, true);
}

#[test]
fn adc_test1() {
    let program: Vec<u8> = vec![0xA9, 0x02, 0x69, 0x02, 0x00];

    let cpu = setup(program);

    assert_eq!(cpu.a, 0x04);
    assert_eq!(cpu.status.carry, false);
    println!("{:?}", cpu.status);
}

#[test]
fn adc_test2() {
    let program: Vec<u8> = vec![0xA9, 0xFF, 0x69, 0xFF, 0x00];

    let cpu = setup(program);

    assert_eq!(cpu.a, 0xFE);
    assert_eq!(cpu.status.carry, true);
    println!("{:?}", cpu.status);
}

#[test]
fn overflow_test() {
    let mut a: u8 = 0xFF;
    let mut b: u8 = 0xFF;
    let mut cin = false;
    let (sum, c, v) = CPU::add_cv(a, b, cin);
    assert_eq!(sum, 0xFE);
    assert_eq!(c, true);
    assert_eq!(v, false);

    a = 0x7F;
    b = 0x7F;
    cin = true;
    let (sum, c, v) = CPU::add_cv(a, b, cin);
    assert_eq!(sum, 0xFF);
    assert_eq!(c, false);
    assert_eq!(v, true);

}

