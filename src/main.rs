use sdl2::pixels::PixelFormatEnum;
use sdl2::EventPump;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::pixels::Color;
use rand::Rng;
use std::fs;

use emu_nes::cpu::CPU;
use emu_nes::mem::Memory;
use emu_nes::mem::rom::ROM;

fn main() {
    const SCALE: u32 = 10;

    let program = fs::read("../snake.nes").unwrap();
    
    let sdl_cxt = sdl2::init().unwrap();
    let mut event_pump = sdl_cxt.event_pump().unwrap();
    let video = sdl_cxt.video().unwrap();
    let window = video.window("Snakers", (32 * SCALE) as u32, (32 * SCALE) as u32)
        .position_centered()
        .build().unwrap();

    let mut canvas = window.into_canvas().present_vsync().build().unwrap();
    canvas.set_scale(10.0, 10.0).unwrap();

    let creator = canvas.texture_creator();
    let mut texture = creator.create_texture_target(PixelFormatEnum::RGB24, 32, 32).unwrap();

    let mut frame = [0 as u8; 32 * 32 * 3];

    let mut rng = rand::thread_rng();

    let rom = ROM::new(program).expect("Failed to build ROM struct");
    let mut cpu = CPU::new(rom);

    //match cpu.load_prg_rom() {
    //    Err(e) => {
    //        panic!("{0}", e);
    //    },
    //    Ok(_) => {}
    //}

    cpu.run_with_callback(move |cpu| {
        cpu.print_state();
        handle_input(cpu, &mut event_pump);
        cpu.write_mem(0xfe, rng.gen_range(1..16));

        if update_frame(cpu, &mut frame) {
            texture.update(None, &frame, 32 * 3).unwrap();
            canvas.copy(&texture, None, None).unwrap();
            canvas.present();
        }

        std::thread::sleep(std::time::Duration::new(0, 40000));
    });

    println!("CLOSING");

}

fn color_map(cb: u8) -> Color {
    match cb {
        0 => Color::BLACK,
        1 => Color::WHITE,
        2 | 9 => Color::GREY,
        3 | 10 => Color::RED,
        4 | 11 => Color::GREEN,
        5 | 12 => Color::BLUE,
        6 | 13 => Color::MAGENTA,
        7 | 14 => Color::YELLOW,
        _ => Color::CYAN,
    }
}

fn update_frame(cpu: &CPU, frame: &mut [u8; 32 * 32 * 3]) -> bool {
    let mut updated = false;
    let mut fi = 0;

    for i in 0x0200..0x0600 {
        let ci = cpu.read_mem(i as u16);
        let (c0, c1, c2) = color_map(ci).rgb();
        if frame[fi] != c0 || frame[fi + 1] != c1 || frame[fi + 2] != c2 {
            frame[fi] = c0;
            frame[fi + 1] = c1;
            frame[fi + 2] = c2;
            updated = true;
        }
        fi += 3;
    }

    updated
}

fn handle_input(cpu: &mut CPU, epump: &mut EventPump) {
    for event in epump.poll_iter() {
        match event {
            Event::Quit {..} | Event::KeyDown { keycode: Some(Keycode::Escape), .. } => std::process::exit(0),
            Event::KeyDown { keycode: Some(Keycode::W), .. } => cpu.write_mem(0xff, 0x77),     
            Event::KeyDown { keycode: Some(Keycode::S), .. } => cpu.write_mem(0xff, 0x73),     
            Event::KeyDown { keycode: Some(Keycode::A), .. } => cpu.write_mem(0xff, 0x61),     
            Event::KeyDown { keycode: Some(Keycode::D), .. } => cpu.write_mem(0xff, 0x64),     
            _ => {}
        }
    }
}



