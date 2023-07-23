use std::mem;

mod mod_test;
mod utils;

use crate::opcode::{OpCode, CPU_OPS_CODES};

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum AddressingMode {
    Immediate,
    ZeroPage,
    ZeroPage_X,
    ZeroPage_Y,
    Absolute,
    Absolute_X,
    Absolute_Y,
    Indirect,
    Indirect_X,
    Indirect_Y,
    NoneAddressing,
}

pub struct CPU {
    pub register_a: u8,
    pub status: u8,
    pub program_counter: u16,
    pub stack_pointer: u8,
    pub register_x: u8,
    pub register_y: u8,
    memory: [u8; 0xFFFF],
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            register_a: 0,
            status: 0,
            program_counter: 0,
            stack_pointer: 0,
            register_x: 0,
            register_y: 0,
            memory: [0; 0xFFFF],
        }
    }

    pub fn mem_read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    pub fn mem_write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data;
    }

    fn mem_read_u16(&self, pos: u16) -> u16 {
        let lo = self.mem_read(pos) as u16;
        let hi = self.mem_read(pos + 1) as u16;
        (hi << 8) | (lo as u16)
    }

    fn mem_write_u16(&mut self, pos: u16, data: u16) {
        let hi = (data >> 8) as u8;
        let lo = (data & 0xff) as u8;
        self.mem_write(pos, lo);
        self.mem_write(pos + 1, hi);
    }

    fn get_operand_address(&self, mode: &AddressingMode) -> u16 {
        match mode {
            AddressingMode::Immediate => self.program_counter,
            AddressingMode::ZeroPage => self.mem_read(self.program_counter) as u16,
            AddressingMode::Absolute => self.mem_read_u16(self.program_counter),
            AddressingMode::ZeroPage_X => {
                let pos = self.mem_read(self.program_counter);
                let addr = pos.wrapping_add(self.register_x) as u16;
                addr
            }
            AddressingMode::ZeroPage_Y => {
                let pos = self.mem_read(self.program_counter);
                let addr = pos.wrapping_add(self.register_y) as u16;
                addr
            }
            AddressingMode::Absolute_X => {
                let base = self.mem_read_u16(self.program_counter);
                let addr = base.wrapping_add(self.register_x as u16);
                addr
            }
            AddressingMode::Absolute_Y => {
                let base = self.mem_read_u16(self.program_counter);
                let addr = base.wrapping_add(self.register_y as u16);
                addr
            }
            AddressingMode::Indirect => {
                let base = self.mem_read_u16(self.program_counter);
                let lo = self.mem_read(base as u16);
                let mut hi = self.mem_read(base.wrapping_add(1) as u16);
                if (base & 0x00ff) == 0xff {
                    hi = self.mem_read(base & 0xff00);
                }

                (hi as u16) << 8 | (lo as u16)
            }
            AddressingMode::Indirect_X => {
                let base = self.mem_read(self.program_counter);

                let ptr: u8 = (base as u8).wrapping_add(self.register_x);
                let lo = self.mem_read(ptr as u16);
                let hi = self.mem_read(ptr.wrapping_add(1) as u16);
                (hi as u16) << 8 | (lo as u16)
            }
            AddressingMode::Indirect_Y => {
                let base = self.mem_read(self.program_counter);

                let lo = self.mem_read(base as u16);
                let hi = self.mem_read((base as u8).wrapping_add(1) as u16);
                let deref_base = (hi as u16) << 8 | (lo as u16);
                let deref = deref_base.wrapping_add(self.register_y as u16);
                deref
            }
            AddressingMode::NoneAddressing => {
                panic!("mode {:?} is not supported", mode);
            }
        }
    }

    fn adc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let m = self.mem_read(addr) as i8;

        let (res, overflow) = (self.register_a as i8)
            .overflowing_add(m + utils::flag_enabled(self.status, utils::FlagType::OVERFLOW) as i8);
        self.update_carry_flag(overflow);
        self.update_zero_and_negative_flags(res as u8);

        let v = (self.register_a ^ res as u8) & (m as u8 ^ res as u8) & 0x80;
        if v != 0 {
            utils::set_flag(&mut self.status, utils::FlagType::OVERFLOW);
        }

        self.register_a = res as u8;
    }

    fn and(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a &= value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn asl(&mut self, mode: &AddressingMode) {
        match mode {
            AddressingMode::NoneAddressing => {
                let (res, _) = self.register_a.overflowing_shl(1);
                let carry = if ((self.register_a as u32) << 1) > u8::MAX as u32 {
                    true
                } else {
                    false
                };
                self.register_a = res;
                self.update_carry_flag(carry);
                self.update_zero_and_negative_flags(self.register_a);
            }
            _ => {
                let addr = self.get_operand_address(mode);
                let value = self.mem_read(addr);
                let (res, _) = value.overflowing_shl(1);
                let carry = if ((value as u32) << 1) > u8::MAX as u32 {
                    true
                } else {
                    false
                };
                self.mem_write(addr, res);
                self.update_carry_flag(carry);
                self.update_zero_and_negative_flags(value);
            }
        }
    }

    fn bit(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);
        let res = self.register_a & value;
        self.update_zero_and_negative_flags(res);

        if value & 0b1000_0000 > 0 {
            utils::set_flag(&mut self.status, utils::FlagType::NEGATIVE);
        } else {
            utils::clear_flag(&mut self.status, utils::FlagType::NEGATIVE);
        }

        if value & 0b0100_0000 > 0 {
            utils::set_flag(&mut self.status, utils::FlagType::OVERFLOW);
        } else {
            utils::clear_flag(&mut self.status, utils::FlagType::OVERFLOW);
        }
    }

    fn branch(&mut self, set: bool, flag_type: utils::FlagType) {
        if utils::flag_enabled(self.status, flag_type) == set {
            let offset = self.mem_read(self.program_counter) as i8;
            let counter = self.program_counter as i32 + offset as i32 + 1;
            self.program_counter = counter as u16;
        }
    }

    fn cmp(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr) as i8;
        let res = self.register_a as i16 - value as i16;
        self.update_carry_flag(self.register_a as i16 >= value as i16);
        self.update_zero_and_negative_flags(res as u8);
    }

    fn cpx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr) as i8;
        let res = self.register_x as i16 - value as i16;
        self.update_carry_flag(self.register_x as i16 >= value as i16);
        self.update_zero_and_negative_flags(res as u8);
    }

    fn cpy(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr) as i8;
        let res = self.register_y as i16 - value as i16;
        self.update_carry_flag(self.register_y as i16 >= value as i16);
        self.update_zero_and_negative_flags(res as u8);
    }

    fn dec(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        let value = (value as i16) - 1;
        self.mem_write(addr, value as u8);
        self.update_zero_and_negative_flags(value as u8);
    }

    fn dex(&mut self) {
        let value = (self.register_x as i16) - 1;
        self.register_x = value as u8;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn dey(&mut self) {
        let value = (self.register_y as i16) - 1;
        self.register_y = value as u8;
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn eor(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a ^= value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn inc(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let mut value = self.mem_read(addr);
        value = match value {
            0xff => 0,
            _ => value + 1,
        };

        self.mem_write(addr, value);
        self.update_zero_and_negative_flags(value);
    }

    fn inx(&mut self) {
        self.register_x = match self.register_x {
            0xff => 0,
            _ => self.register_x + 1,
        };
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn iny(&mut self) {
        self.register_y = match self.register_y {
            0xff => 0,
            _ => self.register_y + 1,
        };
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn jmp(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.program_counter = addr;
    }

    fn push_stack_u16(&mut self, value: u16) {
        let pos = 0x0100 | self.stack_pointer.wrapping_sub(1) as u16;
        self.mem_write_u16(pos, value);
        self.stack_pointer = self.stack_pointer.wrapping_sub(mem::size_of::<u16>() as u8);
    }

    fn pop_stack_u16(&mut self) -> u16 {
        let pos = 0x0100 | self.stack_pointer.wrapping_add(1) as u16;
        let val = self.mem_read_u16(pos);
        self.stack_pointer = self.stack_pointer.wrapping_add(mem::size_of::<u16>() as u8);
        val
    }

    fn push_stack(&mut self, value: u8) {
        let pos = 0x0100 | self.stack_pointer as u16;
        self.mem_write(pos, value);
        self.stack_pointer = self.stack_pointer.wrapping_sub(1);
    }

    fn pop_stack(&mut self) -> u8 {
        let pos = 0x0100 | self.stack_pointer.wrapping_add(1) as u16;
        let val = self.mem_read(pos);
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        val
    }

    fn jsr(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.push_stack_u16(self.program_counter + mem::size_of::<u16>() as u16);
        self.program_counter = addr;
    }

    fn rts(&mut self) {
        let addr = self.pop_stack_u16();
        self.program_counter = addr;
    }

    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a = value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn ldx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_x = value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn ldy(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_y = value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn lsr(&mut self, mode: &AddressingMode) {
        let value;
        let last_bit: u8;
        match mode {
            AddressingMode::NoneAddressing => {
                last_bit = self.register_a & 0x01;
                value = self.register_a >> 1;
                self.register_a = value;
            }

            _ => {
                let addr = self.get_operand_address(mode);
                last_bit = self.mem_read(addr) & 0x01;
                value = self.mem_read(addr) >> 1;
                self.mem_write(addr, value);
            }
        }

        self.update_carry_flag(last_bit != 0);
        self.update_zero_and_negative_flags(value);
    }

    fn nop(&mut self) {}

    fn ora(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a = self.register_a | value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn pha(&mut self) {
        self.push_stack(self.register_a);
    }

    fn php(&mut self) {
        self.push_stack(self.status);
        utils::set_flag(&mut self.status, utils::FlagType::BIT_5);
        utils::set_flag(&mut self.status, utils::FlagType::B_FLAG);
    }

    fn pla(&mut self) {
        self.register_a = self.pop_stack();
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn plp(&mut self) {
        self.status = self.pop_stack();
    }

    fn rol(&mut self, mode: &AddressingMode) {
        let status = self.status;
        let carry = utils::flag_enabled(self.status, utils::FlagType::CARRY);

        match mode {
            AddressingMode::NoneAddressing => {
                if self.register_a >> 7 == 1 {
                    utils::set_flag(&mut self.status, utils::FlagType::CARRY);
                } else {
                    utils::clear_flag(&mut self.status, utils::FlagType::CARRY);
                }

                if carry {
                    self.register_a = (self.register_a << 1) | 0b0000_0001;
                } else {
                    self.register_a = (self.register_a << 1) & 0b1111_1110;
                }
            }
            _ => {
                let addr = self.get_operand_address(mode);
                let mut value = self.mem_read(addr);

                if value >> 7 == 1 {
                    utils::set_flag(&mut self.status, utils::FlagType::CARRY);
                } else {
                    utils::clear_flag(&mut self.status, utils::FlagType::CARRY);
                }

                if carry {
                    value = (value << 1) | 0b0000_0001;
                } else {
                    value = (value << 1) & 0b1111_1110;
                }

                self.mem_write(addr, value);
            }
        }
    }

    fn ror(&mut self, mode: &AddressingMode) {
        let status = self.status;
        let carry = utils::flag_enabled(self.status, utils::FlagType::CARRY);

        match mode {
            AddressingMode::NoneAddressing => {
                if self.register_a & 1 == 1 {
                    utils::set_flag(&mut self.status, utils::FlagType::CARRY);
                } else {
                    utils::clear_flag(&mut self.status, utils::FlagType::CARRY);
                }

                if carry {
                    self.register_a = (self.register_a >> 1) | 0b1000_0000;
                } else {
                    self.register_a = (self.register_a >> 1) & 0b0111_1111;
                }
            }
            _ => {
                let addr = self.get_operand_address(mode);
                let mut value = self.mem_read(addr);

                if value & 1 == 1 {
                    utils::set_flag(&mut self.status, utils::FlagType::CARRY);
                } else {
                    utils::clear_flag(&mut self.status, utils::FlagType::CARRY);
                }

                if carry {
                    value = (value >> 1) | 0b1000_0000;
                } else {
                    value = (value >> 1) & 0b0111_1111;
                }

                self.mem_write(addr, value);
            }
        }
    }

    fn rti(&mut self) {
        self.status = self.pop_stack();
        self.program_counter = self.pop_stack_u16();
    }

    fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_a);
    }

    fn sbc(&mut self, mode: &AddressingMode) {
        self.register_a = !self.register_a + 1;
        self.adc(mode);
    }

    fn stx(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_x);
    }

    fn sty(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_y);
    }

    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn tay(&mut self) {
        self.register_y = self.register_a;
        self.update_zero_and_negative_flags(self.register_y);
    }

    fn tsx(&mut self) {
        self.register_x = self.stack_pointer;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn txa(&mut self) {
        self.register_a = self.register_x;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn txs(&mut self) {
        self.stack_pointer = self.register_x;
        self.update_zero_and_negative_flags(self.stack_pointer);
    }

    fn tya(&mut self) {
        self.register_a = self.register_y;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn update_carry_flag(&mut self, overflow: bool) {
        if overflow {
            utils::set_flag(&mut self.status, utils::FlagType::CARRY);
        } else {
            utils::clear_flag(&mut self.status, utils::FlagType::CARRY);
        }
    }

    fn update_zero_and_negative_flags(&mut self, result: u8) {
        if result == 0 {
            utils::set_flag(&mut self.status, utils::FlagType::ZERO);
        } else {
            utils::clear_flag(&mut self.status, utils::FlagType::ZERO);
        }

        if result & 0b1000_0000 != 0 {
            utils::set_flag(&mut self.status, utils::FlagType::NEGATIVE);
        } else {
            utils::clear_flag(&mut self.status, utils::FlagType::NEGATIVE);
        }
    }

    pub fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.status = 0;
        self.stack_pointer = 0xff;

        self.program_counter = self.mem_read_u16(0xFFFC);
    }

    pub fn load(&mut self, program: Vec<u8>) {
        // self.memory[0x8000..(0x8000 + program.len())].copy_from_slice(&program[..]);
        // self.mem_write_u16(0xFFFC, 0x8000);

        self.memory[0x0600..(0x0600 + program.len())].copy_from_slice(&program[..]);
        self.mem_write_u16(0xFFFC, 0x0600);
    }

    pub fn run(&mut self) {
        self.run_with_callback(|_| {});
    }

    fn DebugOp(&self, op: &OpCode) {
        print!("Operation: {} ", op.name);
        let a = self.mem_read(self.program_counter);
        let b = self.mem_read(self.program_counter + 1);
        match op.mode {
            AddressingMode::Immediate => println!("#{:#x}", a),
            AddressingMode::ZeroPage => println!("${:#x}", a),
            AddressingMode::Absolute => println!("${:#x}", ((b as u16) << 8) | (a as u16)),
            AddressingMode::ZeroPage_X => println!("${:#x},x", a),
            AddressingMode::ZeroPage_Y => println!("${:#x},y", a),
            AddressingMode::Absolute_X => println!("${:#x},x", ((b as u16) << 8) | (a as u16)),
            AddressingMode::Absolute_Y => println!("${:#x},y", ((b as u16) << 8) | (a as u16)),
            AddressingMode::Indirect => println!("(${:#x})", ((b as u16) << 8) | (a as u16)),
            AddressingMode::Indirect_X => println!("(${:#x},x)", a),
            AddressingMode::Indirect_Y => {
                println!("$({:#x}),y", ((b as u16) << 8) | (a as u16))
            }
            AddressingMode::NoneAddressing => println!(""),
        }
    }

    fn DebugGame(&self) {
        println!(
            "head snake: {},{}",
            self.mem_read(0x10),
            self.mem_read(0x11)
        );
    }

    pub fn run_with_callback<F>(&mut self, mut callback: F)
    where
        F: FnMut(&mut CPU),
    {
        loop {
            callback(self);

            let opcode = CPU_OPS_CODES
                .iter()
                .find(|op| op.code == self.mem_read(self.program_counter));

            self.program_counter += 1;
            let pc_current = self.program_counter;

            let op = opcode.unwrap();

            self.DebugOp(op);

            match op.name.as_str() {
                "ADC" => self.adc(&op.mode),
                "AND" => self.and(&op.mode),
                "ASL" => self.asl(&op.mode),
                "BCC" => self.branch(false, utils::FlagType::CARRY),
                "BCS" => self.branch(true, utils::FlagType::CARRY),
                "BEQ" => self.branch(true, utils::FlagType::ZERO),
                "BIT" => self.bit(&op.mode),
                "BMI" => self.branch(true, utils::FlagType::NEGATIVE),
                "BNE" => self.branch(false, utils::FlagType::ZERO),
                "BPL" => self.branch(false, utils::FlagType::NEGATIVE),
                "BVC" => self.branch(false, utils::FlagType::OVERFLOW),
                "BVS" => self.branch(true, utils::FlagType::OVERFLOW),
                "CLC" => utils::clear_flag(&mut self.status, utils::FlagType::CARRY),
                "CLD" => utils::clear_flag(&mut self.status, utils::FlagType::DECIMAL),
                "CLI" => utils::clear_flag(&mut self.status, utils::FlagType::INTERRUPT),
                "CLV" => utils::clear_flag(&mut self.status, utils::FlagType::OVERFLOW),
                "CMP" => self.cmp(&op.mode),
                "CPX" => self.cpx(&op.mode),
                "CPY" => self.cpy(&op.mode),
                "DEC" => self.dec(&op.mode),
                "DEX" => self.dex(),
                "DEY" => self.dey(),
                "EOR" => self.eor(&op.mode),
                "INC" => self.inc(&op.mode),
                "INX" => self.inx(),
                "INY" => self.iny(),
                "JMP" => self.jmp(&op.mode),
                "JSR" => self.jsr(&op.mode),
                "LDA" => self.lda(&op.mode),
                "LDX" => self.ldx(&op.mode),
                "LDY" => self.ldy(&op.mode),
                "LSR" => self.lsr(&op.mode),
                "NOP" => self.nop(),
                "ORA" => self.ora(&op.mode),
                "PHA" => self.pha(),
                "PHP" => self.php(),
                "PLA" => self.pla(),
                "PLP" => self.plp(),
                "ROL" => self.rol(&op.mode),
                "ROR" => self.ror(&op.mode),
                "RTI" => self.rti(),
                "RTS" => self.rts(),
                "SBC" => self.sbc(&op.mode),
                "SEC" => utils::set_flag(&mut self.status, utils::FlagType::CARRY),
                "SED" => utils::set_flag(&mut self.status, utils::FlagType::DECIMAL),
                "SEI" => utils::set_flag(&mut self.status, utils::FlagType::INTERRUPT),
                "STA" => self.sta(&op.mode),
                "STX" => self.stx(&op.mode),
                "STY" => self.sty(&op.mode),
                "TAX" => self.tax(),
                "TAY" => self.tay(),
                "TSX" => self.tsx(),
                "TXA" => self.txa(),
                "TXS" => self.txs(),
                "TYA" => self.tya(),
                "BRK" => return,
                _ => todo!(),
            }

            self.DebugGame();

            if pc_current == self.program_counter {
                self.program_counter += (op.bytes - 1) as u16;
            }
        }
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run()
    }
}
