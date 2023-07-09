use std::mem;

mod mod_test;
mod utils;

use crate::opcode::CPU_OPS_CODES;

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

    fn mem_read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
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

        let value = (value << 1) & 0xc0;
        self.status |= value;
    }

    fn branch(&mut self, set: bool, flag_type: utils::FlagType) {
        if utils::flag_enabled(self.status, flag_type) == set {
            let offset = self.mem_read(self.program_counter) as i8;
            let counter = self.program_counter as i32 + offset as i32;
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

    fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_a);
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

    fn reset(&mut self) {
        self.register_a = 0;
        self.register_x = 0;
        self.status = 0;
        self.stack_pointer = 0xff;

        self.program_counter = self.mem_read_u16(0xFFFC);
    }

    fn load(&mut self, program: Vec<u8>) {
        self.memory[0x8000..(0x8000 + program.len())].copy_from_slice(&program[..]);
        self.mem_write_u16(0xFFFC, 0x8000);
    }

    fn run(&mut self) {
        loop {
            let opcode = CPU_OPS_CODES
                .iter()
                .find(|op| op.code == self.mem_read(self.program_counter));

            match opcode {
                Some(op) => match op.name.as_str() {
                    "ADC" => {
                        self.program_counter += 1;
                        self.adc(&op.mode);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "AND" => {
                        self.program_counter += 1;
                        self.and(&op.mode);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "ASL" => {
                        self.program_counter += 1;
                        self.asl(&op.mode);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "BCC" => {
                        self.program_counter += 1;
                        self.branch(false, utils::FlagType::CARRY);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "BCS" => {
                        self.program_counter += 1;
                        self.branch(true, utils::FlagType::CARRY);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "BEQ" => {
                        self.program_counter += 1;
                        self.branch(true, utils::FlagType::ZERO);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "BIT" => {
                        self.program_counter += 1;
                        self.bit(&op.mode);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "BMI" => {
                        self.program_counter += 1;
                        self.branch(true, utils::FlagType::NEGATIVE);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "BNE" => {
                        self.program_counter += 1;
                        self.branch(false, utils::FlagType::ZERO);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "BPL" => {
                        self.program_counter += 1;
                        self.branch(false, utils::FlagType::NEGATIVE);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "BVC" => {
                        self.program_counter += 1;
                        self.branch(false, utils::FlagType::OVERFLOW);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "BVS" => {
                        self.program_counter += 1;
                        self.branch(true, utils::FlagType::OVERFLOW);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "CLC" => {
                        utils::clear_flag(&mut self.status, utils::FlagType::CARRY);
                        self.program_counter += op.bytes as u16;
                    }
                    "CLD" => {
                        utils::clear_flag(&mut self.status, utils::FlagType::DECIMAL);
                        self.program_counter += op.bytes as u16;
                    }
                    "CLI" => {
                        utils::clear_flag(&mut self.status, utils::FlagType::INTERRUPT);
                        self.program_counter += op.bytes as u16;
                    }
                    "CLV" => {
                        utils::clear_flag(&mut self.status, utils::FlagType::OVERFLOW);
                        self.program_counter += op.bytes as u16;
                    }
                    "CMP" => {
                        self.program_counter += 1;
                        self.cmp(&op.mode);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "CPX" => {
                        self.program_counter += 1;
                        self.cpx(&op.mode);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "CPY" => {
                        self.program_counter += 1;
                        self.cpy(&op.mode);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "DEC" => {
                        self.program_counter += 1;
                        self.dec(&op.mode);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "DEX" => {
                        self.dex();
                        self.program_counter += op.bytes as u16;
                    }
                    "DEY" => {
                        self.dey();
                        self.program_counter += op.bytes as u16;
                    }
                    "EOR" => {
                        self.program_counter += 1;
                        self.eor(&op.mode);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "INC" => {
                        self.program_counter += 1;
                        self.inc(&op.mode);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "INX" => {
                        self.inx();
                        self.program_counter += op.bytes as u16;
                    }
                    "INY" => {
                        self.iny();
                        self.program_counter += op.bytes as u16;
                    }
                    "JMP" => {
                        self.program_counter += 1;
                        self.jmp(&op.mode);
                    }
                    "JSR" => {
                        self.program_counter += 1;
                        self.jsr(&op.mode);
                    }
                    "LDA" => {
                        self.program_counter += 1;
                        self.lda(&op.mode);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "LDX" => {
                        self.program_counter += 1;
                        self.ldx(&op.mode);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "LDY" => {
                        self.program_counter += 1;
                        self.ldy(&op.mode);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "LSR" => {
                        self.program_counter += 1;
                        self.lsr(&op.mode);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "RTS" => {
                        self.rts();
                    }
                    "SEC" => {
                        utils::set_flag(&mut self.status, utils::FlagType::CARRY);
                        self.program_counter += op.bytes as u16;
                    }
                    "SED" => {
                        utils::set_flag(&mut self.status, utils::FlagType::DECIMAL);
                        self.program_counter += op.bytes as u16;
                    }
                    "SEI" => {
                        utils::set_flag(&mut self.status, utils::FlagType::INTERRUPT);
                        self.program_counter += op.bytes as u16;
                    }
                    "STA" => {
                        self.program_counter += 1;
                        self.sta(&op.mode);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "STX" => {
                        self.program_counter += 1;
                        self.stx(&op.mode);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "STY" => {
                        self.program_counter += 1;
                        self.sty(&op.mode);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "TAX" => {
                        self.tax();
                        self.program_counter += op.bytes as u16;
                    }
                    "TAY" => {
                        self.tay();
                        self.program_counter += op.bytes as u16;
                    }
                    "TSX" => {
                        self.tsx();
                        self.program_counter += op.bytes as u16;
                    }
                    "TXA" => {
                        self.txa();
                        self.program_counter += op.bytes as u16;
                    }
                    "TXS" => {
                        self.txs();
                        self.program_counter += op.bytes as u16;
                    }
                    "TYA" => {
                        self.tya();
                        self.program_counter += op.bytes as u16;
                    }
                    "BRK" => return,
                    _ => todo!(),
                },
                None => todo!(),
            }
        }
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.reset();
        self.run()
    }
}
