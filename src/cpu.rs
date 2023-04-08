use std::mem;

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

        let mut carry = 0;
        if self.status & 0b0000_0001 == 1 {
            carry = 1;
        }

        let (res, overflow) = (self.register_a as i8).overflowing_add(m + carry);
        self.update_carry_flag(overflow);
        self.update_zero_and_negative_flags(res as u8);

        let v = (self.register_a ^ res as u8) & (m as u8 ^ res as u8) & 0x80;
        if v != 0 {
            self.status |= 0b0100_0000;
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

    fn branch(&mut self, set: bool, status: u8) {
        if (self.status & status) >= set as u8 {
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
            self.status = self.status | 0b0000_0001;
        } else {
            self.status = self.status & 0b1111_1110;
        }
    }

    fn update_zero_and_negative_flags(&mut self, result: u8) {
        if result == 0 {
            self.status = self.status | 0b0000_0010;
        } else {
            self.status = self.status & 0b1111_1101;
        }

        if result & 0b1000_0000 != 0 {
            self.status = self.status | 0b1000_0000;
        } else {
            self.status = self.status & 0b0111_1111;
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
                        self.branch(false, 0b0000_0001);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "BCS" => {
                        self.program_counter += 1;
                        self.branch(true, 0b0000_0001);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "BEQ" => {
                        self.program_counter += 1;
                        self.branch(true, 0b0000_0010);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "BIT" => {
                        self.program_counter += 1;
                        self.bit(&op.mode);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "BMI" => {
                        self.program_counter += 1;
                        self.branch(true, 0b1000_0000);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "BNE" => {
                        self.program_counter += 1;
                        self.branch(false, 0b0000_0010);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "BPL" => {
                        self.program_counter += 1;
                        self.branch(false, 0b1000_0000);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "BVC" => {
                        self.program_counter += 1;
                        self.branch(false, 0b0100_0000);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "BVS" => {
                        self.program_counter += 1;
                        self.branch(true, 0b0100_0000);
                        self.program_counter += (op.bytes - 1) as u16;
                    }
                    "CLC" => {
                        self.status &= 0b1111_1110;
                        self.program_counter += op.bytes as u16;
                    }
                    "CLD" => {
                        self.status &= 0b1111_0111;
                        self.program_counter += op.bytes as u16;
                    }
                    "CLI" => {
                        self.status &= 0b1111_1011;
                        self.program_counter += op.bytes as u16;
                    }
                    "CLV" => {
                        self.status &= 0b1011_1111;
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
                    "RTS" => {
                        self.rts();
                    }
                    "SEC" => {
                        self.status |= 0b0000_0001;
                        self.program_counter += op.bytes as u16;
                    }
                    "SED" => {
                        self.status |= 0b0000_1000;
                        self.program_counter += op.bytes as u16;
                    }
                    "SEI" => {
                        self.status |= 0b0000_0100;
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

mod test {
    use super::*;

    #[test]
    fn test_0xa9_lda_immidiate_load_data() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x05, 0x00]);
        assert_eq!(cpu.register_a, 0x05);
        assert!(cpu.status & 0b0000_0010 == 0b00);
        assert!(cpu.status & 0b1000_0000 == 0);
    }

    #[test]
    fn test_0xa9_lda_zero_flag() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0x00, 0x00]);
        assert!(cpu.status & 0b0000_0010 == 0b10);
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xaa, 0x00]);
        cpu.reset();
        cpu.register_a = 10;
        cpu.run();

        assert_eq!(cpu.register_x, 10)
    }

    #[test]
    fn test_0xaa_tay_move_a_to_y() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xa8, 0x00]);
        cpu.reset();
        cpu.register_a = 10;
        cpu.run();

        assert_eq!(cpu.register_y, 10)
    }

    #[test]
    fn test_0xfd_tsx_move_stack_to_x() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xba, 0x00]);
        cpu.reset();
        cpu.stack_pointer = 0xfd;
        cpu.run();

        assert_eq!(cpu.register_x, 0xfd)
    }

    #[test]
    fn test_0x01_txa_move_x_to_a() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x8a, 0x00]);
        cpu.reset();
        cpu.register_x = 0x01;
        cpu.run();

        assert_eq!(cpu.register_a, 0x01)
    }

    #[test]
    fn test_0xfd_txs_move_x_to_stack() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x9a, 0x00]);
        cpu.reset();
        cpu.register_x = 0xfd;
        cpu.run();

        assert_eq!(cpu.stack_pointer, 0xfd)
    }

    #[test]
    fn test_0xaa_tya_move_y_to_a() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x98, 0x00]);
        cpu.reset();
        cpu.register_y = 10;
        cpu.run();

        assert_eq!(cpu.register_a, 10)
    }

    #[test]
    fn test_dec_zero_page() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0x55);
        cpu.load_and_run(vec![0xc6, 0x10, 0x00]);

        assert_eq!(cpu.mem_read(0x10), 0x54);
    }

    #[test]
    fn test_cmp_zero_page() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xc5, 0x10, 0x00]);
        cpu.reset();
        cpu.register_a = 10;
        cpu.mem_write(0x10, 10);
        cpu.run();

        assert_eq!(cpu.status, 0b0000_0011);
    }

    #[test]
    fn test_cmp_zero_page_negative() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xc5, 0x10, 0x00]);
        cpu.reset();
        cpu.register_a = 0x00;
        cpu.mem_write(0x10, 0x01);
        cpu.run();

        assert_eq!(cpu.status, 0b1000_0000);
    }

    #[test]
    fn test_cpx_zero_page() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xe4, 0x10, 0x00]);
        cpu.reset();
        cpu.register_x = 10;
        cpu.mem_write(0x10, 10);
        cpu.run();

        assert_eq!(cpu.status, 0b0000_0011);
    }

    #[test]
    fn test_cpx_zero_page_negative() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xe4, 0x10, 0x00]);
        cpu.reset();
        cpu.register_x = 0x00;
        cpu.mem_write(0x10, 0x01);
        cpu.run();

        assert_eq!(cpu.status, 0b1000_0000);
    }

    #[test]
    fn test_cpy_zero_page() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xc4, 0x10, 0x00]);
        cpu.reset();
        cpu.register_y = 10;
        cpu.mem_write(0x10, 10);
        cpu.run();

        assert_eq!(cpu.status, 0b0000_0011);
    }

    #[test]
    fn test_cpy_zero_page_negative() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xc4, 0x10, 0x00]);
        cpu.reset();
        cpu.register_y = 0x00;
        cpu.mem_write(0x10, 0x01);
        cpu.run();

        assert_eq!(cpu.status, 0b1000_0000);
    }

    #[test]
    fn test_dex() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xca, 0x00]);
        cpu.reset();
        cpu.register_x = 10;
        cpu.run();

        assert_eq!(cpu.register_x, 9)
    }

    #[test]
    fn test_dey() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x88, 0x00]);
        cpu.reset();
        cpu.register_y = 10;
        cpu.run();

        assert_eq!(cpu.register_y, 9)
    }

    #[test]
    fn test_5_ops_working_together() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 0xc1)
    }

    #[test]
    fn test_inc_overflow() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xe6, 0x10, 0x00]);
        cpu.reset();
        cpu.mem_write(0x10, 0xff);
        cpu.run();

        assert_eq!(cpu.mem_read(0x10), 0);
        assert_eq!(cpu.status, 0b0000_0010)
    }

    #[test]
    fn test_inx_overflow() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xe8, 0xe8, 0x00]);
        cpu.reset();
        cpu.register_x = 0xff;
        cpu.run();

        assert_eq!(cpu.register_x, 1)
    }

    #[test]
    fn test_iny_overflow() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xc8, 0xc8, 0x00]);
        cpu.reset();
        cpu.register_y = 0xff;
        cpu.run();

        assert_eq!(cpu.register_y, 1)
    }

    #[test]
    fn test_jmp_absolute() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0x55);
        cpu.load_and_run(vec![0xe8, 0x4c, 0x07, 0x80, 0x00, 0x00, 0xe8, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 0x02);
    }

    #[test]
    fn test_jmp_indirect() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x1122, 0x07);
        cpu.mem_write(0x1123, 0x80);
        cpu.load_and_run(vec![0xe8, 0x6c, 0x22, 0x11, 0x00, 0x00, 0xe8, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 0x02);
    }

    #[test]
    fn test_jmp_indirect_last_byte_page() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x1100, 0x80);
        cpu.mem_write(0x11ff, 0x07);
        cpu.mem_write(0x1200, 0x79);
        cpu.load_and_run(vec![0xe8, 0x6c, 0xff, 0x11, 0x00, 0x00, 0xe8, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 0x02);
    }

    #[test]
    fn test_jsr() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0x55);
        cpu.load_and_run(vec![0x20, 0x05, 0x80, 0x00, 0x00, 0xe8, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 0x02);
        assert_eq!(cpu.stack_pointer, 0xfd);
        assert_eq!(cpu.mem_read_u16(0x01fe), 0x8003);
    }

    #[test]
    fn test_rts() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0x55);
        cpu.load_and_run(vec![0x20, 0x05, 0x80, 0xe8, 0x00, 0x60, 0x00]);

        assert_eq!(cpu.register_x, 0x01);
        assert_eq!(cpu.stack_pointer, 0xff);
    }

    #[test]
    fn test_lda_from_memory() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0x55);

        cpu.load_and_run(vec![0xa5, 0x10, 0x00]);

        assert_eq!(cpu.register_a, 0x55);
    }

    #[test]
    fn test_ldx_from_memory() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0x55);

        cpu.load_and_run(vec![0xa6, 0x10, 0x00]);

        assert_eq!(cpu.register_x, 0x55);
    }

    #[test]
    fn test_adc_immediate() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x69, 3, 0x00]);
        cpu.reset();
        cpu.register_a = 8;
        cpu.run();

        assert_eq!(cpu.register_a, 11);
        assert_eq!(cpu.status, 0);
    }

    #[test]
    fn test_adc_immediate_overflow() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x69, 0x80, 0x00]);
        cpu.reset();
        cpu.register_a = 0x80;
        cpu.run();

        assert_eq!(cpu.register_a as i8, 0);
        assert_eq!(cpu.status, 0b0100_0011);
    }

    #[test]
    fn test_0b0111_and_immediate() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x29, 0b0011, 0x00]);
        cpu.reset();
        cpu.register_a = 0b0111;
        cpu.run();

        assert_eq!(cpu.register_a, 0b0011);
    }

    #[test]
    fn test_0b0111_asl_accumulator() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x0a, 0x00]);
        cpu.reset();
        cpu.register_a = 0b0000_0111;
        cpu.run();

        assert_eq!(cpu.register_a, 0b0000_1110);
    }

    #[test]
    fn test_0b0010_asl_mem() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x06, 0x10, 0x00]);
        cpu.reset();
        cpu.mem_write(0x10, 0b0010);
        cpu.run();

        assert_eq!(cpu.mem_read(0x10), 0b0100);
    }

    #[test]
    fn test_0b1010_0000_asl_carry_mem() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x06, 0x10, 0x00]);
        cpu.reset();
        cpu.mem_write(0x10, 0b1010_0000);
        cpu.run();

        assert_eq!(cpu.mem_read(0x10), 0b0100_0000);
        assert!(cpu.status & 0b0000_0001 == 1);
    }

    #[test]
    fn test_0b1010_0000_asl_carry_accumulator() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x0a, 0x00]);
        cpu.reset();
        cpu.register_a = 0b1010_0000;
        cpu.run();

        assert_eq!(cpu.register_a, 0b0100_0000);
        assert!(cpu.status & 0b0000_0001 == 1);
    }

    #[test]
    fn test_bcc_forward() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x90, 0x01, 0xe8, 0xe8, 0xe8, 0x00]);
        cpu.reset();
        cpu.register_x = 0x00;
        cpu.run();

        assert_eq!(cpu.register_x, 2);
    }

    #[test]
    fn test_bcc_backward() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x90, 0x01, 0x00, 0xe8, 0xe8, 0x90, 0xfb, 0x00]);
        cpu.reset();
        cpu.register_x = 0x00;
        cpu.run();

        assert_eq!(cpu.register_x, 2);
    }

    #[test]
    fn test_bcs_forward() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xb0, 0x01, 0xe8, 0xe8, 0xe8, 0x00]);
        cpu.reset();
        cpu.status = 0x01;
        cpu.register_x = 0x00;
        cpu.run();

        assert_eq!(cpu.register_x, 2);
    }

    #[test]
    fn test_bcs_backward() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xb0, 0x01, 0x00, 0xe8, 0xe8, 0xb0, 0xfb, 0x00]);
        cpu.reset();
        cpu.status = 0x01;
        cpu.register_x = 0x00;
        cpu.run();

        assert_eq!(cpu.register_x, 2);
    }

    #[test]
    fn test_beq_forward() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xf0, 0x01, 0xe8, 0xe8, 0xe8, 0x00]);
        cpu.reset();
        cpu.status = 0b0010;
        cpu.register_x = 0x00;
        cpu.run();

        assert_eq!(cpu.register_x, 2);
    }

    #[test]
    fn test_beq_backward() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xf0, 0x01, 0x00, 0xe8, 0xe8, 0xf0, 0xfb, 0x00]);
        cpu.reset();
        cpu.status = 0b0010;
        cpu.register_x = 0x00;
        cpu.run();

        assert_eq!(cpu.register_x, 2);
    }

    #[test]
    fn test_bit_zero_page() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x24, 0x10, 0x00]);
        cpu.reset();
        cpu.register_a = 0b1001_1010;
        cpu.mem_write(0x10, 0b0110_0101);
        cpu.run();

        assert_eq!(cpu.status & 0b0000_0010, 0b0000_0010);
        assert_eq!(cpu.status & 0b0100_0000, 0b0100_0000);
        assert_eq!(cpu.status & 0b1000_0000, 0b1000_0000);
    }

    #[test]
    fn test_bmi_forward() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x30, 0x01, 0xe8, 0xe8, 0xe8, 0x00]);
        cpu.reset();
        cpu.status = 0b1000_0000;
        cpu.register_x = 0x00;
        cpu.run();

        assert_eq!(cpu.register_x, 2);
    }

    #[test]
    fn test_bmi_backward() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x30, 0x01, 0x00, 0xe8, 0xe8, 0x30, 0xfb, 0x00]);
        cpu.reset();
        cpu.status = 0b1000_0000;
        cpu.register_x = 0x00;
        cpu.run();

        assert_eq!(cpu.register_x, 2);
    }

    #[test]
    fn test_bne_forward() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xd0, 0x01, 0xe8, 0xe8, 0xe8, 0x00]);
        cpu.reset();
        cpu.status = 0;
        cpu.register_x = 0x00;
        cpu.run();

        assert_eq!(cpu.register_x, 2);
    }

    #[test]
    fn test_bne_backward() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xd0, 0x01, 0x00, 0xe8, 0xe8, 0xd0, 0xfb, 0x00]);
        cpu.reset();
        cpu.status = 0;
        cpu.register_x = 0x00;
        cpu.run();

        assert_eq!(cpu.register_x, 2);
    }
    #[test]
    fn test_bpl_forward() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x10, 0x01, 0xe8, 0xe8, 0xe8, 0x00]);
        cpu.reset();
        cpu.status = 0;
        cpu.register_x = 0x00;
        cpu.run();

        assert_eq!(cpu.register_x, 2);
    }

    #[test]
    fn test_bpl_backward() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x10, 0x01, 0x00, 0xe8, 0xe8, 0x10, 0xfb, 0x00]);
        cpu.reset();
        cpu.status = 0;
        cpu.register_x = 0x00;
        cpu.run();

        assert_eq!(cpu.register_x, 2);
    }

    #[test]
    fn test_bvc_forward() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x50, 0x01, 0xe8, 0xe8, 0xe8, 0x00]);
        cpu.reset();
        cpu.status = 0;
        cpu.register_x = 0x00;
        cpu.run();

        assert_eq!(cpu.register_x, 2);
    }

    #[test]
    fn test_bvc_backward() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x50, 0x01, 0x00, 0xe8, 0xe8, 0x50, 0xfb, 0x00]);
        cpu.reset();
        cpu.status = 0;
        cpu.register_x = 0x00;
        cpu.run();

        assert_eq!(cpu.register_x, 2);
    }

    #[test]
    fn test_bvs_forward() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x70, 0x01, 0xe8, 0xe8, 0xe8, 0x00]);
        cpu.reset();
        cpu.status = 0b0100_0000;
        cpu.register_x = 0x00;
        cpu.run();

        assert_eq!(cpu.register_x, 2);
    }

    #[test]
    fn test_bvs_backward() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x70, 0x01, 0x00, 0xe8, 0xe8, 0x70, 0xfb, 0x00]);
        cpu.reset();
        cpu.status = 0b0100_0000;
        cpu.register_x = 0x00;
        cpu.run();

        assert_eq!(cpu.register_x, 2);
    }

    #[test]
    fn test_clc() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x18, 0x00]);
        cpu.reset();
        cpu.status = 0b0101_0101;
        cpu.run();

        assert_eq!(cpu.status, 0b0101_0100);
    }

    #[test]
    fn test_cld() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xd8, 0x00]);
        cpu.reset();
        cpu.status = 0b1010_1010;
        cpu.run();

        assert_eq!(cpu.status, 0b1010_0010);
    }

    #[test]
    fn test_cli() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x58, 0x00]);
        cpu.reset();
        cpu.status = 0b0101_0101;
        cpu.run();

        assert_eq!(cpu.status, 0b0101_0001);
    }

    #[test]
    fn test_clv() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xb8, 0x00]);
        cpu.reset();
        cpu.status = 0b0101_0101;
        cpu.run();

        assert_eq!(cpu.status, 0b0001_0101);
    }

    #[test]
    fn test_sec() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x38, 0x00]);
        cpu.reset();
        cpu.status = 0b1010_1010;
        cpu.run();

        assert_eq!(cpu.status, 0b1010_1011);
    }

    #[test]
    fn test_sed() {
        let mut cpu = CPU::new();
        cpu.load(vec![0xf8, 0x00]);
        cpu.reset();
        cpu.status = 0b0101_0101;
        cpu.run();

        assert_eq!(cpu.status, 0b0101_1101);
    }

    #[test]
    fn test_sei() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x78, 0x00]);
        cpu.reset();
        cpu.status = 0b1010_1010;
        cpu.run();

        assert_eq!(cpu.status, 0b1010_1110);
    }

    #[test]
    fn test_sta_zero_page() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x85, 0x10, 0x00]);
        cpu.reset();
        cpu.register_a = 0x0f;
        cpu.run();

        assert_eq!(cpu.mem_read(0x10), 0x0f);
    }

    #[test]
    fn test_stx_zero_page() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x86, 0x10, 0x00]);
        cpu.reset();
        cpu.register_x = 0x0f;
        cpu.run();

        assert_eq!(cpu.mem_read(0x10), 0x0f);
    }

    #[test]
    fn test_sty_zero_page() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x84, 0x10, 0x00]);
        cpu.reset();
        cpu.register_y = 0x0f;
        cpu.run();

        assert_eq!(cpu.mem_read(0x10), 0x0f);
    }

    #[test]
    fn test_0b0111_eor_immediate() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x49, 0b0011, 0x00]);
        cpu.reset();
        cpu.register_a = 0b0111;
        cpu.run();

        assert_eq!(cpu.register_a, 0b0100);
    }
}
