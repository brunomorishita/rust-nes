use lazy_static::lazy_static;

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
    Indirect_X,
    Indirect_Y,
    NoneAddressing,
}

pub struct OpCode {
    pub code: u8,
    pub name: String,
    pub bytes: u8,
    pub cycles: u8,
    pub mode: AddressingMode,
}

impl OpCode {
    fn new(code: u8, name: &str, bytes: u8, cycles: u8, mode: AddressingMode) -> Self {
        OpCode {
            code,
            name: name.to_string(),
            bytes,
            cycles,
            mode,
        }
    }
}

lazy_static! {
    pub static ref CPU_OPS_CODES: Vec<OpCode> = vec![
        // A logical AND is performed, bit by bit, on the accumulator contents
        // using the contents of a byte of memory.
        OpCode::new(0x29, "AND", 2, 2, AddressingMode::Immediate),
        OpCode::new(0x25, "AND", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x35, "AND", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0x2d, "AND", 3, 4, AddressingMode::Absolute),
        OpCode::new(0x3d, "AND", 3, 4, AddressingMode::Absolute_X),
        OpCode::new(0x39, "AND", 3, 4, AddressingMode::Absolute_Y),
        OpCode::new(0x21, "AND", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(0x31, "AND", 2, 5, AddressingMode::Indirect_Y),
        // This operation shifts all the bits of the accumulator or memory contents one bit left.
        // Bit 0 is set to 0 and bit 7 is placed in the carry flag.
        // The effect of this operation is to multiply the memory contents by 2 (ignoring 2's complement considerations),
        // setting the carry if the result will not fit in 8 bits.
        OpCode::new(0x0a, "ASL", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x06, "ASL", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0x16, "ASL", 2, 6, AddressingMode::ZeroPage_X),
        OpCode::new(0x0e, "ASL", 3, 6, AddressingMode::Absolute),
        OpCode::new(0x1e, "ASL", 3, 7, AddressingMode::Absolute_X),
        // If the carry flag is clear then add the relative displacement
        // to the program counter to cause a branch to a new location.
        OpCode::new(0x90, "BCC", 2, 2, AddressingMode::NoneAddressing),
        // If the carry flag is set then add the relative displacement
        // to the program counter to cause a branch to a new location.
        OpCode::new(0xb0, "BCS", 2, 2, AddressingMode::NoneAddressing),
        // If the zero flag is set then add the relative displacement
        // to the program counter to cause a branch to a new location.
        OpCode::new(0xf0, "BEQ", 2, 2, AddressingMode::NoneAddressing),
        // If the negative flag is set then add the relative displacement
        // to the program counter to cause a branch to a new location.
        OpCode::new(0x30, "BMI", 2, 2, AddressingMode::NoneAddressing),
        // If the zero flag is clear then add the relative displacement
        // to the program counter to cause a branch to a new location.
        OpCode::new(0xd0, "BNE", 2, 2, AddressingMode::NoneAddressing),
        // If the negative flag is clear then add the relative displacement
        // to the program counter to cause a branch to a new location.
        OpCode::new(0x10, "BPL", 2, 2, AddressingMode::NoneAddressing),
        // The BRK instruction forces the generation of an interrupt request.
        // The program counter and processor status are pushed on the stack then
        // the IRQ interrupt vector at $FFFE/F is loaded into the PC and the break flag
        // in the status set to one.
        OpCode::new(0x00, "BRK", 1, 7, AddressingMode::NoneAddressing),
        // If the overflow flag is clear then add the relative displacement
        // to the program counter to cause a branch to a new location.
        OpCode::new(0x50, "BVC", 2, 2, AddressingMode::NoneAddressing),
        // If the overflow flag is set then add the relative displacement
        // to the program counter to cause a branch to a new location.
        OpCode::new(0x70, "BVS", 2, 2, AddressingMode::NoneAddressing),
        // Set the carry flag to zero.
        OpCode::new(0x18, "CLC", 1, 2, AddressingMode::NoneAddressing),
        // Sets the decimal mode flag to zero.
        OpCode::new(0xd8, "CLD", 1, 2, AddressingMode::NoneAddressing),
        // Clears the interrupt disable flag allowing normal interrupt requests to be serviced.
        OpCode::new(0x58, "CLI", 1, 2, AddressingMode::NoneAddressing),
        // Clears the overflow flag.
        OpCode::new(0xb8, "CLV", 1, 2, AddressingMode::NoneAddressing),
        // Adds one to the X register setting the zero and negative flags as appropriate.
        OpCode::new(0xe8, "INX", 1, 2, AddressingMode::NoneAddressing),
        // Adds one to the Y register setting the zero and negative flags as appropriate.
        OpCode::new(0xc8, "INY", 1, 2, AddressingMode::NoneAddressing),
        // Set the carry flag to one.
        OpCode::new(0x38, "SEC", 1, 2, AddressingMode::NoneAddressing),
        // Set the decimal mode flag to one.
        OpCode::new(0xf8, "SED", 1, 2, AddressingMode::NoneAddressing),
        // Set the interrupt disable flag to one.
        OpCode::new(0x78, "SEI", 1, 2, AddressingMode::NoneAddressing),
        // Stores the contents of the accumulator into memory
        OpCode::new(0x85, "STA", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x95, "STA", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0x8d, "STA", 2, 4, AddressingMode::Absolute),
        OpCode::new(0x9d, "STA", 2, 4, AddressingMode::Absolute_X),
        OpCode::new(0x99, "STA", 2, 4, AddressingMode::Absolute_Y),
        OpCode::new(0x81, "STA", 2, 4, AddressingMode::Indirect_X),
        OpCode::new(0x91, "STA", 2, 4, AddressingMode::Indirect_Y),
        // Adds one to the X register setting the zero and negative flags as appropriate.
        OpCode::new(0xaa, "TAX", 1, 2, AddressingMode::NoneAddressing),
        // Copies the current contents of the accumulator into the Y register
        // and sets the zero and negative flags as appropriate.
        OpCode::new(0xa8, "TAY", 1, 2, AddressingMode::NoneAddressing),
        // Loads a byte of memory into the accumulator
        // setting the zero and negative flags as appropriate
        OpCode::new(0xa9, "LDA", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xa5, "LDA", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xb5, "LDA", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0xad, "LDA", 3, 4, AddressingMode::Absolute),
        OpCode::new(0xbd, "LDA", 3, 4, AddressingMode::Absolute_X),
        OpCode::new(0xb9, "LDA", 3, 4, AddressingMode::Absolute_Y),
        OpCode::new(0xa1, "LDA", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(0xb1, "LDA", 2, 5, AddressingMode::Indirect_Y),
    ];
}

pub struct CPU {
    pub register_a: u8,
    pub status: u8,
    pub program_counter: u16,
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

    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a = value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    fn sta(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        self.mem_write(addr, self.register_a);
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

    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn tay(&mut self) {
        self.register_y = self.register_a;
        self.update_zero_and_negative_flags(self.register_y);
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

    fn branch(&mut self, set: bool, status: u8) {
        if (self.status & status) >= set as u8 {
            let offset = self.mem_read(self.program_counter) as i8;
            let counter = self.program_counter as i32 + offset as i32;
            self.program_counter = counter as u16;
        }
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
                    "INX" => {
                        self.inx();
                        self.program_counter += op.bytes as u16;
                    }
                    "INY" => {
                        self.iny();
                        self.program_counter += op.bytes as u16;
                    }
                    "LDA" => {
                        self.program_counter += 1;
                        self.lda(&op.mode);
                        self.program_counter += (op.bytes - 1) as u16;
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
                    "TAX" => {
                        self.tax();
                        self.program_counter += op.bytes as u16;
                    }
                    "TAY" => {
                        self.tay();
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
    fn test_5_ops_working_together() {
        let mut cpu = CPU::new();
        cpu.load_and_run(vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 0xc1)
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
    fn test_lda_from_memory() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0x55);

        cpu.load_and_run(vec![0xa5, 0x10, 0x00]);

        assert_eq!(cpu.register_a, 0x55);
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
}
