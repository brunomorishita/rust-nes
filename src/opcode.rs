use crate::cpu::AddressingMode;
use lazy_static::lazy_static;

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
        // This instruction adds the contents of a memory location to the accumulator
        // together with the carry bit. If overflow occurs the carry bit is set,
        // this enables multiple byte addition to be performed.
        OpCode::new(0x69, "ADC", 2, 2, AddressingMode::Immediate),
        OpCode::new(0x65, "ADC", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x75, "ADC", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0x6d, "ADC", 3, 4, AddressingMode::Absolute),
        OpCode::new(0x7d, "ADC", 3, 4, AddressingMode::Absolute_X),
        OpCode::new(0x79, "ADC", 3, 4, AddressingMode::Absolute_Y),
        OpCode::new(0x61, "ADC", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(0x71, "ADC", 2, 5, AddressingMode::Indirect_Y),
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
        // This instructions is used to test if one or more bits are set
        // in a target memory location. The mask pattern in A is ANDed with
        // the value in memory to set or clear the zero flag, but the result
        // is not kept. Bits 7 and 6 of the value from memory are copied into
        // the N and V flags.
        OpCode::new(0x24, "BIT", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x2c, "BIT", 3, 4, AddressingMode::Absolute),
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
        // This instruction compares the contents of the accumulator with another
        // memory held value and sets the zero and carry flags as appropriate.
        OpCode::new(0xc9, "CMP", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xc5, "CMP", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xd5, "CMP", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0xcd, "CMP", 3, 4, AddressingMode::Absolute),
        OpCode::new(0xdd, "CMP", 3, 4, AddressingMode::Absolute_X),
        OpCode::new(0xd9, "CMP", 3, 4, AddressingMode::Absolute_Y),
        OpCode::new(0xc1, "CMP", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(0xd1, "CMP", 2, 5, AddressingMode::Indirect_Y),
        // This instruction compares the contents of the X register with another memory
        // held value and sets the zero and carry flags as appropriate.
        OpCode::new(0xe0, "CPX", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xe4, "CPX", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xec, "CPX", 3, 4, AddressingMode::Absolute),
        // This instruction compares the contents of the Y register with another memory
        // held value and sets the zero and carry flags as appropriate.
        OpCode::new(0xc0, "CPY", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xc4, "CPY", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xcc, "CPY", 3, 4, AddressingMode::Absolute),
        // Subtracts one from the value held at a specified memory location setting
        // the zero and negative flags as appropriate.
        OpCode::new(0xc6, "DEC", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0xd6, "DEC", 2, 6, AddressingMode::ZeroPage_X),
        OpCode::new(0xce, "DEC", 3, 6, AddressingMode::Absolute),
        OpCode::new(0xde, "DEC", 3, 7, AddressingMode::Absolute_X),
        // Subtracts one from the X register setting the zero and negative flags as appropriate.
        OpCode::new(0xca, "DEX", 1, 2, AddressingMode::NoneAddressing),
        // Subtracts one from the Y register setting the zero and negative flags as appropriate.
        OpCode::new(0x88, "DEY", 1, 2, AddressingMode::NoneAddressing),
        // An exclusive OR is performed, bit by bit, on the accumulator contents using
        // the contents of a byte of memory.
        OpCode::new(0x49, "EOR", 2, 2, AddressingMode::Immediate),
        OpCode::new(0x45, "EOR", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x55, "EOR", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0x4d, "EOR", 3, 4, AddressingMode::Absolute),
        OpCode::new(0x5d, "EOR", 3, 4, AddressingMode::Absolute_X),
        OpCode::new(0x59, "EOR", 3, 4, AddressingMode::Absolute_Y),
        OpCode::new(0x41, "EOR", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(0x51, "EOR", 2, 5, AddressingMode::Indirect_Y),
        // Adds one to the value held at a specified memory location
        // setting the zero and negative flags as appropriate.
        OpCode::new(0xe6, "INC", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0xf6, "INC", 2, 6, AddressingMode::ZeroPage_X),
        OpCode::new(0xee, "INC", 3, 6, AddressingMode::Absolute),
        OpCode::new(0xfe, "INC", 3, 7, AddressingMode::Absolute_X),
        // Adds one to the X register setting the zero and negative flags as appropriate.
        OpCode::new(0xe8, "INX", 1, 2, AddressingMode::NoneAddressing),
        // Adds one to the Y register setting the zero and negative flags as appropriate.
        OpCode::new(0xc8, "INY", 1, 2, AddressingMode::NoneAddressing),
        // Sets the program counter to the address specified by the operand.
        OpCode::new(0x4c, "JMP", 3, 3, AddressingMode::Absolute),
        OpCode::new(0x6c, "JMP", 3, 5, AddressingMode::Indirect),
        // The JSR instruction pushes the address (minus one) of the return point on to the
        // stack and then sets the program counter to the target memory address.
        OpCode::new(0x20, "JSR", 3, 6, AddressingMode::Absolute),
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
        // Loads a byte of memory into the X register
        // setting the zero and negative flags as appropriate.
        OpCode::new(0xa2, "LDX", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xa6, "LDX", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xb6, "LDX", 2, 4, AddressingMode::ZeroPage_Y),
        OpCode::new(0xae, "LDX", 3, 4, AddressingMode::Absolute),
        OpCode::new(0xbe, "LDX", 3, 4, AddressingMode::Absolute_Y),
        // Loads a byte of memory into the Y register
        // setting the zero and negative flags as appropriate.
        OpCode::new(0xa0, "LDY", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xa4, "LDY", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xb4, "LDY", 2, 4, AddressingMode::ZeroPage_Y),
        OpCode::new(0xac, "LDY", 3, 4, AddressingMode::Absolute),
        OpCode::new(0xbc, "LDY", 3, 4, AddressingMode::Absolute_Y),
        // Each of the bits in A or M is shift one place to the right.
        // The bit that was in bit 0 is shifted into the carry flag. Bit 7 is set to zero.
        OpCode::new(0x4a, "LSR", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x46, "LSR", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0x56, "LSR", 2, 6, AddressingMode::ZeroPage_X),
        OpCode::new(0x4e, "LSR", 3, 6, AddressingMode::Absolute),
        OpCode::new(0x5e, "LSR", 3, 7, AddressingMode::Absolute_X),

        // The NOP instruction causes no changes to the processor other than
        // the normal incrementing of the program counter to the next instruction.
        OpCode::new(0xea, "NOP", 1, 2, AddressingMode::NoneAddressing),

        // An inclusive OR is performed, bit by bit, on the accumulator contents
        // using the contents of a byte of memory.
        OpCode::new(0x09, "ORA", 2, 2, AddressingMode::Immediate),
        OpCode::new(0x05, "ORA", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x15, "ORA", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0x0d, "ORA", 3, 4, AddressingMode::Absolute),
        OpCode::new(0x1d, "ORA", 3, 4, AddressingMode::Absolute_X),
        OpCode::new(0x19, "ORA", 3, 4, AddressingMode::Absolute_Y),
        OpCode::new(0x01, "ORA", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(0x11, "ORA", 2, 5, AddressingMode::Indirect_Y),

        // Pushes a copy of the accumulator on to the stack.
        OpCode::new(0x48, "PHA", 1, 3, AddressingMode::NoneAddressing),

        // Pushes a copy of the status flags on to the stack.
        OpCode::new(0x08, "PHP", 1, 3, AddressingMode::NoneAddressing),

        // Pulls an 8 bit value from the stack and into the accumulator.
        // The zero and negative flags are set as appropriate.
        OpCode::new(0x68, "PLA", 1, 3, AddressingMode::NoneAddressing),

        // Pulls an 8 bit value from the stack and into the processor flags.
        // The flags will take on new states as determined by the value pulled.
        OpCode::new(0x28, "PLP", 1, 3, AddressingMode::NoneAddressing),

        // Move each of the bits in either A or M one place to the left.
        // Bit 0 is filled with the current value of the carry flag whilst
        // the old bit 7 becomes the new carry flag value.
        OpCode::new(0x2a, "ROL", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x26, "ROL", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0x36, "ROL", 2, 6, AddressingMode::ZeroPage_X),
        OpCode::new(0x2e, "ROL", 3, 6, AddressingMode::Absolute),
        OpCode::new(0x3e, "ROL", 3, 7, AddressingMode::Absolute_X),

        // Move each of the bits in either A or M one place to the right.
        // Bit 7 is filled with the current value of the carry flag whilst
        // the old bit 0 becomes the new carry flag value.
        OpCode::new(0x6a, "ROR", 1, 2, AddressingMode::NoneAddressing),
        OpCode::new(0x66, "ROR", 2, 5, AddressingMode::ZeroPage),
        OpCode::new(0x76, "ROR", 2, 6, AddressingMode::ZeroPage_X),
        OpCode::new(0x6e, "ROR", 3, 6, AddressingMode::Absolute),
        OpCode::new(0x7e, "ROR", 3, 7, AddressingMode::Absolute_X),

        // The RTI instruction is used at the end of an interrupt processing routine.
        // It pulls the processor flags from the stack followed by the program counter.
        OpCode::new(0x40, "RTI", 1, 6, AddressingMode::NoneAddressing),

        // The RTS instruction is used at the end of a subroutine to return
        // to the calling routine. It pulls the program counter (minus one) from the stack.
        OpCode::new(0x60, "RTS", 1, 6, AddressingMode::NoneAddressing),

        // This instruction subtracts the contents of a memory location to the accumulator
        // together with the not of the carry bit. If overflow occurs the carry bit is clear,
        // this enables multiple byte subtraction to be performed.
        OpCode::new(0xe9, "SBC", 2, 2, AddressingMode::Immediate),
        OpCode::new(0xe5, "SBC", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0xf5, "SBC", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0xed, "SBC", 3, 4, AddressingMode::Absolute),
        OpCode::new(0xfd, "SBC", 3, 4, AddressingMode::Absolute_X),
        OpCode::new(0xf9, "SBC", 3, 4, AddressingMode::Absolute_Y),
        OpCode::new(0xe1, "SBC", 2, 6, AddressingMode::Indirect_X),
        OpCode::new(0xf1, "SBC", 2, 5, AddressingMode::Indirect_Y),

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
        // Stores the contents of the X register into memory.
        OpCode::new(0x86, "STX", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x96, "STX", 2, 4, AddressingMode::ZeroPage_Y),
        OpCode::new(0x8e, "STX", 3, 4, AddressingMode::Absolute),
        // Stores the contents of the Y register into memory.
        OpCode::new(0x84, "STY", 2, 3, AddressingMode::ZeroPage),
        OpCode::new(0x94, "STY", 2, 4, AddressingMode::ZeroPage_X),
        OpCode::new(0x8c, "STY", 3, 4, AddressingMode::Absolute),
        // Adds one to the X register setting the zero and negative flags as appropriate.
        OpCode::new(0xaa, "TAX", 1, 2, AddressingMode::NoneAddressing),
        // Copies the current contents of the accumulator into the Y register
        // and sets the zero and negative flags as appropriate.
        OpCode::new(0xa8, "TAY", 1, 2, AddressingMode::NoneAddressing),
        // Copies the current contents of the stack register into the X register
        // and sets the zero and negative flags as appropriate.
        OpCode::new(0xba, "TSX", 1, 2, AddressingMode::NoneAddressing),
        // Copies the current contents of the X register into the accumulator
        // and sets the zero and negative flags as appropriate.
        OpCode::new(0x8a, "TXA", 1, 2, AddressingMode::NoneAddressing),
        // Copies the current contents of the X register into the stack register.
        OpCode::new(0x9a, "TXS", 1, 2, AddressingMode::NoneAddressing),
        // Copies the current contents of the Y register into the accumulator
        // and sets the zero and negative flags as appropriate.
        OpCode::new(0x98, "TYA", 1, 2, AddressingMode::NoneAddressing),
    ];
}
