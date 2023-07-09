pub enum FlagType {
    CARRY,
    ZERO,
    INTERRUPT,
    DECIMAL,
    B_FLAG,
    BIT_5,
    NEGATIVE,
    OVERFLOW,
}

pub fn set_flag(status_flag: &mut u8, flag_type: FlagType) {
    match flag_type {
        FlagType::CARRY => set_carry_flag(status_flag),
        FlagType::ZERO => set_zero_flag(status_flag),
        FlagType::INTERRUPT => set_interrupt_flag(status_flag),
        FlagType::DECIMAL => set_decimal_flag(status_flag),
        FlagType::B_FLAG => set_b_flag(status_flag),
        FlagType::BIT_5 => set_bit5_flag(status_flag),
        FlagType::NEGATIVE => set_negative_flag(status_flag),
        FlagType::OVERFLOW => set_overflow_flag(status_flag),
    }
}

pub fn clear_flag(status_flag: &mut u8, flag_type: FlagType) {
    match flag_type {
        FlagType::CARRY => clear_carry_flag(status_flag),
        FlagType::ZERO => clear_zero_flag(status_flag),
        FlagType::INTERRUPT => clear_interrupt_flag(status_flag),
        FlagType::DECIMAL => clear_decimal_flag(status_flag),
        FlagType::B_FLAG => clear_b_flag(status_flag),
        FlagType::BIT_5 => clear_bit5_flag(status_flag),
        FlagType::NEGATIVE => clear_negative_flag(status_flag),
        FlagType::OVERFLOW => clear_overflow_flag(status_flag),
    }
}

pub fn flag_enabled(status_flag: u8, flag_type: FlagType) -> bool {
    match flag_type {
        FlagType::CARRY => carry_flag(status_flag),
        FlagType::ZERO => zero_flag(status_flag),
        FlagType::INTERRUPT => interrupt_flag(status_flag),
        FlagType::DECIMAL => decimal_flag(status_flag),
        FlagType::B_FLAG => b_flag(status_flag),
        FlagType::BIT_5 => bit5_flag(status_flag),
        FlagType::NEGATIVE => negative_flag(status_flag),
        FlagType::OVERFLOW => overflow_flag(status_flag),
    }
}

fn carry_flag(status_flag: u8) -> bool {
    return (status_flag & 0b0000_0001) == 0b0000_0001;
}

fn set_carry_flag(status_flag: &mut u8) {
    *status_flag = *status_flag | 0b0000_0001;
}

fn clear_carry_flag(status_flag: &mut u8) {
    *status_flag = *status_flag & 0b1111_1110;
}

fn zero_flag(status_flag: u8) -> bool {
    return (status_flag & 0b0000_0010) == 0b0000_0010;
}

fn set_zero_flag(status_flag: &mut u8) {
    *status_flag = *status_flag | 0b0000_0010;
}

fn clear_zero_flag(status_flag: &mut u8) {
    *status_flag = *status_flag & 0b1111_1101;
}

fn interrupt_flag(status_flag: u8) -> bool {
    return (status_flag & 0b0000_0100) == 0b0000_0100;
}

fn set_interrupt_flag(status_flag: &mut u8) {
    *status_flag = *status_flag | 0b0000_0100;
}

fn clear_interrupt_flag(status_flag: &mut u8) {
    *status_flag = *status_flag & 0b1111_1011;
}

fn decimal_flag(status_flag: u8) -> bool {
    return (status_flag & 0b0000_1000) == 0b0000_1000;
}

fn set_decimal_flag(status_flag: &mut u8) {
    *status_flag = *status_flag | 0b0000_1000;
}

fn clear_decimal_flag(status_flag: &mut u8) {
    *status_flag = *status_flag & 0b1111_0111;
}

fn b_flag(status_flag: u8) -> bool {
    return (status_flag & 0b0001_0000) == 0b0001_0000;
}

fn set_b_flag(status_flag: &mut u8) {
    *status_flag = *status_flag | 0b0001_0000;
}

fn clear_b_flag(status_flag: &mut u8) {
    *status_flag = *status_flag & 0b1110_1111;
}

fn bit5_flag(status_flag: u8) -> bool {
    return (status_flag & 0b0010_0000) == 0b0010_0000;
}

fn set_bit5_flag(status_flag: &mut u8) {
    *status_flag = *status_flag | 0b0010_0000;
}

fn clear_bit5_flag(status_flag: &mut u8) {
    *status_flag = *status_flag & 0b1101_1111;
}

fn negative_flag(status_flag: u8) -> bool {
    return (status_flag & 0b1000_0000) == 0b1000_0000;
}

fn set_negative_flag(status_flag: &mut u8) {
    *status_flag = *status_flag | 0b1000_0000;
}

fn clear_negative_flag(status_flag: &mut u8) {
    *status_flag = *status_flag & 0b0111_1111;
}

fn overflow_flag(status_flag: u8) -> bool {
    return (status_flag & 0b0100_0000) == 0b0100_0000;
}

fn set_overflow_flag(status_flag: &mut u8) {
    *status_flag = *status_flag | 0b0100_0000;
}

fn clear_overflow_flag(status_flag: &mut u8) {
    *status_flag = *status_flag & 0b1011_1111;
}
