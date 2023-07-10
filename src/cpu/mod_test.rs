mod test {
    use super::super::*;

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
    fn test_ldy_from_memory() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0x55);

        cpu.load_and_run(vec![0xa4, 0x10, 0x00]);

        assert_eq!(cpu.register_y, 0x55);
    }

    #[test]
    fn test_lsr_from_memory() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0xff);

        cpu.load_and_run(vec![0x46, 0x10, 0x00]);

        assert_eq!(cpu.mem_read(0x10), 0x7f);
        assert_eq!(cpu.status, 0x01);
    }

    #[test]
    fn test_lsr_from_reg_a() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x4a, 0x00]);
        cpu.reset();
        cpu.register_a = 0xff;
        cpu.run();

        assert_eq!(cpu.register_a, 0x7f);
        assert_eq!(cpu.status, 0x01);
    }

    #[test]
    fn test_ora_immediate() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x09, 0x05, 0x00]);
        cpu.reset();

        cpu.register_a = 0x09;
        cpu.run();

        assert_eq!(cpu.status, 0x00);
        assert_eq!(cpu.register_a, 0x0d);
    }

    #[test]
    fn test_ora_zero_page() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x05, 0x10, 0x00]);
        cpu.reset();

        cpu.mem_write(0x10, 0x05);
        cpu.register_a = 0x09;
        cpu.run();

        assert_eq!(cpu.status, 0x00);
        assert_eq!(cpu.register_a, 0x0d);
    }

    #[test]
    fn test_pha() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x48, 0x00]);
        cpu.reset();

        cpu.register_a = 0x09;
        cpu.run();

        assert_eq!(cpu.stack_pointer, 0xfe);
        assert_eq!(cpu.pop_stack(), 0x09);
        assert_eq!(cpu.register_a, 0x09);
    }

    #[test]
    fn test_php() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x08, 0x00]);
        cpu.reset();

        let status: u8 = 0b1100_1010;
        cpu.status = status;
        cpu.run();

        assert_eq!(cpu.stack_pointer, 0xfe);
        assert!(utils::flag_enabled(cpu.status, utils::FlagType::B_FLAG));
        assert!(utils::flag_enabled(cpu.status, utils::FlagType::BIT_5));
        assert_eq!(cpu.pop_stack(), status);
    }

    #[test]
    fn test_pla() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x68, 0x00]);
        cpu.reset();

        cpu.push_stack(0x09);
        cpu.run();

        assert_eq!(cpu.stack_pointer, 0xff);
        assert_eq!(cpu.register_a, 0x09);
    }

    #[test]
    fn test_plp() {
        let mut cpu = CPU::new();
        cpu.load(vec![0x28, 0x00]);
        cpu.reset();

        let status: u8 = 0b1100_1010;
        cpu.push_stack(status);
        cpu.run();

        assert_eq!(cpu.stack_pointer, 0xff);
        assert_eq!(cpu.status, status);
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
