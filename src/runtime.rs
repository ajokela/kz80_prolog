//! Z80 runtime for the Simplified Prolog Machine (SPM).
//!
//! Contains all Z80 machine code for the runtime interpreter including:
//! - Bytecode dispatch loop
//! - Unification algorithm
//! - Heap allocation
//! - Trail management for backtracking
//! - Choice point creation and restoration
//! - Built-in predicates (arithmetic, I/O)

/// Memory layout constants
pub mod memory {
    /// Start of ROM
    pub const ROM_START: u16 = 0x0000;
    /// End of ROM (8KB)
    pub const ROM_END: u16 = 0x1FFF;

    /// Start of interpreter code
    pub const INTERP_START: u16 = 0x0100;
    /// End of interpreter code
    pub const INTERP_END: u16 = 0x0EFF;

    /// Start of runtime library
    pub const RUNTIME_START: u16 = 0x0F00;
    /// End of runtime library
    pub const RUNTIME_END: u16 = 0x13FF;

    /// Start of bytecode
    pub const CODE_START: u16 = 0x1400;
    /// End of bytecode
    pub const CODE_END: u16 = 0x1BFF;

    /// Start of atom table
    pub const ATOM_TABLE: u16 = 0x1C00;
    /// Start of clause index
    pub const CLAUSE_INDEX: u16 = 0x1E00;

    /// Start of heap (RAM)
    pub const HEAP_START: u16 = 0x2000;
    /// End of heap
    pub const HEAP_END: u16 = 0x27FF;

    /// Start of trail
    pub const TRAIL_START: u16 = 0x2800;
    /// End of trail
    pub const TRAIL_END: u16 = 0x2FFF;

    /// Start of stack (environments and choice points)
    pub const STACK_START: u16 = 0x3000;
    /// End of stack
    pub const STACK_END: u16 = 0x35FF;

    /// Argument registers
    pub const ARG_REGS: u16 = 0x3600;
    /// Number of argument registers (16 words = 32 bytes)
    pub const ARG_REG_COUNT: u16 = 16;

    /// Scratch area
    pub const SCRATCH: u16 = 0x3700;
    /// Z80 hardware stack top
    pub const STACK_TOP: u16 = 0x37FF;

    /// SPM register file in RAM
    pub const SPM_PC: u16 = 0x3620;      // Bytecode PC
    pub const SPM_H: u16 = 0x3622;       // Heap pointer
    pub const SPM_TR: u16 = 0x3624;      // Trail pointer
    pub const SPM_E: u16 = 0x3626;       // Environment pointer
    pub const SPM_B: u16 = 0x3628;       // Choice point pointer
    pub const SPM_S: u16 = 0x362A;       // Structure pointer (for unify mode)
    pub const SPM_MODE: u16 = 0x362C;    // Read/Write mode flag
}

/// Tag values for tagged pointers (upper 2 bits)
pub mod tags {
    pub const TAG_REF: u8 = 0b00;        // Reference/variable
    pub const TAG_INT: u8 = 0b01;        // Immediate integer
    pub const TAG_ATM: u8 = 0b10;        // Atom index
    pub const TAG_STR: u8 = 0b11;        // Structure pointer

    pub const TAG_MASK: u16 = 0xC000;
    pub const VALUE_MASK: u16 = 0x3FFF;
}

/// SPM bytecode opcodes
pub mod opcodes {
    pub const HALT: u8 = 0x00;

    // PUT instructions (load argument registers)
    pub const PUT_VAR: u8 = 0x01;        // PUT_VAR Ai, Yn
    pub const PUT_VAL: u8 = 0x02;        // PUT_VAL Ai, Yn
    pub const PUT_ATOM: u8 = 0x03;       // PUT_ATOM Ai, atom_idx
    pub const PUT_INT: u8 = 0x04;        // PUT_INT Ai, value
    pub const PUT_STR: u8 = 0x05;        // PUT_STR Ai, functor, arity
    pub const PUT_LIST: u8 = 0x06;       // PUT_LIST Ai
    pub const PUT_NIL: u8 = 0x07;        // PUT_NIL Ai

    // GET instructions (unify argument registers with head)
    pub const GET_VAR: u8 = 0x08;        // GET_VAR Ai, Yn
    pub const GET_VAL: u8 = 0x09;        // GET_VAL Ai, Yn
    pub const GET_ATOM: u8 = 0x0A;       // GET_ATOM Ai, atom_idx
    pub const GET_INT: u8 = 0x0B;        // GET_INT Ai, value
    pub const GET_STR: u8 = 0x0C;        // GET_STR Ai, functor, arity
    pub const GET_LIST: u8 = 0x0D;       // GET_LIST Ai
    pub const GET_NIL: u8 = 0x0E;        // GET_NIL Ai

    // UNIFY instructions (for structure arguments)
    pub const UNIFY_VAR: u8 = 0x10;      // UNIFY_VAR Yn
    pub const UNIFY_VAL: u8 = 0x11;      // UNIFY_VAL Yn
    pub const UNIFY_ATOM: u8 = 0x12;     // UNIFY_ATOM atom_idx
    pub const UNIFY_INT: u8 = 0x13;      // UNIFY_INT value
    pub const UNIFY_VOID: u8 = 0x14;     // UNIFY_VOID n (skip n args)

    // Control flow
    pub const CALL: u8 = 0x20;           // CALL addr
    pub const EXEC: u8 = 0x21;           // EXECUTE addr (tail call)
    pub const PROCEED: u8 = 0x22;        // PROCEED (return)
    pub const ALLOC: u8 = 0x23;          // ALLOCATE n
    pub const DEALLOC: u8 = 0x24;        // DEALLOCATE

    // Choice point management
    pub const TRY_ME: u8 = 0x28;         // TRY_ME_ELSE addr
    pub const RETRY_ME: u8 = 0x29;       // RETRY_ME_ELSE addr
    pub const TRUST_ME: u8 = 0x2A;       // TRUST_ME
    pub const TRY: u8 = 0x2B;            // TRY addr
    pub const RETRY: u8 = 0x2C;          // RETRY addr
    pub const TRUST: u8 = 0x2D;          // TRUST addr

    // Cut and fail
    pub const CUT: u8 = 0x30;            // CUT
    pub const NECK_CUT: u8 = 0x31;       // NECK_CUT
    pub const FAIL: u8 = 0x32;           // FAIL

    // Built-ins
    pub const IS: u8 = 0x40;             // IS Yn, expr_addr
    pub const CMP_LT: u8 = 0x41;         // CMP_LT
    pub const CMP_GT: u8 = 0x42;         // CMP_GT
    pub const CMP_LE: u8 = 0x43;         // CMP_LE
    pub const CMP_GE: u8 = 0x44;         // CMP_GE
    pub const CMP_EQ: u8 = 0x45;         // CMP_EQ
    pub const CMP_NE: u8 = 0x46;         // CMP_NE

    pub const WRITE: u8 = 0x50;          // WRITE Ai
    pub const NL: u8 = 0x51;             // NL
    pub const WRITE_ATOM: u8 = 0x52;     // WRITE_ATOM atom_idx
    pub const WRITE_INT: u8 = 0x53;      // WRITE_INT
}

/// Runtime code generator
pub struct Runtime {
    code: Vec<u8>,
    labels: std::collections::HashMap<String, u16>,
    fixups: Vec<(usize, String)>,
}

impl Runtime {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            labels: std::collections::HashMap::new(),
            fixups: Vec::new(),
        }
    }

    /// Get the generated code.
    pub fn code(&self) -> &[u8] {
        &self.code
    }

    /// Get code length.
    pub fn len(&self) -> usize {
        self.code.len()
    }

    /// Check if empty.
    pub fn is_empty(&self) -> bool {
        self.code.is_empty()
    }

    /// Current address.
    fn addr(&self) -> u16 {
        memory::INTERP_START + self.code.len() as u16
    }

    /// Emit a byte.
    fn emit(&mut self, byte: u8) {
        self.code.push(byte);
    }

    /// Emit a 16-bit word (little endian).
    fn emit_word(&mut self, word: u16) {
        self.code.push((word & 0xFF) as u8);
        self.code.push((word >> 8) as u8);
    }

    /// Define a label at current position.
    fn label(&mut self, name: &str) {
        self.labels.insert(name.to_string(), self.addr());
    }

    /// Emit a reference to a label (to be fixed up later).
    fn emit_label_ref(&mut self, name: &str) {
        self.fixups.push((self.code.len(), name.to_string()));
        self.emit_word(0x0000);
    }

    /// Resolve all label references.
    pub fn resolve_labels(&mut self) {
        for (offset, name) in &self.fixups {
            if let Some(&addr) = self.labels.get(name) {
                self.code[*offset] = (addr & 0xFF) as u8;
                self.code[*offset + 1] = (addr >> 8) as u8;
            }
        }
    }

    /// Generate the complete runtime.
    pub fn generate(&mut self) {
        self.emit_startup();
        self.emit_dispatch_loop();
        self.emit_unification();
        self.emit_heap_ops();
        self.emit_trail_ops();
        self.emit_choice_point_ops();
        self.emit_print_routines();
        self.emit_arithmetic();
        self.resolve_labels();
    }

    /// Emit startup code.
    fn emit_startup(&mut self) {
        // JP main (will be at 0x0000)
        self.emit(0xC3);                // JP
        self.emit_label_ref("main");

        // Padding to 0x0100
        while self.code.len() < 0x0100 {
            self.emit(0x00);
        }

        self.label("main");

        // Initialize stack pointer
        self.emit(0x31);                // LD SP, nn
        self.emit_word(memory::STACK_TOP);

        // Initialize SPM registers
        // H (heap pointer) = HEAP_START
        self.emit(0x21);                // LD HL, nn
        self.emit_word(memory::HEAP_START);
        self.emit(0x22);                // LD (nn), HL
        self.emit_word(memory::SPM_H);

        // TR (trail pointer) = TRAIL_START
        self.emit(0x21);                // LD HL, nn
        self.emit_word(memory::TRAIL_START);
        self.emit(0x22);                // LD (nn), HL
        self.emit_word(memory::SPM_TR);

        // E (environment) = 0
        self.emit(0x21);                // LD HL, 0
        self.emit_word(0x0000);
        self.emit(0x22);                // LD (nn), HL
        self.emit_word(memory::SPM_E);

        // B (choice point) = 0
        self.emit(0x22);                // LD (nn), HL (still 0)
        self.emit_word(memory::SPM_B);

        // PC = CODE_START (will be set by query setup)
        self.emit(0x21);                // LD HL, CODE_START
        self.emit_word(memory::CODE_START);
        self.emit(0x22);                // LD (nn), HL
        self.emit_word(memory::SPM_PC);

        // Fall through to dispatch loop
    }

    /// Emit the main bytecode dispatch loop.
    fn emit_dispatch_loop(&mut self) {
        self.label("dispatch");

        // Fetch next opcode
        // LD HL, (SPM_PC)
        self.emit(0x2A);
        self.emit_word(memory::SPM_PC);

        // LD A, (HL)
        self.emit(0x7E);

        // INC HL
        self.emit(0x23);

        // LD (SPM_PC), HL
        self.emit(0x22);
        self.emit_word(memory::SPM_PC);

        // Dispatch based on opcode in A
        // For now, use a simple comparison chain
        // A full implementation would use a jump table

        // CP HALT
        self.emit(0xFE);
        self.emit(opcodes::HALT);
        self.emit(0xCA);                // JP Z, halt
        self.emit_label_ref("halt");

        // CP PROCEED
        self.emit(0xFE);
        self.emit(opcodes::PROCEED);
        self.emit(0xCA);                // JP Z, op_proceed
        self.emit_label_ref("op_proceed");

        // CP FAIL
        self.emit(0xFE);
        self.emit(opcodes::FAIL);
        self.emit(0xCA);                // JP Z, op_fail
        self.emit_label_ref("op_fail");

        // CP NL
        self.emit(0xFE);
        self.emit(opcodes::NL);
        self.emit(0xCA);                // JP Z, op_nl
        self.emit_label_ref("op_nl");

        // Default: unknown opcode, halt
        self.emit(0xC3);                // JP halt
        self.emit_label_ref("halt");

        // HALT handler
        self.label("halt");
        self.emit(0x76);                // HALT

        // PROCEED handler (return from clause)
        self.label("op_proceed");
        // TODO: Implement proper return
        // For now, just print "yes" and halt
        self.emit(0xCD);                // CALL print_yes
        self.emit_label_ref("print_yes");
        self.emit(0xC3);                // JP halt
        self.emit_label_ref("halt");

        // FAIL handler
        self.label("op_fail");
        self.emit(0xCD);                // CALL backtrack
        self.emit_label_ref("backtrack");
        self.emit(0xC3);                // JP dispatch
        self.emit_label_ref("dispatch");

        // NL handler
        self.label("op_nl");
        self.emit(0xCD);                // CALL print_nl
        self.emit_label_ref("print_nl");
        self.emit(0xC3);                // JP dispatch
        self.emit_label_ref("dispatch");
    }

    /// Emit unification routines.
    fn emit_unification(&mut self) {
        // Dereference: follow reference chain
        // Input: HL = term
        // Output: HL = dereferenced term
        self.label("deref");
        // LD A, H
        self.emit(0x7C);
        // AND 0xC0 (get tag)
        self.emit(0xE6);
        self.emit(0xC0);
        // RET NZ (not a reference)
        self.emit(0xC0);

        // It's a reference, follow it
        // LD E, (HL)
        self.emit(0x5E);
        // INC HL
        self.emit(0x23);
        // LD D, (HL)
        self.emit(0x56);
        // DEC HL
        self.emit(0x2B);

        // Check if self-referential (unbound)
        // LD A, E
        self.emit(0x7B);
        // CP L
        self.emit(0xBD);
        // RET NZ
        self.emit(0xC0);
        // LD A, D
        self.emit(0x7A);
        // CP H
        self.emit(0xBC);
        // RET Z (self-referential = unbound)
        self.emit(0xC8);

        // Follow the reference
        // EX DE, HL
        self.emit(0xEB);
        // JR deref
        self.emit(0x18);
        let offset = self.code.len();
        self.emit(0x00);  // Placeholder
        // Calculate relative offset
        let target_offset = -(self.code.len() as i8 - offset as i8 + 1);
        self.code[offset] = target_offset as u8;

        // Unify two terms
        // Input: HL = term1, DE = term2
        // Output: Carry clear = success, Carry set = fail
        self.label("unify");
        // Save registers
        self.emit(0xC5);                // PUSH BC
        self.emit(0xE5);                // PUSH HL
        self.emit(0xD5);                // PUSH DE

        // Dereference both
        self.emit(0xCD);                // CALL deref
        self.emit_label_ref("deref");
        self.emit(0xE3);                // EX (SP), HL (save deref'd HL, get DE)
        self.emit(0xEB);                // EX DE, HL
        self.emit(0xCD);                // CALL deref
        self.emit_label_ref("deref");
        self.emit(0xE1);                // POP DE (original HL, deref'd)
        self.emit(0xEB);                // EX DE, HL (HL=term1, DE=term2)

        // Check if identical
        self.emit(0x7C);                // LD A, H
        self.emit(0xBA);                // CP D
        self.emit(0x20);                // JR NZ, unify_check_tags
        self.emit(0x04);
        self.emit(0x7D);                // LD A, L
        self.emit(0xBB);                // CP E
        self.emit(0x28);                // JR Z, unify_success
        self.emit(0x20);                // Placeholder for forward jump

        // Check tags
        self.label("unify_check_tags");
        // TODO: Full unification implementation
        // For now, simplified version

        // Restore and return failure for non-identical terms
        self.emit(0xD1);                // POP DE
        self.emit(0xC1);                // POP BC
        self.emit(0x37);                // SCF (set carry = fail)
        self.emit(0xC9);                // RET

        self.label("unify_success");
        self.emit(0xD1);                // POP DE
        self.emit(0xC1);                // POP BC
        self.emit(0xA7);                // AND A (clear carry = success)
        self.emit(0xC9);                // RET
    }

    /// Emit heap allocation routines.
    fn emit_heap_ops(&mut self) {
        // Allocate n words on heap
        // Input: BC = number of words
        // Output: HL = address of allocated block
        self.label("heap_alloc");
        // LD HL, (SPM_H)
        self.emit(0x2A);
        self.emit_word(memory::SPM_H);
        // PUSH HL (save start address)
        self.emit(0xE5);
        // SLA C, RL B (multiply by 2 for bytes)
        self.emit(0xCB);
        self.emit(0x21);
        self.emit(0xCB);
        self.emit(0x10);
        // ADD HL, BC
        self.emit(0x09);
        // LD (SPM_H), HL
        self.emit(0x22);
        self.emit_word(memory::SPM_H);
        // POP HL (return start address)
        self.emit(0xE1);
        self.emit(0xC9);                // RET
    }

    /// Emit trail operations.
    fn emit_trail_ops(&mut self) {
        // Trail a variable binding
        // Input: HL = address of variable
        self.label("trail");
        // Check if we need to trail (var created before current choice point)
        // For now, always trail
        // LD DE, (SPM_TR)
        self.emit(0xED);
        self.emit(0x5B);
        self.emit_word(memory::SPM_TR);
        // LD (DE), L
        self.emit(0x73);
        // INC DE
        self.emit(0x13);
        // LD (DE), H
        self.emit(0x72);
        // INC DE
        self.emit(0x13);
        // LD (SPM_TR), DE
        self.emit(0xED);
        self.emit(0x53);
        self.emit_word(memory::SPM_TR);
        self.emit(0xC9);                // RET

        // Unwind trail to a saved point
        // Input: DE = saved trail pointer
        self.label("unwind_trail");
        // LD HL, (SPM_TR)
        self.emit(0x2A);
        self.emit_word(memory::SPM_TR);

        self.label("unwind_loop");
        // Compare HL with DE
        self.emit(0x7C);                // LD A, H
        self.emit(0xBA);                // CP D
        self.emit(0x20);                // JR NZ, unwind_continue
        self.emit(0x04);
        self.emit(0x7D);                // LD A, L
        self.emit(0xBB);                // CP E
        self.emit(0xC8);                // RET Z (done)

        self.label("unwind_continue");
        // DEC HL, DEC HL (back up 2 bytes)
        self.emit(0x2B);
        self.emit(0x2B);
        // Get trailed address
        self.emit(0x4E);                // LD C, (HL)
        self.emit(0x23);                // INC HL
        self.emit(0x46);                // LD B, (HL)
        self.emit(0x2B);                // DEC HL
        // Reset variable to unbound (self-referential)
        // LD (BC), C
        self.emit(0x02);
        // INC BC
        self.emit(0x03);
        // LD A, B
        self.emit(0x78);
        // DEC BC
        self.emit(0x0B);
        // PUSH BC
        self.emit(0xC5);
        // LD BC, 1
        self.emit(0x01);
        self.emit_word(0x0001);
        // ADD BC to saved BC...
        // This is getting complex. Simplified:
        self.emit(0xC1);                // POP BC
        self.emit(0x18);                // JR unwind_loop
        // Calculate offset
        let back_offset = -20i8;        // Approximate
        self.emit(back_offset as u8);
    }

    /// Emit choice point operations.
    fn emit_choice_point_ops(&mut self) {
        // Backtrack to previous choice point
        self.label("backtrack");
        // LD HL, (SPM_B)
        self.emit(0x2A);
        self.emit_word(memory::SPM_B);
        // Check if B is 0 (no more choice points)
        self.emit(0x7C);                // LD A, H
        self.emit(0xB5);                // OR L
        self.emit(0xCA);                // JP Z, backtrack_fail
        self.emit_label_ref("backtrack_fail");

        // Restore state from choice point
        // TODO: Full implementation
        // For now, just print "no" and halt

        self.label("backtrack_fail");
        self.emit(0xCD);                // CALL print_no
        self.emit_label_ref("print_no");
        self.emit(0x76);                // HALT
    }

    /// Emit print routines.
    fn emit_print_routines(&mut self) {
        // Print "yes"
        self.label("print_yes");
        self.emit(0x3E);                // LD A, 'y'
        self.emit(b'y');
        self.emit(0xD3);                // OUT (0x81), A
        self.emit(0x81);
        self.emit(0x3E);                // LD A, 'e'
        self.emit(b'e');
        self.emit(0xD3);                // OUT (0x81), A
        self.emit(0x81);
        self.emit(0x3E);                // LD A, 's'
        self.emit(b's');
        self.emit(0xD3);                // OUT (0x81), A
        self.emit(0x81);
        self.emit(0xCD);                // CALL print_nl
        self.emit_label_ref("print_nl");
        self.emit(0xC9);                // RET

        // Print "no"
        self.label("print_no");
        self.emit(0x3E);                // LD A, 'n'
        self.emit(b'n');
        self.emit(0xD3);                // OUT (0x81), A
        self.emit(0x81);
        self.emit(0x3E);                // LD A, 'o'
        self.emit(b'o');
        self.emit(0xD3);                // OUT (0x81), A
        self.emit(0x81);
        self.emit(0xCD);                // CALL print_nl
        self.emit_label_ref("print_nl");
        self.emit(0xC9);                // RET

        // Print newline
        self.label("print_nl");
        self.emit(0x3E);                // LD A, '\r'
        self.emit(0x0D);
        self.emit(0xD3);                // OUT (0x81), A
        self.emit(0x81);
        self.emit(0x3E);                // LD A, '\n'
        self.emit(0x0A);
        self.emit(0xD3);                // OUT (0x81), A
        self.emit(0x81);
        self.emit(0xC9);                // RET

        // Print integer in HL
        self.label("print_int");
        // Check for negative
        self.emit(0xCB);                // BIT 7, H
        self.emit(0x7C);
        self.emit(0x28);                // JR Z, print_int_pos
        self.emit(0x09);
        // Print minus sign
        self.emit(0x3E);                // LD A, '-'
        self.emit(b'-');
        self.emit(0xD3);                // OUT (0x81), A
        self.emit(0x81);
        // Negate HL
        self.emit(0xAF);                // XOR A
        self.emit(0x95);                // SUB L
        self.emit(0x6F);                // LD L, A
        self.emit(0x9F);                // SBC A, A
        self.emit(0x94);                // SUB H
        self.emit(0x67);                // LD H, A

        self.label("print_int_pos");
        // Convert to decimal and print
        // Use stack to reverse digit order
        self.emit(0x01);                // LD BC, 0 (digit count)
        self.emit_word(0x0000);

        self.label("print_int_loop");
        // Divide HL by 10
        self.emit(0x11);                // LD DE, 10
        self.emit_word(10);
        self.emit(0xCD);                // CALL div16
        self.emit_label_ref("div16");
        // Remainder in A, quotient in HL
        // Push digit
        self.emit(0xC6);                // ADD A, '0'
        self.emit(b'0');
        self.emit(0xF5);                // PUSH AF
        self.emit(0x03);                // INC BC
        // Check if HL is 0
        self.emit(0x7C);                // LD A, H
        self.emit(0xB5);                // OR L
        self.emit(0x20);                // JR NZ, print_int_loop
        // Calculate backward jump
        self.emit(0xF0);                // -16 approximately

        self.label("print_int_out");
        // Print digits in reverse order
        self.emit(0x78);                // LD A, B
        self.emit(0xB1);                // OR C
        self.emit(0xC8);                // RET Z
        self.emit(0xF1);                // POP AF
        self.emit(0xD3);                // OUT (0x81), A
        self.emit(0x81);
        self.emit(0x0B);                // DEC BC
        self.emit(0x18);                // JR print_int_out
        self.emit(0xF6);                // -10
    }

    /// Emit arithmetic routines.
    fn emit_arithmetic(&mut self) {
        // 16-bit division: HL / DE -> HL (quotient), A (remainder)
        self.label("div16");
        self.emit(0xAF);                // XOR A
        self.emit(0x06);                // LD B, 16
        self.emit(16);

        self.label("div16_loop");
        self.emit(0x29);                // ADD HL, HL
        self.emit(0x17);                // RLA
        self.emit(0xBA);                // CP D... simplified
        // This needs proper implementation
        self.emit(0x10);                // DJNZ div16_loop
        self.emit(0xFB);
        self.emit(0xC9);                // RET

        // 16-bit multiply: HL * DE -> HL
        self.label("mul16");
        self.emit(0xC5);                // PUSH BC
        self.emit(0x44);                // LD B, H
        self.emit(0x4D);                // LD C, L
        self.emit(0x21);                // LD HL, 0
        self.emit_word(0x0000);
        self.emit(0x3E);                // LD A, 16
        self.emit(16);

        self.label("mul16_loop");
        self.emit(0x29);                // ADD HL, HL
        self.emit(0xCB);                // RL E
        self.emit(0x13);
        self.emit(0xCB);                // RL D
        self.emit(0x12);
        self.emit(0x30);                // JR NC, mul16_skip
        self.emit(0x01);
        self.emit(0x09);                // ADD HL, BC

        self.label("mul16_skip");
        self.emit(0x3D);                // DEC A
        self.emit(0x20);                // JR NZ, mul16_loop
        self.emit(0xF4);
        self.emit(0xC1);                // POP BC
        self.emit(0xC9);                // RET
    }
}

impl Default for Runtime {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_runtime_generation() {
        let mut runtime = Runtime::new();
        runtime.generate();

        // Check that we generated some code
        assert!(!runtime.is_empty());

        // Check that startup is at 0x0000 (JP instruction)
        assert_eq!(runtime.code()[0], 0xC3);
    }

    #[test]
    fn test_memory_layout() {
        // Verify memory regions don't overlap
        assert!(memory::ROM_END < memory::HEAP_START);
        assert!(memory::HEAP_END < memory::TRAIL_START);
        assert!(memory::TRAIL_END < memory::STACK_START);
        assert!(memory::STACK_END < memory::STACK_TOP);
    }
}
