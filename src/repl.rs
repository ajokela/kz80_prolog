//! On-target REPL generator for Prolog on Z80
//!
//! Generates a standalone REPL binary that runs on the Z80,
//! including lexer, parser, unification, and backtracking.

/// Memory layout constants
const HEAP_START: u16 = 0x2000;      // Heap for terms (2KB)
const HEAP_END: u16 = 0x27FF;
const TRAIL_START: u16 = 0x2800;     // Trail for backtracking (1KB)
const TRAIL_END: u16 = 0x2BFF;
const STACK_START: u16 = 0x2C00;     // Choice point stack (1KB)
const STACK_END: u16 = 0x2FFF;
const INPUT_BUF: u16 = 0x3000;       // Line input buffer (256 bytes)
const ATOM_TABLE: u16 = 0x3100;      // Atom string table (512 bytes)
const CLAUSE_DB: u16 = 0x3300;       // Clause database (512 bytes)
const ARG_REGS: u16 = 0x3500;        // Argument registers (32 bytes)
const SCRATCH: u16 = 0x3600;         // Scratch area
const CPU_STACK: u16 = 0x3FFF;       // CPU stack top

/// SPM register locations in RAM
const SPM_H: u16 = 0x3520;           // Heap pointer
const SPM_TR: u16 = 0x3522;          // Trail pointer
const SPM_B: u16 = 0x3524;           // Choice point pointer
const SPM_S: u16 = 0x3526;           // Structure pointer
const SPM_MODE: u16 = 0x3528;        // Read/Write mode
const ATOM_COUNT: u16 = 0x352A;      // Number of atoms
const ATOM_NEXT: u16 = 0x352C;       // Next free atom slot
const CLAUSE_COUNT: u16 = 0x352E;    // Number of clauses
const CLAUSE_NEXT: u16 = 0x3530;     // Next free clause slot
const QUERY_TERM: u16 = 0x3532;      // Current query term (for backtracking)
const SEARCH_IDX: u16 = 0x3534;      // Current search index in clause DB
const LOOKAHEAD_TOK: u16 = 0x3536;   // Lookahead token type (0xFF = none)
const LOOKAHEAD_VAL: u16 = 0x3537;   // Lookahead token value (2 bytes)
const SCRATCH_POS: u16 = 0x3539;     // Current position in SCRATCH area (2 bytes)
const VAR_TABLE: u16 = 0x353B;       // Variable table start (name_char, addr_lo, addr_hi) * 8 = 24 bytes
const VAR_TABLE_COUNT: u16 = 0x3553; // Number of entries in variable table

/// Environment frame support for proper variable scoping
const TEMPLATE_VAR_BASE: u16 = 0x3E00;  // Template variable marker (index N = 0x3E00 + N)
const ENV_FRAME: u16 = 0x3560;          // Environment frame: 8 vars × 2 bytes = 16 bytes
const PARSE_MODE: u16 = 0x3570;         // Parse mode: 0 = heap vars (query), 1 = template vars (clause)
const ENV_STACK: u16 = 0x3580;          // Environment stack for nested calls (256 bytes)
const ENV_STACK_PTR: u16 = 0x3572;      // Current position in ENV_STACK
const INST_SRC: u16 = 0x3574;           // instantiate_goal: source arg pointer
const INST_DST: u16 = 0x3576;           // instantiate_goal: dest arg pointer
const INST_COUNT: u16 = 0x3578;         // instantiate_goal: remaining arg count

/// Token types
mod tok {
    pub const EOF: u8 = 0x00;
    pub const ATOM: u8 = 0x01;
    pub const VAR: u8 = 0x02;
    pub const INT: u8 = 0x03;
    pub const LPAREN: u8 = 0x04;
    pub const RPAREN: u8 = 0x05;
    pub const LBRACKET: u8 = 0x06;
    pub const RBRACKET: u8 = 0x07;
    pub const COMMA: u8 = 0x08;
    pub const DOT: u8 = 0x09;
    pub const PIPE: u8 = 0x0A;
    pub const NECK: u8 = 0x0B;        // :-
    pub const QUERY: u8 = 0x0C;       // ?-
    pub const CUT: u8 = 0x0D;         // !
    pub const UNDERSCORE: u8 = 0x0E;  // _
    // Arithmetic tokens
    pub const IS: u8 = 0x10;          // is
    pub const PLUS: u8 = 0x11;        // +
    pub const MINUS: u8 = 0x12;       // -
    pub const STAR: u8 = 0x13;        // *
    pub const SLASH: u8 = 0x14;       // /
    pub const MOD: u8 = 0x15;         // mod
    pub const ERROR: u8 = 0xFF;
}

/// Tag values for terms
mod tag {
    pub const REF: u8 = 0x00;         // Reference/unbound variable
    pub const INT: u8 = 0x40;         // Integer (14-bit immediate)
    pub const ATM: u8 = 0x80;         // Atom index
    pub const STR: u8 = 0xC0;         // Structure pointer
}

pub struct ReplGenerator {
    code: Vec<u8>,
    labels: std::collections::HashMap<String, u16>,
    fixups: Vec<(usize, String)>,
    current_addr: u16,
}

impl ReplGenerator {
    pub fn new() -> Self {
        ReplGenerator {
            code: Vec::new(),
            labels: std::collections::HashMap::new(),
            fixups: Vec::new(),
            current_addr: 0x0000,  // Emulator loads binary at address 0
        }
    }

    fn emit(&mut self, bytes: &[u8]) {
        self.code.extend_from_slice(bytes);
        self.current_addr += bytes.len() as u16;
    }

    fn emit_byte(&mut self, b: u8) {
        self.code.push(b);
        self.current_addr += 1;
    }

    fn emit_word(&mut self, w: u16) {
        self.code.push((w & 0xFF) as u8);
        self.code.push((w >> 8) as u8);
        self.current_addr += 2;
    }

    fn label(&mut self, name: &str) {
        self.labels.insert(name.to_string(), self.current_addr);
    }

    fn fixup(&mut self, name: &str) {
        self.fixups.push((self.code.len(), name.to_string()));
        self.emit_word(0x0000);
    }

    fn resolve_fixups(&mut self) {
        for (pos, name) in &self.fixups {
            if let Some(&addr) = self.labels.get(name) {
                self.code[*pos] = (addr & 0xFF) as u8;
                self.code[*pos + 1] = (addr >> 8) as u8;
            } else {
                panic!("Unresolved label: {}", name);
            }
        }
    }

    /// Generate the complete REPL binary.
    pub fn generate(&mut self) -> Vec<u8> {
        // Entry point
        self.emit(&[0x31]); // LD SP, CPU_STACK
        self.emit_word(CPU_STACK);

        // Initialize heap pointer
        self.emit(&[0x21]); // LD HL, HEAP_START
        self.emit_word(HEAP_START);
        self.emit(&[0x22]); // LD (SPM_H), HL
        self.emit_word(SPM_H);

        // Initialize trail pointer
        self.emit(&[0x21]); // LD HL, TRAIL_START
        self.emit_word(TRAIL_START);
        self.emit(&[0x22]); // LD (SPM_TR), HL
        self.emit_word(SPM_TR);

        // Initialize choice point pointer to 0
        self.emit(&[0x21]); // LD HL, 0
        self.emit_word(0x0000);
        self.emit(&[0x22]); // LD (SPM_B), HL
        self.emit_word(SPM_B);

        // Initialize atom table pointer
        self.emit(&[0x21]); // LD HL, ATOM_TABLE
        self.emit_word(ATOM_TABLE);
        self.emit(&[0x22]); // LD (ATOM_NEXT), HL
        self.emit_word(ATOM_NEXT);

        // Zero atom count
        self.emit(&[0x21]); // LD HL, 0
        self.emit_word(0x0000);
        self.emit(&[0x22]); // LD (ATOM_COUNT), HL
        self.emit_word(ATOM_COUNT);

        // Initialize clause database pointer
        self.emit(&[0x21]); // LD HL, CLAUSE_DB
        self.emit_word(CLAUSE_DB);
        self.emit(&[0x22]); // LD (CLAUSE_NEXT), HL
        self.emit_word(CLAUSE_NEXT);

        // Zero clause count
        self.emit(&[0x21]); // LD HL, 0
        self.emit_word(0x0000);
        self.emit(&[0x22]); // LD (CLAUSE_COUNT), HL
        self.emit_word(CLAUSE_COUNT);

        // Initialize environment stack pointer
        self.emit(&[0x21]); // LD HL, ENV_STACK
        self.emit_word(ENV_STACK);
        self.emit(&[0x22]); // LD (ENV_STACK_PTR), HL
        self.emit_word(ENV_STACK_PTR);

        // Print welcome message
        self.emit(&[0xCD]); // CALL print_welcome
        self.fixup("print_welcome");

        // Jump to REPL loop
        self.emit(&[0xC3]); // JP repl_loop
        self.fixup("repl_loop");

        // Generate all runtime routines
        self.emit_serial_io();
        self.emit_print_routines();
        self.emit_read_line();
        self.emit_lexer();
        self.emit_parser();
        self.emit_unification();
        self.emit_heap_ops();
        self.emit_trail_ops();
        self.emit_backtracking();
        self.emit_clause_db();
        self.emit_builtins();
        self.emit_repl_loop();

        // Resolve all label fixups
        self.resolve_fixups();

        self.code.clone()
    }

    /// Serial I/O routines for MC6850 ACIA (port 0x80/0x81)
    fn emit_serial_io(&mut self) {
        // Read character (blocking)
        // Output: A = character
        self.label("getchar");
        self.emit(&[0xDB, 0x80]);     // IN A, (0x80) - status
        self.emit(&[0xE6, 0x01]);     // AND 0x01 - RDRF bit
        self.emit(&[0x28, 0xFA]);     // JR Z, getchar (-6)
        self.emit(&[0xDB, 0x81]);     // IN A, (0x81) - data
        self.emit(&[0xC9]);           // RET

        // Write character
        // Input: A = character
        self.label("putchar");
        self.emit(&[0xF5]);           // PUSH AF
        self.label("putchar_wait");
        self.emit(&[0xDB, 0x80]);     // IN A, (0x80) - status
        self.emit(&[0xE6, 0x02]);     // AND 0x02 - TDRE bit
        self.emit(&[0x28, 0xFA]);     // JR Z, putchar_wait
        self.emit(&[0xF1]);           // POP AF
        self.emit(&[0xD3, 0x81]);     // OUT (0x81), A
        self.emit(&[0xC9]);           // RET

        // Check if character available (non-blocking)
        // Output: Z flag set if no char, A = char if available
        self.label("kbhit");
        self.emit(&[0xDB, 0x80]);     // IN A, (0x80)
        self.emit(&[0xE6, 0x01]);     // AND 0x01
        self.emit(&[0xC8]);           // RET Z (no char)
        self.emit(&[0xDB, 0x81]);     // IN A, (0x81)
        self.emit(&[0xC9]);           // RET
    }

    /// Print routines
    fn emit_print_routines(&mut self) {
        // Print null-terminated string
        // Input: HL = string address
        self.label("print_string");
        self.emit(&[0x7E]);           // LD A, (HL)
        self.emit(&[0xB7]);           // OR A
        self.emit(&[0xC8]);           // RET Z
        self.emit(&[0xCD]);           // CALL putchar
        self.fixup("putchar");
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0xC3]);           // JP print_string (use absolute jump)
        self.fixup("print_string");

        // Print newline
        self.label("print_nl");
        self.emit(&[0x3E, 0x0D]);     // LD A, '\r'
        self.emit(&[0xCD]);           // CALL putchar
        self.fixup("putchar");
        self.emit(&[0x3E, 0x0A]);     // LD A, '\n'
        self.emit(&[0xCD]);           // CALL putchar
        self.fixup("putchar");
        self.emit(&[0xC9]);           // RET

        // Print integer in HL
        // Uses sentinel value on stack to know when digits are done
        self.label("print_int");
        // Check sign
        self.emit(&[0xCB, 0x7C]);     // BIT 7, H
        self.emit(&[0xCA]);           // JP Z, print_int_pos
        self.fixup("print_int_pos");
        // Print minus
        self.emit(&[0xE5]);           // PUSH HL (save value)
        self.emit(&[0x3E, b'-']);     // LD A, '-'
        self.emit(&[0xCD]);           // CALL putchar
        self.fixup("putchar");
        self.emit(&[0xE1]);           // POP HL
        // Negate HL
        self.emit(&[0xAF]);           // XOR A
        self.emit(&[0x95]);           // SUB L
        self.emit(&[0x6F]);           // LD L, A
        self.emit(&[0x9F]);           // SBC A, A
        self.emit(&[0x94]);           // SUB H
        self.emit(&[0x67]);           // LD H, A

        self.label("print_int_pos");
        // Push sentinel (A=0xFF) to mark end of digits
        self.emit(&[0x3E, 0xFF]);     // LD A, 0xFF
        self.emit(&[0xF5]);           // PUSH AF

        self.label("print_int_loop");
        // Divide by 10
        self.emit(&[0x11]);           // LD DE, 10
        self.emit_word(10);
        self.emit(&[0xCD]);           // CALL div16
        self.fixup("div16");
        // A = remainder, HL = quotient
        self.emit(&[0xC6, b'0']);     // ADD A, '0'
        self.emit(&[0xF5]);           // PUSH AF (push digit)
        // Check if HL is 0
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xB5]);           // OR L
        self.emit(&[0xC2]);           // JP NZ, print_int_loop
        self.fixup("print_int_loop");

        // Print digits (they're in reverse order on stack)
        self.label("print_int_out");
        self.emit(&[0xF1]);           // POP AF
        self.emit(&[0xFE, 0xFF]);     // CP 0xFF (sentinel?)
        self.emit(&[0xC8]);           // RET Z (if sentinel, done)
        self.emit(&[0xCD]);           // CALL putchar
        self.fixup("putchar");
        self.emit(&[0xC3]);           // JP print_int_out
        self.fixup("print_int_out");

        // 16-bit division: HL / DE -> HL quotient, A remainder
        // Simplified division: use repeated subtraction for small numbers
        self.label("div16");
        self.emit(&[0xE5]);           // PUSH HL (save dividend)
        self.emit(&[0x21]);           // LD HL, 0 (quotient)
        self.emit_word(0x0000);
        self.emit(&[0xC1]);           // POP BC (BC = dividend)

        self.label("div16_loop");
        // Compare BC with DE
        self.emit(&[0x78]);           // LD A, B
        self.emit(&[0xBA]);           // CP D
        self.emit(&[0xDA]);           // JP C, div16_done (B < D)
        self.fixup("div16_done");
        self.emit(&[0xC2]);           // JP NZ, div16_sub (B > D)
        self.fixup("div16_sub");
        self.emit(&[0x79]);           // LD A, C
        self.emit(&[0xBB]);           // CP E
        self.emit(&[0xDA]);           // JP C, div16_done (C < E)
        self.fixup("div16_done");

        self.label("div16_sub");
        // BC = BC - DE
        self.emit(&[0x79]);           // LD A, C
        self.emit(&[0x93]);           // SUB E
        self.emit(&[0x4F]);           // LD C, A
        self.emit(&[0x78]);           // LD A, B
        self.emit(&[0x9A]);           // SBC D
        self.emit(&[0x47]);           // LD B, A
        self.emit(&[0x23]);           // INC HL (quotient++)
        self.emit(&[0xC3]);           // JP div16_loop
        self.fixup("div16_loop");

        self.label("div16_done");
        // A = remainder (low byte of BC)
        self.emit(&[0x79]);           // LD A, C
        self.emit(&[0xC9]);           // RET

        // Print welcome message
        self.label("print_welcome");
        self.emit(&[0x21]);           // LD HL, welcome_msg
        self.fixup("welcome_msg");
        self.emit(&[0xCD]);           // CALL print_string
        self.fixup("print_string");
        self.emit(&[0xC9]);           // RET

        // Welcome message
        self.label("welcome_msg");
        let msg = b"kz80_prolog REPL v0.1\r\n";
        for &b in msg {
            self.emit_byte(b);
        }
        self.emit_byte(0);

        // Prompt string
        self.label("prompt_str");
        let prompt = b"?- ";
        for &b in prompt {
            self.emit_byte(b);
        }
        self.emit_byte(0);

        // "yes" string
        self.label("yes_str");
        let yes = b"yes\r\n";
        for &b in yes {
            self.emit_byte(b);
        }
        self.emit_byte(0);

        // "no" string
        self.label("no_str");
        let no = b"no\r\n";
        for &b in no {
            self.emit_byte(b);
        }
        self.emit_byte(0);

        // Error string
        self.label("error_str");
        let err = b"Error\r\n";
        for &b in err {
            self.emit_byte(b);
        }
        self.emit_byte(0);

        // "ok" string (for fact assertion)
        self.label("ok_str");
        let ok = b"ok\r\n";
        for &b in ok {
            self.emit_byte(b);
        }
        self.emit_byte(0);
    }

    /// Read line from serial into INPUT_BUF
    fn emit_read_line(&mut self) {
        self.label("read_line");
        self.emit(&[0x21]);           // LD HL, INPUT_BUF
        self.emit_word(INPUT_BUF);
        self.emit(&[0x06, 0]);        // LD B, 0 (count)

        self.label("read_line_loop");
        self.emit(&[0xCD]);           // CALL getchar
        self.fixup("getchar");

        // Check for backspace
        self.emit(&[0xFE, 0x08]);     // CP 8
        self.emit(&[0xCA]);           // JP Z, read_line_bs
        self.fixup("read_line_bs");
        self.emit(&[0xFE, 0x7F]);     // CP 127
        self.emit(&[0xCA]);           // JP Z, read_line_bs
        self.fixup("read_line_bs");

        // Check for enter (CR or LF)
        self.emit(&[0xFE, 0x0D]);     // CP '\r'
        self.emit(&[0xCA]);           // JP Z, read_line_done
        self.fixup("read_line_done");
        self.emit(&[0xFE, 0x0A]);     // CP '\n'
        self.emit(&[0xCA]);           // JP Z, read_line_done
        self.fixup("read_line_done");

        // Store character
        self.emit(&[0x77]);           // LD (HL), A
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x04]);           // INC B
        // Echo
        self.emit(&[0xCD]);           // CALL putchar
        self.fixup("putchar");
        self.emit(&[0xC3]);           // JP read_line_loop
        self.fixup("read_line_loop");

        // Backspace handling
        self.label("read_line_bs");
        self.emit(&[0x78]);           // LD A, B
        self.emit(&[0xB7]);           // OR A
        self.emit(&[0xCA]);           // JP Z, read_line_loop (ignore if at start)
        self.fixup("read_line_loop");
        self.emit(&[0x2B]);           // DEC HL
        self.emit(&[0x05]);           // DEC B
        // Echo backspace
        self.emit(&[0x3E, 0x08]);     // LD A, 8
        self.emit(&[0xCD]);           // CALL putchar
        self.fixup("putchar");
        self.emit(&[0x3E, b' ']);     // LD A, ' '
        self.emit(&[0xCD]);           // CALL putchar
        self.fixup("putchar");
        self.emit(&[0x3E, 0x08]);     // LD A, 8
        self.emit(&[0xCD]);           // CALL putchar
        self.fixup("putchar");
        self.emit(&[0xC3]);           // JP read_line_loop
        self.fixup("read_line_loop");

        // Done
        self.label("read_line_done");
        self.emit(&[0x36, 0x00]);     // LD (HL), 0 (null terminate)
        self.emit(&[0xCD]);           // CALL print_nl
        self.fixup("print_nl");
        self.emit(&[0xC9]);           // RET
    }

    /// Lexer - tokenizes input
    fn emit_lexer(&mut self) {
        // Initialize lexer
        // Sets IX to INPUT_BUF, clears lookahead
        self.label("lex_init");
        self.emit(&[0xDD, 0x21]);     // LD IX, INPUT_BUF
        self.emit_word(INPUT_BUF);
        // Clear lookahead (0xFF = none)
        self.emit(&[0x3E, 0xFF]);     // LD A, 0xFF
        self.emit(&[0x32]);           // LD (LOOKAHEAD_TOK), A
        self.emit_word(LOOKAHEAD_TOK);
        self.emit(&[0xC9]);           // RET

        // Unget token - save token back for next lex_next call
        // Input: A = token type, HL = token value
        self.label("lex_unget");
        self.emit(&[0x32]);           // LD (LOOKAHEAD_TOK), A
        self.emit_word(LOOKAHEAD_TOK);
        self.emit(&[0x22]);           // LD (LOOKAHEAD_VAL), HL
        self.emit_word(LOOKAHEAD_VAL);
        self.emit(&[0xC9]);           // RET

        // Get next token
        // Output: A = token type, HL = value (for INT) or atom index (for ATOM)
        self.label("lex_next");
        // Check for saved lookahead token
        self.emit(&[0x3A]);           // LD A, (LOOKAHEAD_TOK)
        self.emit_word(LOOKAHEAD_TOK);
        self.emit(&[0xFE, 0xFF]);     // CP 0xFF (none)
        self.emit(&[0xCA]);           // JP Z, lex_real_next
        self.fixup("lex_real_next");
        // Return saved token
        self.emit(&[0xF5]);           // PUSH AF (save token type)
        self.emit(&[0x3E, 0xFF]);     // LD A, 0xFF (clear lookahead)
        self.emit(&[0x32]);           // LD (LOOKAHEAD_TOK), A
        self.emit_word(LOOKAHEAD_TOK);
        self.emit(&[0x2A]);           // LD HL, (LOOKAHEAD_VAL)
        self.emit_word(LOOKAHEAD_VAL);
        self.emit(&[0xF1]);           // POP AF
        self.emit(&[0xC9]);           // RET

        self.label("lex_real_next");
        // Skip whitespace
        self.label("lex_skip_ws");
        self.emit(&[0xDD, 0x7E, 0x00]); // LD A, (IX+0)
        self.emit(&[0xFE, b' ']);     // CP ' '
        self.emit(&[0xCA]);           // JP Z, lex_advance
        self.fixup("lex_advance");
        self.emit(&[0xFE, b'\t']);    // CP '\t'
        self.emit(&[0xC2]);           // JP NZ, lex_check_char
        self.fixup("lex_check_char");
        self.label("lex_advance");
        self.emit(&[0xDD, 0x23]);     // INC IX
        self.emit(&[0xC3]);           // JP lex_skip_ws
        self.fixup("lex_skip_ws");

        self.label("lex_check_char");
        // Check for end of input
        self.emit(&[0xB7]);           // OR A
        self.emit(&[0xCA]);           // JP Z, lex_eof
        self.fixup("lex_eof");

        // Check for various tokens
        // Digit -> integer
        self.emit(&[0xFE, b'0']);     // CP '0'
        self.emit(&[0xDA]);           // JP C, lex_not_digit
        self.fixup("lex_not_digit");
        self.emit(&[0xFE, b'9' + 1]); // CP '9'+1
        self.emit(&[0xD2]);           // JP NC, lex_not_digit
        self.fixup("lex_not_digit");
        self.emit(&[0xC3]);           // JP lex_integer
        self.fixup("lex_integer");

        self.label("lex_not_digit");
        // Lowercase -> atom
        self.emit(&[0xFE, b'a']);     // CP 'a'
        self.emit(&[0xDA]);           // JP C, lex_not_lower
        self.fixup("lex_not_lower");
        self.emit(&[0xFE, b'z' + 1]); // CP 'z'+1
        self.emit(&[0xD2]);           // JP NC, lex_not_lower
        self.fixup("lex_not_lower");
        self.emit(&[0xC3]);           // JP lex_atom
        self.fixup("lex_atom");

        self.label("lex_not_lower");
        // Uppercase -> variable
        self.emit(&[0xFE, b'A']);     // CP 'A'
        self.emit(&[0xDA]);           // JP C, lex_not_upper
        self.fixup("lex_not_upper");
        self.emit(&[0xFE, b'Z' + 1]); // CP 'Z'+1
        self.emit(&[0xD2]);           // JP NC, lex_not_upper
        self.fixup("lex_not_upper");
        self.emit(&[0xC3]);           // JP lex_var
        self.fixup("lex_var");

        self.label("lex_not_upper");
        // Single character tokens
        self.emit(&[0xDD, 0x23]);     // INC IX (advance past char)

        self.emit(&[0xFE, b'(']);     // CP '('
        self.emit(&[0xCA]);           // JP Z, lex_lparen
        self.fixup("lex_lparen");
        self.emit(&[0xFE, b')']);     // CP ')'
        self.emit(&[0xCA]);           // JP Z, lex_rparen
        self.fixup("lex_rparen");
        self.emit(&[0xFE, b'[']);     // CP '['
        self.emit(&[0xCA]);           // JP Z, lex_lbracket
        self.fixup("lex_lbracket");
        self.emit(&[0xFE, b']']);     // CP ']'
        self.emit(&[0xCA]);           // JP Z, lex_rbracket
        self.fixup("lex_rbracket");
        self.emit(&[0xFE, b',']);     // CP ','
        self.emit(&[0xCA]);           // JP Z, lex_comma
        self.fixup("lex_comma");
        self.emit(&[0xFE, b'.']);     // CP '.'
        self.emit(&[0xCA]);           // JP Z, lex_dot
        self.fixup("lex_dot");
        self.emit(&[0xFE, b'|']);     // CP '|'
        self.emit(&[0xCA]);           // JP Z, lex_pipe
        self.fixup("lex_pipe");
        self.emit(&[0xFE, b'!']);     // CP '!'
        self.emit(&[0xCA]);           // JP Z, lex_cut
        self.fixup("lex_cut");
        self.emit(&[0xFE, b'_']);     // CP '_'
        self.emit(&[0xCA]);           // JP Z, lex_underscore
        self.fixup("lex_underscore");

        // Check for :- or ?-
        self.emit(&[0xFE, b':']);     // CP ':'
        self.emit(&[0xCA]);           // JP Z, lex_neck_maybe
        self.fixup("lex_neck_maybe");
        self.emit(&[0xFE, b'?']);     // CP '?'
        self.emit(&[0xCA]);           // JP Z, lex_query_maybe
        self.fixup("lex_query_maybe");

        // Arithmetic operators
        self.emit(&[0xFE, b'+']);     // CP '+'
        self.emit(&[0xCA]);           // JP Z, lex_plus
        self.fixup("lex_plus");
        self.emit(&[0xFE, b'-']);     // CP '-'
        self.emit(&[0xCA]);           // JP Z, lex_minus
        self.fixup("lex_minus");
        self.emit(&[0xFE, b'*']);     // CP '*'
        self.emit(&[0xCA]);           // JP Z, lex_star
        self.fixup("lex_star");
        self.emit(&[0xFE, b'/']);     // CP '/'
        self.emit(&[0xCA]);           // JP Z, lex_slash
        self.fixup("lex_slash");

        // Unknown - return error
        self.emit(&[0x3E, tok::ERROR]); // LD A, ERROR
        self.emit(&[0xC9]);           // RET

        // EOF
        self.label("lex_eof");
        self.emit(&[0x3E, tok::EOF]); // LD A, EOF
        self.emit(&[0xC9]);           // RET

        // Integer parsing
        self.label("lex_integer");
        self.emit(&[0x21]);           // LD HL, 0
        self.emit_word(0x0000);
        self.label("lex_int_loop");
        self.emit(&[0xDD, 0x7E, 0x00]); // LD A, (IX+0)
        self.emit(&[0xFE, b'0']);     // CP '0'
        self.emit(&[0xDA]);           // JP C, lex_int_done
        self.fixup("lex_int_done");
        self.emit(&[0xFE, b'9' + 1]); // CP '9'+1
        self.emit(&[0xD2]);           // JP NC, lex_int_done
        self.fixup("lex_int_done");
        // HL = HL * 10 + digit
        self.emit(&[0xD6, b'0']);     // SUB '0'
        self.emit(&[0x4F]);           // LD C, A
        self.emit(&[0x06, 0x00]);     // LD B, 0
        // Proper *10: HL*10 = HL*8 + HL*2
        self.emit(&[0xE5]);           // PUSH HL (save original)
        self.emit(&[0x29]);           // ADD HL, HL (*2)
        self.emit(&[0xE3]);           // EX (SP), HL (save *2, get original)
        self.emit(&[0x29]);           // ADD HL, HL (*2)
        self.emit(&[0x29]);           // ADD HL, HL (*4)
        self.emit(&[0x29]);           // ADD HL, HL (*8)
        self.emit(&[0xC1]);           // POP BC (BC = original*2)
        self.emit(&[0x09]);           // ADD HL, BC (HL = *8 + *2 = *10)
        self.emit(&[0x06, 0x00]);     // LD B, 0
        self.emit(&[0xDD, 0x4E, 0x00]); // LD C, (IX+0)
        self.emit(&[0x79]);           // LD A, C
        self.emit(&[0xD6, b'0']);     // SUB '0'
        self.emit(&[0x4F]);           // LD C, A
        self.emit(&[0x09]);           // ADD HL, BC (add digit)
        self.emit(&[0xDD, 0x23]);     // INC IX
        self.emit(&[0xC3]);           // JP lex_int_loop
        self.fixup("lex_int_loop");
        self.label("lex_int_done");
        self.emit(&[0x3E, tok::INT]); // LD A, INT
        self.emit(&[0xC9]);           // RET

        // Atom parsing - intern full atom string in ATOM_TABLE
        // Returns HL = pointer to null-terminated string in ATOM_TABLE
        self.label("lex_atom");
        // Get current atom table position into DE
        self.emit(&[0x2A]);           // LD HL, (ATOM_NEXT)
        self.emit_word(ATOM_NEXT);
        self.emit(&[0xEB]);           // EX DE, HL  (DE = destination)
        self.emit(&[0xD5]);           // PUSH DE (save start address) - NOTE: Using PUSH DE not PUSH HL!

        // Copy characters from IX to (DE) until non-alnum
        self.label("lex_atom_copy");
        self.emit(&[0xDD, 0x7E, 0x00]); // LD A, (IX+0)
        self.emit(&[0xCD]);           // CALL is_alnum
        self.fixup("is_alnum");
        self.emit(&[0xCA]);           // JP Z, lex_atom_end
        self.fixup("lex_atom_end");
        self.emit(&[0xDD, 0x7E, 0x00]); // LD A, (IX+0)
        self.emit(&[0x12]);           // LD (DE), A
        self.emit(&[0x13]);           // INC DE
        self.emit(&[0xDD, 0x23]);     // INC IX
        self.emit(&[0xC3]);           // JP lex_atom_copy
        self.fixup("lex_atom_copy");

        // Null-terminate and update ATOM_NEXT
        self.label("lex_atom_end");
        self.emit(&[0xAF]);           // XOR A (A = 0)
        self.emit(&[0x12]);           // LD (DE), A (null terminate)
        self.emit(&[0x13]);           // INC DE
        self.emit(&[0xEB]);           // EX DE, HL
        self.emit(&[0x22]);           // LD (ATOM_NEXT), HL
        self.emit_word(ATOM_NEXT);
        self.emit(&[0xD1]);           // POP DE (get start address back)
        self.emit(&[0xEB]);           // EX DE, HL (HL = start address)
        // Check for keywords: "is", "mod"
        // Save HL (start address) for restoration if not a keyword
        self.emit(&[0xE5]);           // PUSH HL (save start address)
        // Check for "is"
        self.emit(&[0x7E]);           // LD A, (HL) - first char
        self.emit(&[0xFE, b'i']);
        self.emit(&[0xC2]);           // JP NZ, lex_check_mod
        self.fixup("lex_check_mod");
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x7E]);           // LD A, (HL) - second char
        self.emit(&[0xFE, b's']);
        self.emit(&[0xC2]);           // JP NZ, lex_atom_not_keyword
        self.fixup("lex_atom_not_keyword");
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x7E]);           // LD A, (HL) - third char (should be null)
        self.emit(&[0xB7]);           // OR A
        self.emit(&[0xC2]);           // JP NZ, lex_atom_not_keyword
        self.fixup("lex_atom_not_keyword");
        // It's "is"
        self.emit(&[0xD1]);           // POP DE (discard saved start)
        self.emit(&[0x3E, tok::IS]);
        self.emit(&[0xC9]);           // RET

        // Check for "mod"
        self.label("lex_check_mod");
        self.emit(&[0xE1]);           // POP HL (restore start)
        self.emit(&[0xE5]);           // PUSH HL (save again)
        self.emit(&[0x7E]);           // LD A, (HL) - first char
        self.emit(&[0xFE, b'm']);
        self.emit(&[0xC2]);           // JP NZ, lex_atom_not_keyword
        self.fixup("lex_atom_not_keyword");
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x7E]);           // LD A, (HL) - second char
        self.emit(&[0xFE, b'o']);
        self.emit(&[0xC2]);           // JP NZ, lex_atom_not_keyword
        self.fixup("lex_atom_not_keyword");
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x7E]);           // LD A, (HL) - third char
        self.emit(&[0xFE, b'd']);
        self.emit(&[0xC2]);           // JP NZ, lex_atom_not_keyword
        self.fixup("lex_atom_not_keyword");
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x7E]);           // LD A, (HL) - fourth char (should be null)
        self.emit(&[0xB7]);           // OR A
        self.emit(&[0xC2]);           // JP NZ, lex_atom_not_keyword
        self.fixup("lex_atom_not_keyword");
        // It's "mod"
        self.emit(&[0xD1]);           // POP DE (discard saved start)
        self.emit(&[0x3E, tok::MOD]);
        self.emit(&[0xC9]);           // RET

        // Not a keyword - restore HL from stack
        self.label("lex_atom_not_keyword");
        self.emit(&[0xE1]);           // POP HL (restore start address)
        self.label("lex_atom_return");
        self.emit(&[0x3E, tok::ATOM]); // LD A, ATOM
        self.emit(&[0xC9]);           // RET

        // Variable parsing
        self.label("lex_var");
        self.emit(&[0xDD, 0x6E, 0x00]); // LD L, (IX+0)
        self.emit(&[0x26, 0x00]);     // LD H, 0
        self.emit(&[0xDD, 0x23]);     // INC IX
        // Skip remaining alphanumeric
        self.label("lex_var_skip");
        self.emit(&[0xDD, 0x7E, 0x00]); // LD A, (IX+0)
        self.emit(&[0xCD]);           // CALL is_alnum
        self.fixup("is_alnum");
        self.emit(&[0xCA]);           // JP Z, lex_var_done
        self.fixup("lex_var_done");
        self.emit(&[0xDD, 0x23]);     // INC IX
        self.emit(&[0xC3]);           // JP lex_var_skip
        self.fixup("lex_var_skip");
        self.label("lex_var_done");
        self.emit(&[0x3E, tok::VAR]); // LD A, VAR
        self.emit(&[0xC9]);           // RET

        // Single char token returns
        self.label("lex_lparen");
        self.emit(&[0x3E, tok::LPAREN]);
        self.emit(&[0xC9]);
        self.label("lex_rparen");
        self.emit(&[0x3E, tok::RPAREN]);
        self.emit(&[0xC9]);
        self.label("lex_lbracket");
        self.emit(&[0x3E, tok::LBRACKET]);
        self.emit(&[0xC9]);
        self.label("lex_rbracket");
        self.emit(&[0x3E, tok::RBRACKET]);
        self.emit(&[0xC9]);
        self.label("lex_comma");
        self.emit(&[0x3E, tok::COMMA]);
        self.emit(&[0xC9]);
        self.label("lex_dot");
        self.emit(&[0x3E, tok::DOT]);
        self.emit(&[0xC9]);
        self.label("lex_pipe");
        self.emit(&[0x3E, tok::PIPE]);
        self.emit(&[0xC9]);
        self.label("lex_cut");
        self.emit(&[0x3E, tok::CUT]);
        self.emit(&[0xC9]);
        self.label("lex_underscore");
        self.emit(&[0x3E, tok::UNDERSCORE]);
        self.emit(&[0xC9]);

        // Arithmetic operator returns
        self.label("lex_plus");
        self.emit(&[0x3E, tok::PLUS]);
        self.emit(&[0xC9]);
        self.label("lex_minus");
        self.emit(&[0x3E, tok::MINUS]);
        self.emit(&[0xC9]);
        self.label("lex_star");
        self.emit(&[0x3E, tok::STAR]);
        self.emit(&[0xC9]);
        self.label("lex_slash");
        self.emit(&[0x3E, tok::SLASH]);
        self.emit(&[0xC9]);

        // :- check
        self.label("lex_neck_maybe");
        self.emit(&[0xDD, 0x7E, 0x00]); // LD A, (IX+0)
        self.emit(&[0xFE, b'-']);     // CP '-'
        self.emit(&[0xC2]);           // JP NZ, lex_error
        self.fixup("lex_error");
        self.emit(&[0xDD, 0x23]);     // INC IX
        self.emit(&[0x3E, tok::NECK]);
        self.emit(&[0xC9]);

        // ?- check
        self.label("lex_query_maybe");
        self.emit(&[0xDD, 0x7E, 0x00]); // LD A, (IX+0)
        self.emit(&[0xFE, b'-']);     // CP '-'
        self.emit(&[0xC2]);           // JP NZ, lex_error
        self.fixup("lex_error");
        self.emit(&[0xDD, 0x23]);     // INC IX
        self.emit(&[0x3E, tok::QUERY]);
        self.emit(&[0xC9]);

        self.label("lex_error");
        self.emit(&[0x3E, tok::ERROR]);
        self.emit(&[0xC9]);

        // Helper: is alphanumeric
        // Input: A = char
        // Output: Z flag clear if alnum
        self.label("is_alnum");
        self.emit(&[0xFE, b'0']);
        self.emit(&[0xDA]);           // JP C, not_alnum
        self.fixup("not_alnum");
        self.emit(&[0xFE, b'9' + 1]);
        self.emit(&[0xDA]);           // JP C, is_alnum_yes
        self.fixup("is_alnum_yes");
        self.emit(&[0xFE, b'A']);
        self.emit(&[0xDA]);           // JP C, not_alnum
        self.fixup("not_alnum");
        self.emit(&[0xFE, b'z' + 1]);
        self.emit(&[0xDA]);           // JP C, is_alnum_yes
        self.fixup("is_alnum_yes");
        self.label("not_alnum");
        self.emit(&[0xAF]);           // XOR A (set Z)
        self.emit(&[0xC9]);
        self.label("is_alnum_yes");
        self.emit(&[0x3E, 0x01]);     // LD A, 1 (clear Z)
        self.emit(&[0xB7]);           // OR A
        self.emit(&[0xC9]);
    }

    /// Parser - builds terms
    fn emit_parser(&mut self) {
        // Parse a term
        // Output: HL = term (tagged pointer)
        self.label("parse_term");
        self.emit(&[0xCD]);           // CALL lex_next
        self.fixup("lex_next");

        // Switch on token type
        self.emit(&[0xFE, tok::INT]); // CP INT
        self.emit(&[0xCA]);           // JP Z, parse_int
        self.fixup("parse_int");
        self.emit(&[0xFE, tok::ATOM]); // CP ATOM
        self.emit(&[0xCA]);           // JP Z, parse_atom
        self.fixup("parse_atom");
        self.emit(&[0xFE, tok::VAR]); // CP VAR
        self.emit(&[0xCA]);           // JP Z, parse_var
        self.fixup("parse_var");
        self.emit(&[0xFE, tok::UNDERSCORE]); // CP UNDERSCORE
        self.emit(&[0xCA]);           // JP Z, parse_anon
        self.fixup("parse_anon");
        self.emit(&[0xFE, tok::LBRACKET]); // CP LBRACKET
        self.emit(&[0xCA]);           // JP Z, parse_list
        self.fixup("parse_list");

        // Error - unexpected token
        self.emit(&[0x21]);           // LD HL, 0 (error)
        self.emit_word(0x0000);
        self.emit(&[0xC9]);           // RET

        // Integer
        self.label("parse_int");
        // HL already has value, add INT tag
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xE6, 0x3F]);     // AND 0x3F (mask to 14 bits)
        self.emit(&[0xF6, tag::INT]); // OR INT tag
        self.emit(&[0x67]);           // LD H, A
        self.emit(&[0xC9]);           // RET

        // Atom
        self.label("parse_atom");
        // HL has atom index, check for compound term
        self.emit(&[0xE5]);           // PUSH HL (save atom)
        self.emit(&[0xCD]);           // CALL lex_next
        self.fixup("lex_next");
        self.emit(&[0xFE, tok::LPAREN]); // CP LPAREN
        self.emit(&[0xCA]);           // JP Z, parse_compound
        self.fixup("parse_compound");
        // Simple atom - need to unget the token we just read
        // A = token type, HL = token value (from lex_next)
        self.emit(&[0xCD]);           // CALL lex_unget
        self.fixup("lex_unget");
        self.emit(&[0xE1]);           // POP HL (get atom value back)
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xF6, tag::ATM]); // OR ATM tag
        self.emit(&[0x67]);           // LD H, A
        self.emit(&[0xC9]);           // RET

        // Compound term: functor(arg1, arg2, ...)
        // Stack has functor atom index
        // Heap layout: [functor (2), arity (1), arg1 (2), arg2 (2), ...]
        // We use IY to track arg position independently of SPM_H
        self.label("parse_compound");
        // Get heap pointer - this will be our term address
        self.emit(&[0x2A]);           // LD HL, (SPM_H)
        self.emit_word(SPM_H);
        self.emit(&[0xE5]);           // PUSH HL (save structure address)

        // Store functor (from stack)
        self.emit(&[0xD1]);           // POP DE (term address)
        self.emit(&[0xE1]);           // POP HL (functor)
        self.emit(&[0xD5]);           // PUSH DE (save term address again)
        self.emit(&[0xEB]);           // EX DE, HL (DE = functor, HL = term addr)
        self.emit(&[0x73]);           // LD (HL), E (store functor low)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x72]);           // LD (HL), D (store functor high)
        self.emit(&[0x23]);           // INC HL
        // Save arity address
        self.emit(&[0xE5]);           // PUSH HL (save arity address)
        self.emit(&[0x23]);           // INC HL (skip arity byte)
        // IY = current arg position (where to store next arg)
        // Z80 doesn't have LD IY, HL - use stack instead
        self.emit(&[0xE5]);           // PUSH HL
        self.emit(&[0xFD, 0xE1]);     // POP IY
        // CRITICAL: Reserve space for MAX_ARGS (8) arg slots = 16 bytes
        // This ensures that heap_alloc_var allocates AFTER the arg slot area,
        // preventing variables from being overwritten by subsequent arg storage
        self.emit(&[0x11]);           // LD DE, 16 (8 args * 2 bytes each)
        self.emit_word(16);
        self.emit(&[0x19]);           // ADD HL, DE
        self.emit(&[0x22]);           // LD (SPM_H), HL
        self.emit_word(SPM_H);

        // Parse arguments
        self.emit(&[0x06, 0x00]);     // LD B, 0 (arity counter)

        self.label("parse_compound_arg");
        // Save IY (arg position) and BC (arity) on stack
        self.emit(&[0xC5]);           // PUSH BC (save arity)
        self.emit(&[0xFD, 0xE5]);     // PUSH IY (save arg position)

        // Note: SPM_H was advanced by 16 bytes at structure start to reserve arg slots
        // So heap_alloc_var and nested parse_compound will allocate AFTER the reserved area

        // Parse one argument
        self.emit(&[0xCD]);           // CALL parse_term
        self.fixup("parse_term");
        // HL = parsed argument term
        // DE = arg term, restore IY
        self.emit(&[0xEB]);           // EX DE, HL (DE = arg term)
        self.emit(&[0xFD, 0xE1]);     // POP IY (restore arg position)

        // Store arg at IY position
        self.emit(&[0xFD, 0x73, 0x00]); // LD (IY+0), E
        self.emit(&[0xFD, 0x72, 0x01]); // LD (IY+1), D
        // Advance IY by 2 (args are at fixed offsets within structure)
        self.emit(&[0xFD, 0x23]);     // INC IY
        self.emit(&[0xFD, 0x23]);     // INC IY

        self.emit(&[0xC1]);           // POP BC
        self.emit(&[0x04]);           // INC B (arity++)

        // Check for comma (more args) or rparen (end)
        self.emit(&[0xC5]);           // PUSH BC
        self.emit(&[0xFD, 0xE5]);     // PUSH IY
        self.emit(&[0xCD]);           // CALL lex_next
        self.fixup("lex_next");
        self.emit(&[0xFD, 0xE1]);     // POP IY
        self.emit(&[0xC1]);           // POP BC
        self.emit(&[0xFE, tok::COMMA]); // CP COMMA
        self.emit(&[0xCA]);           // JP Z, parse_compound_arg
        self.fixup("parse_compound_arg");
        // Should be RPAREN - if not, error but continue

        // Fill in arity
        self.emit(&[0xD1]);           // POP DE (arity address)
        self.emit(&[0x78]);           // LD A, B (arity)
        self.emit(&[0x12]);           // LD (DE), A

        // Update SPM_H to after all args (IY points there now)
        self.emit(&[0xFD, 0xE5]);     // PUSH IY
        self.emit(&[0xE1]);           // POP HL
        // Compare with current SPM_H, use whichever is higher
        self.emit(&[0xED, 0x5B]);     // LD DE, (SPM_H)
        self.emit_word(SPM_H);
        // If HL > DE, update SPM_H to HL
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xBA]);           // CP D
        self.emit(&[0xDA]);           // JP C, parse_compound_keep_spmh (HL < DE)
        self.fixup("parse_compound_keep_spmh");
        self.emit(&[0xC2]);           // JP NZ, parse_compound_update_spmh (H > D)
        self.fixup("parse_compound_update_spmh");
        // H == D, compare L with E
        self.emit(&[0x7D]);           // LD A, L
        self.emit(&[0xBB]);           // CP E
        self.emit(&[0xDA]);           // JP C, parse_compound_keep_spmh (L < E)
        self.fixup("parse_compound_keep_spmh");

        self.label("parse_compound_update_spmh");
        self.emit(&[0x22]);           // LD (SPM_H), HL
        self.emit_word(SPM_H);

        self.label("parse_compound_keep_spmh");
        // Return STR-tagged pointer to structure
        self.emit(&[0xE1]);           // POP HL (structure address)
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xF6, tag::STR]); // OR STR tag
        self.emit(&[0x67]);           // LD H, A
        self.emit(&[0xC9]);           // RET

        // Variable - uses variable table for sharing
        // In clause mode (PARSE_MODE=1): returns template variable (0x3E00 + index)
        // In query mode (PARSE_MODE=0): returns heap variable
        // Input: L = first char of variable name
        // Output: HL = variable (template or heap depending on mode)
        self.label("parse_var");
        // Save var name char before lookup (lookup may destroy registers)
        self.emit(&[0x7D]);           // LD A, L (var name char)
        self.emit(&[0xF5]);           // PUSH AF (save var name char)
        // Look up variable in table
        self.emit(&[0xCD]);           // CALL var_table_lookup
        self.fixup("var_table_lookup");
        // var_table_lookup returns: Carry clear = found (HL = addr), Carry set = not found
        self.emit(&[0xD2]);           // JP NC, parse_var_found (carry clear = found)
        self.fixup("parse_var_found");
        // Not found - create new variable based on parse mode
        self.emit(&[0xF1]);           // POP AF (restore var name char)
        self.emit(&[0x47]);           // LD B, A (save var name in B)
        // Check parse mode
        self.emit(&[0x3A]);           // LD A, (PARSE_MODE)
        self.emit_word(PARSE_MODE);
        self.emit(&[0xB7]);           // OR A
        self.emit(&[0xCA]);           // JP Z, parse_var_heap_mode
        self.fixup("parse_var_heap_mode");
        // Template mode (clause parsing): return TEMPLATE_VAR_BASE + index
        self.emit(&[0x3A]);           // LD A, (VAR_TABLE_COUNT)
        self.emit_word(VAR_TABLE_COUNT);
        self.emit(&[0x6F]);           // LD L, A (L = index)
        self.emit(&[0x26, 0x3E]);     // LD H, 0x3E (H = TEMPLATE_VAR_BASE high byte)
        // HL = TEMPLATE_VAR_BASE + index = template variable
        self.emit(&[0xC3]);           // JP parse_var_add_to_table
        self.fixup("parse_var_add_to_table");

        self.label("parse_var_heap_mode");
        // Heap mode (query parsing): allocate real heap variable
        self.emit(&[0xCD]);           // CALL heap_alloc_var
        self.fixup("heap_alloc_var");
        // HL = new heap var address
        // Fall through to add to table

        self.label("parse_var_add_to_table");
        self.emit(&[0xE5]);           // PUSH HL (save var)
        // Add to table: A = name char, HL = var address
        self.emit(&[0x78]);           // LD A, B (restore var name)
        self.emit(&[0xCD]);           // CALL var_table_add
        self.fixup("var_table_add");
        self.emit(&[0xE1]);           // POP HL (restore var)
        self.emit(&[0xC9]);           // RET
        self.label("parse_var_found");
        self.emit(&[0xF1]);           // POP AF (discard saved var name)
        self.emit(&[0xC9]);           // RET (HL already has variable from lookup)

        // Anonymous variable - always creates new variable (not added to table)
        // Output: HL = variable (template or heap depending on mode)
        self.label("parse_anon");
        // Check parse mode
        self.emit(&[0x3A]);           // LD A, (PARSE_MODE)
        self.emit_word(PARSE_MODE);
        self.emit(&[0xB7]);           // OR A
        self.emit(&[0xCA]);           // JP Z, parse_anon_heap
        self.fixup("parse_anon_heap");
        // Template mode: return template variable with incremented index
        self.emit(&[0x3A]);           // LD A, (VAR_TABLE_COUNT)
        self.emit_word(VAR_TABLE_COUNT);
        self.emit(&[0x6F]);           // LD L, A (L = index)
        self.emit(&[0x26, 0x3E]);     // LD H, 0x3E (H = TEMPLATE_VAR_BASE high byte)
        // Increment count for next anonymous variable
        self.emit(&[0x3C]);           // INC A
        self.emit(&[0x32]);           // LD (VAR_TABLE_COUNT), A
        self.emit_word(VAR_TABLE_COUNT);
        self.emit(&[0xC9]);           // RET

        self.label("parse_anon_heap");
        // Heap mode: allocate real heap variable
        self.emit(&[0xCD]);           // CALL heap_alloc_var
        self.fixup("heap_alloc_var");
        self.emit(&[0xC9]);           // RET

        // Clear variable table (call at start of parsing each clause/query)
        self.label("var_table_clear");
        self.emit(&[0xAF]);           // XOR A (A = 0)
        self.emit(&[0x32]);           // LD (VAR_TABLE_COUNT), A
        self.emit_word(VAR_TABLE_COUNT);
        self.emit(&[0xC9]);           // RET

        // Look up variable in table
        // Input: A = variable name char
        // Output: Carry clear + HL = address if found; Carry set if not found
        self.label("var_table_lookup");
        self.emit(&[0x47]);           // LD B, A (save var name in B)
        self.emit(&[0x3A]);           // LD A, (VAR_TABLE_COUNT)
        self.emit_word(VAR_TABLE_COUNT);
        self.emit(&[0xB7]);           // OR A
        self.emit(&[0xCA]);           // JP Z, var_not_found (empty table)
        self.fixup("var_not_found");
        self.emit(&[0x4F]);           // LD C, A (C = count)
        self.emit(&[0x21]);           // LD HL, VAR_TABLE
        self.emit_word(VAR_TABLE);
        self.label("var_lookup_loop");
        self.emit(&[0x7E]);           // LD A, (HL) (table entry name char)
        self.emit(&[0xB8]);           // CP B (compare with search char)
        self.emit(&[0xCA]);           // JP Z, var_found
        self.fixup("var_found");
        // Not this entry, advance to next (3 bytes per entry)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x0D]);           // DEC C
        self.emit(&[0xC2]);           // JP NZ, var_lookup_loop
        self.fixup("var_lookup_loop");
        self.label("var_not_found");
        self.emit(&[0x37]);           // SCF (set carry = not found)
        self.emit(&[0xC9]);           // RET
        self.label("var_found");
        // HL points to name char, get address from next 2 bytes
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x5E]);           // LD E, (HL)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x56]);           // LD D, (HL)
        self.emit(&[0xEB]);           // EX DE, HL (HL = var address)
        self.emit(&[0xB7]);           // OR A (clear carry = found)
        self.emit(&[0xC9]);           // RET

        // Add variable to table
        // Input: A = variable name char, HL = variable address
        self.label("var_table_add");
        self.emit(&[0xE5]);           // PUSH HL (save address)
        self.emit(&[0x47]);           // LD B, A (save name char)
        // Calculate entry position: VAR_TABLE + count * 3
        self.emit(&[0x3A]);           // LD A, (VAR_TABLE_COUNT)
        self.emit_word(VAR_TABLE_COUNT);
        self.emit(&[0x4F]);           // LD C, A (C = count)
        // Multiply by 3: A * 3 = A + A + A
        self.emit(&[0x87]);           // ADD A, A (A * 2)
        self.emit(&[0x81]);           // ADD A, C (A * 3)
        self.emit(&[0x5F]);           // LD E, A
        self.emit(&[0x16, 0x00]);     // LD D, 0
        self.emit(&[0x21]);           // LD HL, VAR_TABLE
        self.emit_word(VAR_TABLE);
        self.emit(&[0x19]);           // ADD HL, DE (HL = entry address)
        // Store entry: [name_char, addr_lo, addr_hi]
        self.emit(&[0x70]);           // LD (HL), B (name char)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0xD1]);           // POP DE (var address)
        self.emit(&[0x73]);           // LD (HL), E
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x72]);           // LD (HL), D
        // Increment count
        self.emit(&[0x3A]);           // LD A, (VAR_TABLE_COUNT)
        self.emit_word(VAR_TABLE_COUNT);
        self.emit(&[0x3C]);           // INC A
        self.emit(&[0x32]);           // LD (VAR_TABLE_COUNT), A
        self.emit_word(VAR_TABLE_COUNT);
        self.emit(&[0xEB]);           // EX DE, HL (HL = var address for return)
        self.emit(&[0xC9]);           // RET

        // List - simplified
        self.label("parse_list");
        // Just return nil for now
        self.emit(&[0x21]);           // LD HL, nil atom
        self.emit_word(tag::ATM as u16); // Tagged nil
        self.emit(&[0xC9]);           // RET

        // ================================================================
        // Goal Parser - handles "X is Expr" syntax
        // ================================================================

        // Parse a goal - could be a regular term or "X is Expr"
        // Returns: HL = goal term
        self.label("parse_goal");
        // First parse a term
        self.emit(&[0xCD]);           // CALL parse_term
        self.fixup("parse_term");
        // Save the term
        self.emit(&[0xE5]);           // PUSH HL

        // Check if next token is IS
        self.emit(&[0xCD]);           // CALL lex_next
        self.fixup("lex_next");
        self.emit(&[0xFE, tok::IS]);  // CP IS
        self.emit(&[0xCA]);           // JP Z, parse_is_goal
        self.fixup("parse_is_goal");

        // Not IS - unget token and return the term
        self.emit(&[0xCD]);           // CALL lex_unget
        self.fixup("lex_unget");
        self.emit(&[0xE1]);           // POP HL (restore term)
        self.emit(&[0xC9]);           // RET

        // Parse "X is Expr" - build is(X, Expr) compound term
        self.label("parse_is_goal");
        // Stack has X (left side)
        // Parse the arithmetic expression on the right
        self.emit(&[0xCD]);           // CALL parse_arith_expr
        self.fixup("parse_arith_expr");
        // HL = expr result, stack has X
        // Build is(X, Expr) compound term
        // Get X from stack and expr from HL
        self.emit(&[0xEB]);           // EX DE, HL (DE = expr)
        self.emit(&[0xE1]);           // POP HL (HL = X)
        // Now: HL = X, DE = expr
        // Build compound is(X, Expr) on heap

        // Save X and expr on stack
        self.emit(&[0xE5]);           // PUSH HL (save X)
        self.emit(&[0xD5]);           // PUSH DE (save expr)
        // Stack: [expr, X]

        // Intern "is" as functor atom
        self.emit(&[0x2A]);           // LD HL, (ATOM_NEXT)
        self.emit_word(ATOM_NEXT);
        self.emit(&[0xE5]);           // PUSH HL (functor_ptr)
        // Stack: [functor_ptr, expr, X]
        self.emit(&[0x36, b'i']);     // LD (HL), 'i'
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x36, b's']);     // LD (HL), 's'
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x36, 0x00]);     // LD (HL), 0 (null)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x22]);           // LD (ATOM_NEXT), HL
        self.emit_word(ATOM_NEXT);

        // Build compound term on heap
        self.emit(&[0x2A]);           // LD HL, (SPM_H)
        self.emit_word(SPM_H);
        self.emit(&[0xE5]);           // PUSH HL (struct_addr)
        // Stack: [struct_addr, functor_ptr, expr, X]

        // Store functor pointer
        self.emit(&[0xD1]);           // POP DE (struct_addr → DE)
        self.emit(&[0xE1]);           // POP HL (functor_ptr → HL)
        self.emit(&[0xD5]);           // PUSH DE (save struct_addr again)
        self.emit(&[0xEB]);           // EX DE, HL (DE = functor, HL = struct_addr)
        // Stack: [struct_addr, expr, X]
        self.emit(&[0x73]);           // LD (HL), E
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x72]);           // LD (HL), D
        self.emit(&[0x23]);           // INC HL

        // Store arity (2)
        self.emit(&[0x36, 0x02]);     // LD (HL), 2
        self.emit(&[0x23]);           // INC HL

        // Store X (first arg) - from stack
        // Stack: [struct_addr, expr, X]
        self.emit(&[0xE5]);           // PUSH HL (heap_pos)
        // Stack: [heap_pos, struct_addr, expr, X]
        self.emit(&[0xDD, 0x21]);     // LD IX, 0
        self.emit_word(0);
        self.emit(&[0xDD, 0x39]);     // ADD IX, SP
        // [IX+6] = X (4th word from top)
        self.emit(&[0xDD, 0x6E, 0x06]); // LD L, (IX+6)
        self.emit(&[0xDD, 0x66, 0x07]); // LD H, (IX+7)
        self.emit(&[0xEB]);           // EX DE, HL (DE = X)
        self.emit(&[0xE1]);           // POP HL (heap_pos)
        // Stack: [struct_addr, expr, X]
        self.emit(&[0x73]);           // LD (HL), E
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x72]);           // LD (HL), D
        self.emit(&[0x23]);           // INC HL

        // Store expr (second arg)
        self.emit(&[0xE5]);           // PUSH HL
        self.emit(&[0xDD, 0x21]);     // LD IX, 0
        self.emit_word(0);
        self.emit(&[0xDD, 0x39]);     // ADD IX, SP
        // [IX+4] = expr
        self.emit(&[0xDD, 0x6E, 0x04]); // LD L, (IX+4)
        self.emit(&[0xDD, 0x66, 0x05]); // LD H, (IX+5)
        self.emit(&[0xEB]);           // EX DE, HL (DE = expr)
        self.emit(&[0xE1]);           // POP HL (heap_pos)
        self.emit(&[0x73]);           // LD (HL), E
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x72]);           // LD (HL), D
        self.emit(&[0x23]);           // INC HL

        // Update heap pointer
        self.emit(&[0x22]);           // LD (SPM_H), HL
        self.emit_word(SPM_H);

        // Return struct_addr with STR tag
        self.emit(&[0xE1]);           // POP HL (struct_addr)
        self.emit(&[0xD1]);           // POP DE (discard expr)
        self.emit(&[0xD1]);           // POP DE (discard X)
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xF6, tag::STR]); // OR STR tag
        self.emit(&[0x67]);           // LD H, A
        self.emit(&[0xC9]);           // RET

        // ================================================================
        // Arithmetic Expression Parser
        // ================================================================

        // Parse an arithmetic expression (handles +, -, *, /, mod)
        // Returns: HL = parsed term (INT, VAR heap ref, or compound for ops)
        self.label("parse_arith_expr");
        // First parse multiplicative expression
        self.emit(&[0xCD]);           // CALL parse_arith_mul
        self.fixup("parse_arith_mul");
        // Check for + or -
        self.label("parse_arith_add_loop");
        self.emit(&[0xE5]);           // PUSH HL (save left operand)
        self.emit(&[0xCD]);           // CALL lex_next
        self.fixup("lex_next");
        // A = token type
        self.emit(&[0xFE, tok::PLUS]);
        self.emit(&[0xCA]);           // JP Z, parse_arith_add
        self.fixup("parse_arith_add");
        self.emit(&[0xFE, tok::MINUS]);
        self.emit(&[0xCA]);           // JP Z, parse_arith_sub
        self.fixup("parse_arith_sub");
        // Not + or -, unget and return
        self.emit(&[0xCD]);           // CALL lex_unget
        self.fixup("lex_unget");
        self.emit(&[0xE1]);           // POP HL (restore result)
        self.emit(&[0xC9]);           // RET

        // Handle addition
        self.label("parse_arith_add");
        self.emit(&[0xCD]);           // CALL parse_arith_mul
        self.fixup("parse_arith_mul");
        // HL = right operand, stack has [left operand]
        self.emit(&[0xEB]);           // EX DE, HL (DE = right)
        self.emit(&[0xE1]);           // POP HL (HL = left)
        // Now: HL = left, DE = right
        self.emit(&[0x3E, b'+']);     // LD A, '+' (op code)
        self.emit(&[0xCD]);           // CALL build_arith_op
        self.fixup("build_arith_op");
        // Result in HL, loop will push it
        self.emit(&[0xC3]);           // JP parse_arith_add_loop
        self.fixup("parse_arith_add_loop");

        // Handle subtraction
        self.label("parse_arith_sub");
        self.emit(&[0xCD]);           // CALL parse_arith_mul
        self.fixup("parse_arith_mul");
        // HL = right operand, stack has [left operand]
        self.emit(&[0xEB]);           // EX DE, HL (DE = right)
        self.emit(&[0xE1]);           // POP HL (HL = left)
        self.emit(&[0x3E, b'-']);     // LD A, '-'
        self.emit(&[0xCD]);           // CALL build_arith_op
        self.fixup("build_arith_op");
        // Result in HL, loop will push it
        self.emit(&[0xC3]);           // JP parse_arith_add_loop
        self.fixup("parse_arith_add_loop");

        // Parse multiplicative expression (* / mod)
        self.label("parse_arith_mul");
        self.emit(&[0xCD]);           // CALL parse_arith_primary
        self.fixup("parse_arith_primary");
        self.label("parse_arith_mul_loop");
        self.emit(&[0xE5]);           // PUSH HL
        self.emit(&[0xCD]);           // CALL lex_next
        self.fixup("lex_next");
        self.emit(&[0xFE, tok::STAR]);
        self.emit(&[0xCA]);           // JP Z, parse_arith_mul_op
        self.fixup("parse_arith_mul_op");
        self.emit(&[0xFE, tok::SLASH]);
        self.emit(&[0xCA]);           // JP Z, parse_arith_div_op
        self.fixup("parse_arith_div_op");
        self.emit(&[0xFE, tok::MOD]);
        self.emit(&[0xCA]);           // JP Z, parse_arith_mod_op
        self.fixup("parse_arith_mod_op");
        // Not *, /, mod - unget and return
        self.emit(&[0xCD]);           // CALL lex_unget
        self.fixup("lex_unget");
        self.emit(&[0xE1]);           // POP HL
        self.emit(&[0xC9]);           // RET

        self.label("parse_arith_mul_op");
        self.emit(&[0xCD]);           // CALL parse_arith_primary
        self.fixup("parse_arith_primary");
        // HL = right operand, stack has [left operand]
        self.emit(&[0xEB]);           // EX DE, HL (DE = right)
        self.emit(&[0xE1]);           // POP HL (HL = left)
        self.emit(&[0x3E, b'*']);
        self.emit(&[0xCD]);           // CALL build_arith_op
        self.fixup("build_arith_op");
        // Result in HL, loop will push it
        self.emit(&[0xC3]);           // JP parse_arith_mul_loop
        self.fixup("parse_arith_mul_loop");

        self.label("parse_arith_div_op");
        self.emit(&[0xCD]);           // CALL parse_arith_primary
        self.fixup("parse_arith_primary");
        // HL = right operand, stack has [left operand]
        self.emit(&[0xEB]);           // EX DE, HL (DE = right)
        self.emit(&[0xE1]);           // POP HL (HL = left)
        self.emit(&[0x3E, b'/']);
        self.emit(&[0xCD]);           // CALL build_arith_op
        self.fixup("build_arith_op");
        // Result in HL, loop will push it
        self.emit(&[0xC3]);           // JP parse_arith_mul_loop
        self.fixup("parse_arith_mul_loop");

        self.label("parse_arith_mod_op");
        self.emit(&[0xCD]);           // CALL parse_arith_primary
        self.fixup("parse_arith_primary");
        // HL = right operand, stack has [left operand]
        self.emit(&[0xEB]);           // EX DE, HL (DE = right)
        self.emit(&[0xE1]);           // POP HL (HL = left)
        self.emit(&[0x3E, b'm']);     // Use 'm' for mod to match eval_arith
        self.emit(&[0xCD]);           // CALL build_arith_op
        self.fixup("build_arith_op");
        // Result in HL, loop will push it
        self.emit(&[0xC3]);           // JP parse_arith_mul_loop
        self.fixup("parse_arith_mul_loop");

        // Parse primary arithmetic term: integer, variable, (expr)
        self.label("parse_arith_primary");
        self.emit(&[0xCD]);           // CALL lex_next
        self.fixup("lex_next");
        self.emit(&[0xFE, tok::INT]);
        self.emit(&[0xCA]);           // JP Z, parse_arith_int
        self.fixup("parse_arith_int");
        self.emit(&[0xFE, tok::VAR]);
        self.emit(&[0xCA]);           // JP Z, parse_arith_var
        self.fixup("parse_arith_var");
        self.emit(&[0xFE, tok::LPAREN]);
        self.emit(&[0xCA]);           // JP Z, parse_arith_paren
        self.fixup("parse_arith_paren");
        // Error - return 0
        self.emit(&[0x21]);
        self.emit_word(tag::INT as u16); // Return 0 with INT tag
        self.emit(&[0xC9]);

        self.label("parse_arith_int");
        // HL has integer value, add INT tag
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xE6, 0x3F]);     // AND 0x3F
        self.emit(&[0xF6, tag::INT]); // OR INT tag
        self.emit(&[0x67]);           // LD H, A
        self.emit(&[0xC9]);           // RET

        self.label("parse_arith_var");
        // HL has variable name (first char in L)
        // Look up in variable table
        self.emit(&[0x7D]);           // LD A, L (variable name char)
        self.emit(&[0xCD]);           // CALL var_table_lookup
        self.fixup("var_table_lookup");
        self.emit(&[0xD0]);           // RET NC (found, HL = var address)
        // Not found - error, return 0
        self.emit(&[0x21]);
        self.emit_word(tag::INT as u16);
        self.emit(&[0xC9]);

        self.label("parse_arith_paren");
        // Skip '(' and parse expression
        self.emit(&[0xCD]);           // CALL parse_arith_expr
        self.fixup("parse_arith_expr");
        self.emit(&[0xE5]);           // PUSH HL
        self.emit(&[0xCD]);           // CALL lex_next
        self.fixup("lex_next");
        // Should be RPAREN, ignore if not
        self.emit(&[0xE1]);           // POP HL
        self.emit(&[0xC9]);           // RET

        // Build arithmetic operator compound term
        // Input: HL = left operand, DE = right operand, A = op char (+, -, *, /, %)
        // Output: HL = compound term with STR tag
        // Heap layout: [functor_ptr (2), arity (1)=2, left (2), right (2)] = 7 bytes
        self.label("build_arith_op");
        // Registers: HL = left, DE = right, A = op char
        // IMPORTANT: Save IX because we'll use it for stack indexing
        self.emit(&[0xDD, 0xE5]);     // PUSH IX (save lexer pointer)
        self.emit(&[0x47]);           // LD B, A (save op char in B)
        self.emit(&[0xE5]);           // PUSH HL (save left)
        self.emit(&[0xD5]);           // PUSH DE (save right)
        // Stack: [right, left, saved_IX]

        // Create functor atom (single char op)
        self.emit(&[0x2A]);           // LD HL, (ATOM_NEXT)
        self.emit_word(ATOM_NEXT);
        self.emit(&[0xE5]);           // PUSH HL (functor_ptr for later)
        // Stack: [functor_ptr, right, left, saved_IX]
        self.emit(&[0x70]);           // LD (HL), B (store op char)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x36, 0x00]);     // LD (HL), 0 (null terminate)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x22]);           // LD (ATOM_NEXT), HL
        self.emit_word(ATOM_NEXT);

        // Now build compound term on heap
        self.emit(&[0x2A]);           // LD HL, (SPM_H)
        self.emit_word(SPM_H);
        self.emit(&[0xE5]);           // PUSH HL (struct_addr for return)
        // Stack: [struct_addr, functor_ptr, right, left, saved_IX]

        // Store functor pointer (from stack)
        self.emit(&[0xD1]);           // POP DE (struct_addr into DE)
        self.emit(&[0xE1]);           // POP HL (functor_ptr into HL)
        self.emit(&[0xD5]);           // PUSH DE (save struct_addr)
        self.emit(&[0xEB]);           // EX DE, HL (DE = functor_ptr, HL = struct_addr)
        // Stack: [struct_addr, right, left, saved_IX]
        self.emit(&[0x73]);           // LD (HL), E (functor low)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x72]);           // LD (HL), D (functor high)
        self.emit(&[0x23]);           // INC HL

        // Store arity (2)
        self.emit(&[0x36, 0x02]);     // LD (HL), 2
        self.emit(&[0x23]);           // INC HL

        // Store left operand - need to get from stack
        // Current stack: [struct_addr, right, left, saved_IX]
        // HL points to where left should be stored
        self.emit(&[0xE5]);           // PUSH HL (save current heap pos)
        // Stack: [heap_pos, struct_addr, right, left, saved_IX]

        // Get left from stack using IX (temporarily - will restore later)
        self.emit(&[0xDD, 0x21]);     // LD IX, 0
        self.emit_word(0);
        self.emit(&[0xDD, 0x39]);     // ADD IX, SP
        // [IX+0] = heap_pos, [IX+2] = struct_addr, [IX+4] = right, [IX+6] = left, [IX+8] = saved_IX
        self.emit(&[0xDD, 0x6E, 0x06]); // LD L, (IX+6)
        self.emit(&[0xDD, 0x66, 0x07]); // LD H, (IX+7)
        // HL = left operand
        self.emit(&[0xEB]);           // EX DE, HL (DE = left)
        self.emit(&[0xE1]);           // POP HL (heap_pos)
        // Stack: [struct_addr, right, left, saved_IX]
        self.emit(&[0x73]);           // LD (HL), E
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x72]);           // LD (HL), D
        self.emit(&[0x23]);           // INC HL

        // Store right operand
        self.emit(&[0xE5]);           // PUSH HL (save heap pos)
        self.emit(&[0xDD, 0x21]);     // LD IX, 0
        self.emit_word(0);
        self.emit(&[0xDD, 0x39]);     // ADD IX, SP
        // [IX+0] = heap_pos, [IX+2] = struct_addr, [IX+4] = right, [IX+6] = left, [IX+8] = saved_IX
        self.emit(&[0xDD, 0x6E, 0x04]); // LD L, (IX+4)
        self.emit(&[0xDD, 0x66, 0x05]); // LD H, (IX+5)
        self.emit(&[0xEB]);           // EX DE, HL (DE = right)
        self.emit(&[0xE1]);           // POP HL (heap_pos)
        self.emit(&[0x73]);           // LD (HL), E
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x72]);           // LD (HL), D
        self.emit(&[0x23]);           // INC HL

        // Update heap pointer
        self.emit(&[0x22]);           // LD (SPM_H), HL
        self.emit_word(SPM_H);

        // Get struct_addr from stack and add STR tag
        // Stack: [struct_addr, right, left, saved_IX]
        self.emit(&[0xE1]);           // POP HL (struct_addr)
        self.emit(&[0xD1]);           // POP DE (discard right)
        self.emit(&[0xD1]);           // POP DE (discard left)
        self.emit(&[0xDD, 0xE1]);     // POP IX (restore lexer pointer)
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xF6, tag::STR]); // OR STR tag
        self.emit(&[0x67]);           // LD H, A
        self.emit(&[0xC9]);           // RET
    }

    /// Unification algorithm
    fn emit_unification(&mut self) {
        // Dereference a term
        // Input: HL = term
        // Output: HL = dereferenced term
        self.label("deref");
        // Check if template variable (H == 0x3E)
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xFE, 0x3E]);     // CP 0x3E
        self.emit(&[0xC2]);           // JP NZ, deref_normal
        self.fixup("deref_normal");
        // Template variable - translate through ENV_FRAME
        self.emit(&[0x7D]);           // LD A, L (index)
        self.emit(&[0x87]);           // ADD A, A (index * 2)
        self.emit(&[0x5F]);           // LD E, A
        self.emit(&[0x16, 0x00]);     // LD D, 0
        self.emit(&[0x21]);           // LD HL, ENV_FRAME
        self.emit_word(ENV_FRAME);
        self.emit(&[0x19]);           // ADD HL, DE (HL = &ENV_FRAME[index])
        self.emit(&[0x5E]);           // LD E, (HL)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x56]);           // LD D, (HL)
        self.emit(&[0xEB]);           // EX DE, HL (HL = actual heap var address)
        // Fall through to normal deref

        self.label("deref_normal");
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xE6, 0xC0]);     // AND 0xC0 (get tag)
        self.emit(&[0xC0]);           // RET NZ (not REF)
        // It's a REF, check if bound
        self.emit(&[0x5E]);           // LD E, (HL)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x56]);           // LD D, (HL)
        self.emit(&[0x2B]);           // DEC HL
        // Check if self-referential (DE == HL means unbound)
        self.emit(&[0x7B]);           // LD A, E
        self.emit(&[0xBD]);           // CP L
        self.emit(&[0xC2]);           // JP NZ, deref_follow (E != L, so bound, follow chain)
        self.fixup("deref_follow");
        self.emit(&[0x7A]);           // LD A, D
        self.emit(&[0xBC]);           // CP H
        self.emit(&[0xC8]);           // RET Z (D == H and E == L, so unbound)
        // Fall through to follow
        self.label("deref_follow");
        self.emit(&[0xEB]);           // EX DE, HL
        self.emit(&[0xC3]);           // JP deref
        self.fixup("deref");

        // Unify two terms
        // Input: HL = term1, DE = term2
        // Output: Carry clear = success, Carry set = fail
        self.label("unify");
        self.emit(&[0xC5]);           // PUSH BC

        // Save term2
        self.emit(&[0xD5]);           // PUSH DE

        // Dereference term1 (HL)
        self.emit(&[0xCD]);           // CALL deref
        self.fixup("deref");
        // HL = deref(term1)

        // Save deref(term1), get term2
        self.emit(&[0xD1]);           // POP DE (DE = term2)
        self.emit(&[0xE5]);           // PUSH HL (save deref(term1))
        self.emit(&[0xEB]);           // EX DE, HL (HL = term2)

        // Dereference term2 (HL)
        self.emit(&[0xCD]);           // CALL deref
        self.fixup("deref");
        // HL = deref(term2)

        // Get deref(term1) back, arrange: HL=deref(term1), DE=deref(term2)
        self.emit(&[0xD1]);           // POP DE (DE = deref(term1))
        self.emit(&[0xEB]);           // EX DE, HL
        // Now: HL = deref(term1), DE = deref(term2)

        // Check if identical
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xBA]);           // CP D
        self.emit(&[0xC2]);           // JP NZ, unify_not_identical
        self.fixup("unify_not_identical");
        self.emit(&[0x7D]);           // LD A, L
        self.emit(&[0xBB]);           // CP E
        self.emit(&[0xCA]);           // JP Z, unify_success
        self.fixup("unify_success");

        self.label("unify_not_identical");
        // Check if term1 is unbound REF (tag 0x00)
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xE6, 0xC0]);     // AND 0xC0
        self.emit(&[0xC2]);           // JP NZ, unify_t1_not_ref
        self.fixup("unify_t1_not_ref");
        // term1 is unbound REF, bind it to term2
        self.emit(&[0x73]);           // LD (HL), E
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x72]);           // LD (HL), D
        self.emit(&[0x2B]);           // DEC HL
        self.emit(&[0xCD]);           // CALL trail
        self.fixup("trail");
        self.emit(&[0xC3]);           // JP unify_success
        self.fixup("unify_success");

        self.label("unify_t1_not_ref");
        // term1 is not REF. Check if term2 is unbound REF
        self.emit(&[0x7A]);           // LD A, D
        self.emit(&[0xE6, 0xC0]);     // AND 0xC0
        self.emit(&[0xC2]);           // JP NZ, unify_both_nonref
        self.fixup("unify_both_nonref");
        // term2 is unbound REF, bind it to term1
        self.emit(&[0xEB]);           // EX DE, HL
        self.emit(&[0x73]);           // LD (HL), E
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x72]);           // LD (HL), D
        self.emit(&[0x2B]);           // DEC HL
        self.emit(&[0xCD]);           // CALL trail
        self.fixup("trail");
        self.emit(&[0xC3]);           // JP unify_success
        self.fixup("unify_success");

        // Neither is REF - check term types
        self.label("unify_both_nonref");
        // term1 tag is in bits 7-6 of H, term2 tag in bits 7-6 of D
        // Get term1 tag
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xE6, 0xC0]);     // AND 0xC0
        self.emit(&[0x4F]);           // LD C, A (save term1 tag in C)
        // Get term2 tag
        self.emit(&[0x7A]);           // LD A, D
        self.emit(&[0xE6, 0xC0]);     // AND 0xC0
        // Compare tags
        self.emit(&[0xB9]);           // CP C (compare term2 tag with term1 tag)
        self.emit(&[0xC2]);           // JP NZ, unify_fail (tags must match)
        self.fixup("unify_fail");

        // Tags match - check which type
        // A still has the tag
        self.emit(&[0xFE, tag::INT]); // CP INT
        self.emit(&[0xCA]);           // JP Z, unify_int
        self.fixup("unify_int");
        self.emit(&[0xFE, tag::ATM]); // CP ATM
        self.emit(&[0xCA]);           // JP Z, unify_atom
        self.fixup("unify_atom");
        self.emit(&[0xFE, tag::STR]); // CP STR
        self.emit(&[0xC2]);           // JP NZ, unify_fail (unknown tag)
        self.fixup("unify_fail");
        // Fall through to STR unification
        self.emit(&[0xC3]);           // JP unify_struct
        self.fixup("unify_struct");

        // Unify integers - compare the 14-bit values
        self.label("unify_int");
        // HL and DE are already the tagged integers
        // Strip tags and compare values
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xE6, 0x3F]);     // AND 0x3F
        self.emit(&[0x67]);           // LD H, A
        self.emit(&[0x7A]);           // LD A, D
        self.emit(&[0xE6, 0x3F]);     // AND 0x3F
        self.emit(&[0x57]);           // LD D, A
        // Compare HL and DE
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xBA]);           // CP D
        self.emit(&[0xC2]);           // JP NZ, unify_fail
        self.fixup("unify_fail");
        self.emit(&[0x7D]);           // LD A, L
        self.emit(&[0xBB]);           // CP E
        self.emit(&[0xC2]);           // JP NZ, unify_fail
        self.fixup("unify_fail");
        self.emit(&[0xC3]);           // JP unify_success
        self.fixup("unify_success");

        // Unify atoms - compare the strings they point to
        self.label("unify_atom");
        // HL and DE point to atom strings (with ATM tag in high bits)
        // Strip tags
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xE6, 0x3F]);     // AND 0x3F
        self.emit(&[0x67]);           // LD H, A
        self.emit(&[0x7A]);           // LD A, D
        self.emit(&[0xE6, 0x3F]);     // AND 0x3F
        self.emit(&[0x57]);           // LD D, A
        // HL and DE now point to null-terminated strings
        // Compare strings
        self.label("unify_atom_loop");
        self.emit(&[0x1A]);           // LD A, (DE)
        self.emit(&[0xBE]);           // CP (HL)
        self.emit(&[0xC2]);           // JP NZ, unify_fail
        self.fixup("unify_fail");
        // Check if both at end (null)
        self.emit(&[0xB7]);           // OR A
        self.emit(&[0xCA]);           // JP Z, unify_success (both ended)
        self.fixup("unify_success");
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x13]);           // INC DE
        self.emit(&[0xC3]);           // JP unify_atom_loop
        self.fixup("unify_atom_loop");

        // Unify structures
        self.label("unify_struct");

        // Both are STR - unify structures
        // HL = STR-tagged addr1, DE = STR-tagged addr2
        // Strip tags to get actual addresses
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xE6, 0x3F]);     // AND 0x3F (remove tag)
        self.emit(&[0x67]);           // LD H, A
        self.emit(&[0x7A]);           // LD A, D
        self.emit(&[0xE6, 0x3F]);     // AND 0x3F
        self.emit(&[0x57]);           // LD D, A
        // Now HL and DE point to structures

        // Compare functors - they're string pointers now
        // Save structure base addresses
        self.emit(&[0xE5]);           // PUSH HL (struct1 base)
        self.emit(&[0xD5]);           // PUSH DE (struct2 base)

        // Load functor pointer from struct1 (HL) into BC
        self.emit(&[0x4E]);           // LD C, (HL) (functor1 low)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x46]);           // LD B, (HL) (functor1 high)

        // Load functor pointer from struct2 (DE) into HL via DE
        self.emit(&[0xEB]);           // EX DE, HL (HL = struct2 base)
        self.emit(&[0x5E]);           // LD E, (HL) (functor2 low)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x56]);           // LD D, (HL) (functor2 high)
        // Now BC = functor1 ptr, DE = functor2 ptr

        // Move functor1 from BC to HL for string compare
        self.emit(&[0x60]);           // LD H, B
        self.emit(&[0x69]);           // LD L, C
        // Now HL = functor1, DE = functor2

        // Compare functor strings
        self.label("cmp_functor_loop");
        self.emit(&[0x1A]);           // LD A, (DE)
        self.emit(&[0xBE]);           // CP (HL)
        self.emit(&[0xC2]);           // JP NZ, unify_struct_fail
        self.fixup("unify_struct_fail");
        self.emit(&[0xB7]);           // OR A (check if null)
        self.emit(&[0xCA]);           // JP Z, cmp_functor_done
        self.fixup("cmp_functor_done");
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x13]);           // INC DE
        self.emit(&[0xC3]);           // JP cmp_functor_loop
        self.fixup("cmp_functor_loop");

        self.label("cmp_functor_done");
        // Restore structure pointers and advance past functor to arity
        self.emit(&[0xD1]);           // POP DE (struct2 base)
        self.emit(&[0xE1]);           // POP HL (struct1 base)
        self.emit(&[0x23]);           // INC HL (skip functor low)
        self.emit(&[0x23]);           // INC HL (skip functor high, now at arity)
        self.emit(&[0x13]);           // INC DE
        self.emit(&[0x13]);           // INC DE (now at arity2)

        // Compare arities (now at offset 2)
        self.emit(&[0x7E]);           // LD A, (HL) (arity1)
        self.emit(&[0xEB]);           // EX DE, HL
        self.emit(&[0xBE]);           // CP (HL) (arity2)
        self.emit(&[0xEB]);           // EX DE, HL (restore HL=struct1+2, DE=struct2+2)
        self.emit(&[0xC2]);           // JP NZ, unify_struct_fail
        self.fixup("unify_struct_fail");

        // Arities match - A has arity, HL at arity1, DE at arity2
        self.emit(&[0xB7]);           // OR A (check if arity 0)
        self.emit(&[0xCA]);           // JP Z, unify_struct_done
        self.fixup("unify_struct_done");

        // Save arity as counter and advance to first arg
        self.emit(&[0x47]);           // LD B, A
        self.emit(&[0x23]);           // INC HL (skip arity, now at first arg)
        self.emit(&[0x13]);           // INC DE (skip arity, now at first arg)

        // Unify each argument
        self.label("unify_args_loop");
        self.emit(&[0xC5]);           // PUSH BC (save counter)
        self.emit(&[0xE5]);           // PUSH HL (save ptr1)
        self.emit(&[0xD5]);           // PUSH DE (save ptr2)
        // Stack is now: [BC, ptr1, ptr2]

        // Load arg1 from (HL=ptr1) into BC temporarily
        self.emit(&[0x4E]);           // LD C, (HL)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x46]);           // LD B, (HL)  ; BC = arg1

        // Load arg2 from (DE=ptr2) into DE
        self.emit(&[0xEB]);           // EX DE, HL   ; HL = ptr2
        self.emit(&[0x5E]);           // LD E, (HL)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x56]);           // LD D, (HL)  ; DE = arg2

        // Move arg1 from BC to HL for unify call
        self.emit(&[0x60]);           // LD H, B
        self.emit(&[0x69]);           // LD L, C     ; HL = arg1, DE = arg2

        // Recursive unify(arg1, arg2)
        self.emit(&[0xCD]);           // CALL unify
        self.fixup("unify");

        // Restore from stack
        self.emit(&[0xD1]);           // POP DE (restore ptr2)
        self.emit(&[0xE1]);           // POP HL (restore ptr1)
        self.emit(&[0xC1]);           // POP BC (restore counter)
        // Check result - if carry set, unify failed
        self.emit(&[0xDA]);           // JP C, unify_args_fail
        self.fixup("unify_args_fail");
        // Advance pointers by 2
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x13]);           // INC DE
        self.emit(&[0x13]);           // INC DE
        // Decrement counter and loop if not zero
        self.emit(&[0x05]);           // DEC B
        self.emit(&[0xC2]);           // JP NZ, unify_args_loop
        self.fixup("unify_args_loop");
        // All args unified - success
        self.emit(&[0xC3]);           // JP unify_success
        self.fixup("unify_success");

        // Args unification failed - jump to outer unify_fail
        self.label("unify_args_fail");
        self.emit(&[0xC3]);           // JP unify_fail
        self.fixup("unify_fail");

        self.label("unify_struct_done");
        // Clean up stack from functor comparison (jumped here with arity 0)
        self.emit(&[0xD1]);           // POP DE
        self.emit(&[0xE1]);           // POP HL
        self.emit(&[0xC3]);           // JP unify_success
        self.fixup("unify_success");

        self.label("unify_struct_fail");
        self.emit(&[0xD1]);           // POP DE
        self.emit(&[0xE1]);           // POP HL
        // Fall through to unify_fail

        // Fail - different ground terms
        self.label("unify_fail");
        self.emit(&[0xC1]);           // POP BC
        self.emit(&[0x37]);           // SCF
        self.emit(&[0xC9]);           // RET

        self.label("unify_success");
        self.emit(&[0xC1]);           // POP BC
        self.emit(&[0xA7]);           // AND A (clear carry)
        self.emit(&[0xC9]);           // RET
    }

    /// Heap operations
    fn emit_heap_ops(&mut self) {
        // Allocate unbound variable on heap
        // Output: HL = address of new variable (tagged REF)
        self.label("heap_alloc_var");
        self.emit(&[0x2A]);           // LD HL, (SPM_H)
        self.emit_word(SPM_H);
        self.emit(&[0xE5]);           // PUSH HL (save address)
        // Create self-referential cell
        self.emit(&[0x75]);           // LD (HL), L
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x74]);           // LD (HL), H
        self.emit(&[0x2B]);           // DEC HL
        // Advance heap pointer
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x22]);           // LD (SPM_H), HL
        self.emit_word(SPM_H);
        self.emit(&[0xE1]);           // POP HL (return address)
        // REF tag is 0, so no tagging needed
        self.emit(&[0xC9]);           // RET
    }

    /// Trail operations
    fn emit_trail_ops(&mut self) {
        // Trail a variable binding
        // Input: HL = address of bound variable
        self.label("trail");
        self.emit(&[0xD5]);           // PUSH DE
        self.emit(&[0xED, 0x5B]);     // LD DE, (SPM_TR)
        self.emit_word(SPM_TR);
        // Store address HL to trail at (DE)
        self.emit(&[0xEB]);           // EX DE, HL
        self.emit(&[0x73]);           // LD (HL), E
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x72]);           // LD (HL), D
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x22]);           // LD (SPM_TR), HL
        self.emit_word(SPM_TR);
        self.emit(&[0xD1]);           // POP DE
        self.emit(&[0xC9]);           // RET

        // Unwind trail to saved point
        // Input: DE = saved trail pointer
        self.label("unwind_trail");
        self.emit(&[0x2A]);           // LD HL, (SPM_TR)
        self.emit_word(SPM_TR);
        self.label("unwind_loop");
        // Compare HL with DE
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xBA]);           // CP D
        self.emit(&[0xC2]);           // JP NZ, unwind_continue
        self.fixup("unwind_continue");
        self.emit(&[0x7D]);           // LD A, L
        self.emit(&[0xBB]);           // CP E
        self.emit(&[0xC8]);           // RET Z
        self.label("unwind_continue");
        // Back up trail pointer
        self.emit(&[0x2B]);           // DEC HL
        self.emit(&[0x2B]);           // DEC HL
        // Get trailed address
        self.emit(&[0x4E]);           // LD C, (HL)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x46]);           // LD B, (HL)
        self.emit(&[0x2B]);           // DEC HL
        // Reset to unbound (self-referential): store BC at (BC)
        self.emit(&[0x79]);           // LD A, C
        self.emit(&[0x02]);           // LD (BC), A
        self.emit(&[0x03]);           // INC BC
        self.emit(&[0x78]);           // LD A, B
        self.emit(&[0x02]);           // LD (BC), A
        // Continue loop
        self.emit(&[0xC3]);           // JP unwind_loop
        self.fixup("unwind_loop");
    }

    /// Backtracking with choice points
    /// Choice point structure (10 bytes):
    ///   offset 0: Previous B (2 bytes) - link to previous choice point
    ///   offset 2: TR (2 bytes) - trail pointer when choice point was created
    ///   offset 4: H (2 bytes) - heap pointer when choice point was created
    ///   offset 6: SEARCH_IDX (2 bytes) - next clause to try
    ///   offset 8: QUERY_TERM (2 bytes) - query being matched
    fn emit_backtracking(&mut self) {
        // Create choice point - called when we have alternatives
        // Input: none (uses current SPM_B, TR, H, SEARCH_IDX, QUERY_TERM)
        // Modifies: SPM_B
        self.label("create_choice_point");
        // Get current stack position (SPM_B points to top of stack, or 0 if empty)
        self.emit(&[0x2A]);           // LD HL, (SPM_B)
        self.emit_word(SPM_B);
        // If HL = 0, start at STACK_START
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xB5]);           // OR L
        self.emit(&[0xC2]);           // JP NZ, cp_have_prev
        self.fixup("cp_have_prev");
        // First choice point - start at STACK_START
        self.emit(&[0x21]);           // LD HL, STACK_START
        self.emit_word(STACK_START);
        self.emit(&[0xC3]);           // JP cp_store
        self.fixup("cp_store");

        self.label("cp_have_prev");
        // HL = previous choice point, new one goes 10 bytes after
        self.emit(&[0x11]);           // LD DE, 10
        self.emit_word(10);
        self.emit(&[0x19]);           // ADD HL, DE

        self.label("cp_store");
        // HL = address of new choice point
        // Save HL as new SPM_B
        self.emit(&[0xE5]);           // PUSH HL (save new CP addr)
        // Store previous B at offset 0
        self.emit(&[0x2A]);           // LD HL, (SPM_B)
        self.emit_word(SPM_B);
        self.emit(&[0xEB]);           // EX DE, HL (DE = old B)
        self.emit(&[0xE1]);           // POP HL (HL = new CP addr)
        self.emit(&[0xE5]);           // PUSH HL (save again)
        self.emit(&[0x73]);           // LD (HL), E - prev_B low
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x72]);           // LD (HL), D - prev_B high
        self.emit(&[0x23]);           // INC HL
        // Store TR at offset 2
        self.emit(&[0xD5]);           // PUSH DE (save)
        self.emit(&[0x2A]);           // LD HL, (SPM_TR)
        self.emit_word(SPM_TR);
        self.emit(&[0xEB]);           // EX DE, HL (DE = TR)
        self.emit(&[0xE1]);           // POP HL (dummy from earlier PUSH)
        // Get new CP addr from stack and set IX
        self.emit(&[0xE1]);           // POP HL (new CP addr)
        self.emit(&[0xDD, 0xE5]);     // PUSH IX (save old IX)
        self.emit(&[0xE5]);           // PUSH HL
        self.emit(&[0xDD, 0xE1]);     // POP IX (IX = new CP addr)
        // Store prev_B again at offset 0
        self.emit(&[0x2A]);           // LD HL, (SPM_B)
        self.emit_word(SPM_B);
        self.emit(&[0xDD, 0x75, 0x00]); // LD (IX+0), L - prev_B low
        self.emit(&[0xDD, 0x74, 0x01]); // LD (IX+1), H - prev_B high
        // Store TR at offset 2
        self.emit(&[0x2A]);           // LD HL, (SPM_TR)
        self.emit_word(SPM_TR);
        self.emit(&[0xDD, 0x75, 0x02]); // LD (IX+2), L - TR low
        self.emit(&[0xDD, 0x74, 0x03]); // LD (IX+3), H - TR high
        // Store H at offset 4
        self.emit(&[0x2A]);           // LD HL, (SPM_H)
        self.emit_word(SPM_H);
        self.emit(&[0xDD, 0x75, 0x04]); // LD (IX+4), L - H low
        self.emit(&[0xDD, 0x74, 0x05]); // LD (IX+5), H - H high
        // Store SEARCH_IDX at offset 6
        self.emit(&[0x2A]);           // LD HL, (SEARCH_IDX)
        self.emit_word(SEARCH_IDX);
        self.emit(&[0xDD, 0x75, 0x06]); // LD (IX+6), L - idx low
        self.emit(&[0xDD, 0x74, 0x07]); // LD (IX+7), H - idx high
        // Store QUERY_TERM at offset 8
        self.emit(&[0x2A]);           // LD HL, (QUERY_TERM)
        self.emit_word(QUERY_TERM);
        self.emit(&[0xDD, 0x75, 0x08]); // LD (IX+8), L - query low
        self.emit(&[0xDD, 0x74, 0x09]); // LD (IX+9), H - query high
        // Update SPM_B to point to new choice point
        self.emit(&[0xDD, 0xE5]);     // PUSH IX
        self.emit(&[0xE1]);           // POP HL (HL = IX = new CP addr)
        self.emit(&[0x22]);           // LD (SPM_B), HL
        self.emit_word(SPM_B);
        self.emit(&[0xDD, 0xE1]);     // POP IX (restore)
        self.emit(&[0xC9]);           // RET

        // Backtrack - try next alternative
        self.label("backtrack");
        // Check if any choice points
        self.emit(&[0x2A]);           // LD HL, (SPM_B)
        self.emit_word(SPM_B);
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xB5]);           // OR L
        self.emit(&[0xCA]);           // JP Z, backtrack_fail (no choice points)
        self.fixup("backtrack_fail");

        // Restore from choice point
        // HL = current choice point address
        self.emit(&[0xDD, 0xE5]);     // PUSH IX (save old IX)
        self.emit(&[0xE5]);           // PUSH HL
        self.emit(&[0xDD, 0xE1]);     // POP IX (IX = choice point addr)

        // Restore TR and unwind trail
        self.emit(&[0xDD, 0x6E, 0x02]); // LD L, (IX+2) - TR low
        self.emit(&[0xDD, 0x66, 0x03]); // LD H, (IX+3) - TR high
        self.emit(&[0xEB]);           // EX DE, HL (DE = saved TR)
        self.emit(&[0xD5]);           // PUSH DE (save target TR)
        self.emit(&[0xCD]);           // CALL unwind_trail (unwind to DE)
        self.fixup("unwind_trail");
        self.emit(&[0xD1]);           // POP DE (restore target TR)
        // Set SPM_TR to saved TR
        self.emit(&[0xEB]);           // EX DE, HL (HL = saved TR)
        self.emit(&[0x22]);           // LD (SPM_TR), HL
        self.emit_word(SPM_TR);

        // Restore heap pointer
        self.emit(&[0xDD, 0x6E, 0x04]); // LD L, (IX+4) - H low
        self.emit(&[0xDD, 0x66, 0x05]); // LD H, (IX+5) - H high
        self.emit(&[0x22]);           // LD (SPM_H), HL
        self.emit_word(SPM_H);

        // Restore SEARCH_IDX
        self.emit(&[0xDD, 0x6E, 0x06]); // LD L, (IX+6) - idx low
        self.emit(&[0xDD, 0x66, 0x07]); // LD H, (IX+7) - idx high
        self.emit(&[0x22]);           // LD (SEARCH_IDX), HL
        self.emit_word(SEARCH_IDX);

        // Restore QUERY_TERM
        self.emit(&[0xDD, 0x6E, 0x08]); // LD L, (IX+8) - query low
        self.emit(&[0xDD, 0x66, 0x09]); // LD H, (IX+9) - query high
        self.emit(&[0x22]);           // LD (QUERY_TERM), HL
        self.emit_word(QUERY_TERM);

        // Pop choice point (restore previous B)
        self.emit(&[0xDD, 0x6E, 0x00]); // LD L, (IX+0) - prev_B low
        self.emit(&[0xDD, 0x66, 0x01]); // LD H, (IX+1) - prev_B high
        self.emit(&[0x22]);           // LD (SPM_B), HL
        self.emit_word(SPM_B);

        self.emit(&[0xDD, 0xE1]);     // POP IX (restore)
        // Jump to try_next_clause to continue with next alternative
        self.emit(&[0xC3]);           // JP try_next_clause
        self.fixup("try_next_clause");

        self.label("backtrack_fail");
        // No choice points - fail completely
        self.emit(&[0x37]);           // SCF (set carry = fail)
        self.emit(&[0xC9]);           // RET
    }

    /// Clause database operations
    fn emit_clause_db(&mut self) {
        // Assert a fact - store term HL in clause database (body_count = 0)
        // Input: HL = term (head) to assert
        // Modifies: DE, BC
        // Format: [var_count, body_count=0, head_lo, head_hi]
        self.label("assert_fact");
        self.emit(&[0xE5]);           // PUSH HL (save term)
        // Get current clause slot
        self.emit(&[0x2A]);           // LD HL, (CLAUSE_NEXT)
        self.emit_word(CLAUSE_NEXT);
        // Store var_count (number of template variables in this clause)
        self.emit(&[0x3A]);           // LD A, (VAR_TABLE_COUNT)
        self.emit_word(VAR_TABLE_COUNT);
        self.emit(&[0x77]);           // LD (HL), A
        self.emit(&[0x23]);           // INC HL
        // Store body_count = 0 (this is a fact, not a rule)
        self.emit(&[0x36, 0x00]);     // LD (HL), 0
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0xD1]);           // POP DE (DE = head term)
        // Store head term
        self.emit(&[0x73]);           // LD (HL), E
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x72]);           // LD (HL), D
        self.emit(&[0x23]);           // INC HL
        // Update clause pointer
        self.emit(&[0x22]);           // LD (CLAUSE_NEXT), HL
        self.emit_word(CLAUSE_NEXT);
        // Increment clause count
        self.emit(&[0x2A]);           // LD HL, (CLAUSE_COUNT)
        self.emit_word(CLAUSE_COUNT);
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x22]);           // LD (CLAUSE_COUNT), HL
        self.emit_word(CLAUSE_COUNT);
        self.emit(&[0xC9]);           // RET

        // Assert a rule - store head + body goals in clause database
        // Input: HL = head term, B = body_count, body goals stored in SCRATCH
        // Format: [var_count, body_count, head_lo, head_hi, body1_lo, body1_hi, ...]
        self.label("assert_rule");
        self.emit(&[0xE5]);           // PUSH HL (save head)
        self.emit(&[0xC5]);           // PUSH BC (save body count)
        // Get current clause slot
        self.emit(&[0x2A]);           // LD HL, (CLAUSE_NEXT)
        self.emit_word(CLAUSE_NEXT);
        // Store var_count (number of template variables in this clause)
        self.emit(&[0x3A]);           // LD A, (VAR_TABLE_COUNT)
        self.emit_word(VAR_TABLE_COUNT);
        self.emit(&[0x77]);           // LD (HL), A
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0xC1]);           // POP BC (restore body count)
        // Store body_count
        self.emit(&[0x70]);           // LD (HL), B (store body_count)
        self.emit(&[0x23]);           // INC HL
        // Store head term
        self.emit(&[0xD1]);           // POP DE (DE = head term)
        self.emit(&[0x73]);           // LD (HL), E
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x72]);           // LD (HL), D
        self.emit(&[0x23]);           // INC HL
        // Now store body goals from SCRATCH (body1, body2, ..., bodyn in order)
        self.emit(&[0x78]);           // LD A, B
        self.emit(&[0xB7]);           // OR A
        self.emit(&[0xCA]);           // JP Z, assert_rule_done
        self.fixup("assert_rule_done");
        // IX = SCRATCH (where body goals are stored)
        self.emit(&[0xDD, 0x21]);     // LD IX, SCRATCH
        self.emit_word(SCRATCH);
        self.label("assert_rule_body_loop");
        self.emit(&[0xDD, 0x5E, 0x00]); // LD E, (IX+0)
        self.emit(&[0xDD, 0x56, 0x01]); // LD D, (IX+1)
        self.emit(&[0x73]);           // LD (HL), E
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x72]);           // LD (HL), D
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0xDD, 0x23]);     // INC IX
        self.emit(&[0xDD, 0x23]);     // INC IX
        self.emit(&[0x05]);           // DEC B
        self.emit(&[0xC2]);           // JP NZ, assert_rule_body_loop
        self.fixup("assert_rule_body_loop");
        self.label("assert_rule_done");
        // Update clause pointer
        self.emit(&[0x22]);           // LD (CLAUSE_NEXT), HL
        self.emit_word(CLAUSE_NEXT);
        // Increment clause count
        self.emit(&[0x2A]);           // LD HL, (CLAUSE_COUNT)
        self.emit_word(CLAUSE_COUNT);
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x22]);           // LD (CLAUSE_COUNT), HL
        self.emit_word(CLAUSE_COUNT);
        self.emit(&[0xC9]);           // RET

        // Query database - try to find a matching clause
        // Input: HL = query term
        // Output: Carry clear = found match, Carry set = no match
        // SEARCH_IDX is an absolute address pointing to current clause
        self.label("query_db");
        // Save query term for backtracking
        self.emit(&[0x22]);           // LD (QUERY_TERM), HL
        self.emit_word(QUERY_TERM);
        // Start from first clause (CLAUSE_DB)
        self.emit(&[0x21]);           // LD HL, CLAUSE_DB
        self.emit_word(CLAUSE_DB);
        self.emit(&[0x22]);           // LD (SEARCH_IDX), HL
        self.emit_word(SEARCH_IDX);
        // Fall through to try_next_clause

        // Try next clause - continue search from SEARCH_IDX (absolute address)
        // Clause format: [body_count(1), head(2), body1(2), ..., bodyn(2)]
        self.label("try_next_clause");
        // Check if we've exhausted all clauses (SEARCH_IDX >= CLAUSE_NEXT)
        self.emit(&[0x2A]);           // LD HL, (SEARCH_IDX)
        self.emit_word(SEARCH_IDX);
        self.emit(&[0xEB]);           // EX DE, HL (DE = current clause addr)
        self.emit(&[0x2A]);           // LD HL, (CLAUSE_NEXT)
        self.emit_word(CLAUSE_NEXT);
        // Compare DE with HL (current vs end)
        self.emit(&[0x7A]);           // LD A, D
        self.emit(&[0xBC]);           // CP H
        self.emit(&[0xDA]);           // JP C, clause_addr_valid (D < H)
        self.fixup("clause_addr_valid");
        self.emit(&[0xC2]);           // JP NZ, query_fail (D > H)
        self.fixup("query_fail");
        self.emit(&[0x7B]);           // LD A, E
        self.emit(&[0xBD]);           // CP L
        self.emit(&[0xD2]);           // JP NC, query_fail (E >= L)
        self.fixup("query_fail");

        self.label("clause_addr_valid");
        // DE = current clause address
        self.emit(&[0xEB]);           // EX DE, HL (HL = clause addr)
        // Read var_count (new: first byte is variable count for env frame)
        self.emit(&[0x4E]);           // LD C, (HL) (C = var_count)
        self.emit(&[0x23]);           // INC HL
        // Read body_count
        self.emit(&[0x46]);           // LD B, (HL) (B = body_count)
        self.emit(&[0x23]);           // INC HL
        // Read head term into DE
        self.emit(&[0x5E]);           // LD E, (HL)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x56]);           // LD D, (HL) (DE = head term)
        self.emit(&[0x23]);           // INC HL
        // HL = body_ptr (points to first body goal if any)
        // Calculate next clause address: current + 4 + 2*body_count
        // (4 bytes for var_count + body_count + head_lo + head_hi)
        // Save current state first
        self.emit(&[0xE5]);           // PUSH HL (body_ptr)
        self.emit(&[0xC5]);           // PUSH BC (body_count in B, var_count in C)
        self.emit(&[0xD5]);           // PUSH DE (head term)
        // Add 2*B to HL for next clause addr
        self.emit(&[0x78]);           // LD A, B
        self.emit(&[0x87]);           // ADD A, A
        self.emit(&[0x5F]);           // LD E, A
        self.emit(&[0x16, 0x00]);     // LD D, 0
        self.emit(&[0x19]);           // ADD HL, DE
        self.emit(&[0x22]);           // LD (SEARCH_IDX), HL (SEARCH_IDX = next clause)
        self.emit_word(SEARCH_IDX);

        // Check if there are more clauses after this one
        // If yes, create a choice point to try the next one on backtrack
        self.emit(&[0xEB]);           // EX DE, HL (DE = next clause addr)
        self.emit(&[0x2A]);           // LD HL, (CLAUSE_NEXT)
        self.emit_word(CLAUSE_NEXT);
        // Compare DE with HL (next clause addr vs end)
        self.emit(&[0x7A]);           // LD A, D
        self.emit(&[0xBC]);           // CP H
        self.emit(&[0xDA]);           // JP C, has_more_clauses (D < H)
        self.fixup("has_more_clauses");
        self.emit(&[0xC2]);           // JP NZ, no_choice_point (D > H, no more clauses)
        self.fixup("no_choice_point");
        self.emit(&[0x7B]);           // LD A, E
        self.emit(&[0xBD]);           // CP L
        self.emit(&[0xD2]);           // JP NC, no_choice_point (E >= L, no more)
        self.fixup("no_choice_point");

        self.label("has_more_clauses");
        // More clauses exist - create a choice point
        self.emit(&[0xCD]);           // CALL create_choice_point
        self.fixup("create_choice_point");

        self.label("no_choice_point");
        // Restore head term, body_count/var_count, body_ptr
        self.emit(&[0xD1]);           // POP DE (head term)
        self.emit(&[0xC1]);           // POP BC (B = body_count, C = var_count)
        self.emit(&[0xE1]);           // POP HL (body_ptr)
        self.emit(&[0xE5]);           // PUSH HL (save body_ptr again)
        self.emit(&[0xC5]);           // PUSH BC (save body_count/var_count again)
        self.emit(&[0xD5]);           // PUSH DE (save head term)

        // Setup environment frame with fresh variables for this clause
        self.emit(&[0x79]);           // LD A, C (A = var_count)
        self.emit(&[0xCD]);           // CALL setup_env_frame
        self.fixup("setup_env_frame");
        // setup_env_frame preserves BC

        // Try to unify query with clause head
        // DE = head term (restore from stack), need HL = query term
        self.emit(&[0x2A]);           // LD HL, (QUERY_TERM)
        self.emit_word(QUERY_TERM);
        self.emit(&[0xD1]);           // POP DE (head term)
        self.emit(&[0xCD]);           // CALL unify
        self.fixup("unify");

        // Get body_count and body_ptr from stack
        self.emit(&[0xC1]);           // POP BC (body_count)
        self.emit(&[0xE1]);           // POP HL (body_ptr)

        // If unify failed, backtrack (don't just try next clause)
        self.emit(&[0xDA]);           // JP C, backtrack
        self.fixup("backtrack");

        // Unification succeeded! Check if this is a rule (body_count > 0)
        self.emit(&[0x78]);           // LD A, B
        self.emit(&[0xB7]);           // OR A
        self.emit(&[0xCA]);           // JP Z, query_success (fact, no body)
        self.fixup("query_success");

        // This is a rule - solve body goals
        // HL = body_ptr, B = body_count
        self.label("solve_body_goals");
        self.emit(&[0xC5]);           // PUSH BC (save remaining count)
        self.emit(&[0xE5]);           // PUSH HL (save body_ptr)
        // Load goal term
        self.emit(&[0x5E]);           // LD E, (HL)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x56]);           // LD D, (HL)
        self.emit(&[0xEB]);           // EX DE, HL (HL = goal term)
        // Instantiate the goal term: resolve template vars to heap addresses
        // This must happen BEFORE save_env_frame while we still have current clause's ENV_FRAME
        self.emit(&[0xCD]);           // CALL instantiate_goal
        self.fixup("instantiate_goal");
        // Save ENV_FRAME before recursive call
        self.emit(&[0xCD]);           // CALL save_env_frame
        self.fixup("save_env_frame");
        // Recursively solve this goal
        self.emit(&[0xCD]);           // CALL solve_goal
        self.fixup("solve_goal");
        // Restore ENV_FRAME after recursive call (carry flag preserved)
        self.emit(&[0xCD]);           // CALL restore_env_frame
        self.fixup("restore_env_frame");
        // Restore body_ptr and body_count
        self.emit(&[0xE1]);           // POP HL (body_ptr)
        self.emit(&[0xC1]);           // POP BC (body_count)
        // If goal failed, backtrack
        self.emit(&[0xDA]);           // JP C, backtrack
        self.fixup("backtrack");
        // Goal succeeded, advance to next body goal
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x05]);           // DEC B
        self.emit(&[0xC2]);           // JP NZ, solve_body_goals
        self.fixup("solve_body_goals");
        // All body goals solved - success!

        self.label("query_success");
        self.emit(&[0xA7]);           // AND A (clear carry = success)
        self.emit(&[0xC9]);           // RET

        // No more clauses - query fails
        self.label("query_fail");
        self.emit(&[0x37]);           // SCF (set carry = fail)
        self.emit(&[0xC9]);           // RET

        // Solve a single goal (recursive entry point for body goals)
        // Input: HL = goal term
        // Output: Carry clear = success, Carry set = failure
        self.label("solve_goal");
        // Check for built-ins before querying database
        // Check tag to determine type
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xE6, 0xC0]);     // AND 0xC0

        // Is it a simple atom (ATM)?
        self.emit(&[0xFE, tag::ATM]); // CP ATM
        self.emit(&[0xCA]);           // JP Z, solve_check_atom_builtin
        self.fixup("solve_check_atom_builtin");

        // Is it a compound term (STR)?
        self.emit(&[0xFE, tag::STR]); // CP STR
        self.emit(&[0xCA]);           // JP Z, solve_check_compound_builtin
        self.fixup("solve_check_compound_builtin");

        // Not a built-in, query the database
        // Save goal to QUERY_TERM before calling query_db
        self.emit(&[0x22]);           // LD (QUERY_TERM), HL
        self.emit_word(QUERY_TERM);
        self.emit(&[0xC3]);           // JP query_db
        self.fixup("query_db");

        // Check for built-in atoms in solve context
        // Must check FULL atom name, not just first char (e.g., 'foo' vs 'fail')
        self.label("solve_check_atom_builtin");
        // HL is atom pointer (with ATM tag), strip tag and get string pointer
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xE6, 0x3F]);     // AND 0x3F (strip tag)
        self.emit(&[0x67]);           // LD H, A
        // Now HL points to the null-terminated atom string
        // Save HL for later checks
        self.emit(&[0xE5]);           // PUSH HL

        // Check for "nl" (n,l,\0)
        self.emit(&[0x7E]);           // LD A, (HL) - first char
        self.emit(&[0xFE, b'n']);
        self.emit(&[0xC2]);           // JP NZ, solve_check_true
        self.fixup("solve_check_true");
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x7E]);           // LD A, (HL) - second char
        self.emit(&[0xFE, b'l']);
        self.emit(&[0xC2]);           // JP NZ, solve_check_true
        self.fixup("solve_check_true");
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x7E]);           // LD A, (HL) - third char (should be null)
        self.emit(&[0xB7]);           // OR A
        self.emit(&[0xC2]);           // JP NZ, solve_check_true
        self.fixup("solve_check_true");
        // It's "nl"
        self.emit(&[0xE1]);           // POP HL (discard saved pointer)
        self.emit(&[0xC3]);           // JP builtin_nl
        self.fixup("builtin_nl");

        // Check for "true" (t,r,u,e,\0)
        self.label("solve_check_true");
        self.emit(&[0xE1]);           // POP HL (restore atom pointer)
        self.emit(&[0xE5]);           // PUSH HL (save again)
        self.emit(&[0x7E]);           // LD A, (HL) - first char
        self.emit(&[0xFE, b't']);
        self.emit(&[0xC2]);           // JP NZ, solve_check_fail
        self.fixup("solve_check_fail");
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x7E]);           // LD A, (HL) - second char
        self.emit(&[0xFE, b'r']);
        self.emit(&[0xC2]);           // JP NZ, solve_check_fail
        self.fixup("solve_check_fail");
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x7E]);           // LD A, (HL) - third char
        self.emit(&[0xFE, b'u']);
        self.emit(&[0xC2]);           // JP NZ, solve_check_fail
        self.fixup("solve_check_fail");
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x7E]);           // LD A, (HL) - fourth char
        self.emit(&[0xFE, b'e']);
        self.emit(&[0xC2]);           // JP NZ, solve_check_fail
        self.fixup("solve_check_fail");
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x7E]);           // LD A, (HL) - fifth char (should be null)
        self.emit(&[0xB7]);           // OR A
        self.emit(&[0xC2]);           // JP NZ, solve_check_fail
        self.fixup("solve_check_fail");
        // It's "true"
        self.emit(&[0xE1]);           // POP HL (discard saved pointer)
        self.emit(&[0xC3]);           // JP builtin_true
        self.fixup("builtin_true");

        // Check for "fail" (f,a,i,l,\0)
        self.label("solve_check_fail");
        self.emit(&[0xE1]);           // POP HL (restore atom pointer)
        self.emit(&[0xE5]);           // PUSH HL (save for query_db)
        self.emit(&[0x7E]);           // LD A, (HL) - first char
        self.emit(&[0xFE, b'f']);
        self.emit(&[0xC2]);           // JP NZ, solve_atom_not_builtin
        self.fixup("solve_atom_not_builtin");
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x7E]);           // LD A, (HL) - second char
        self.emit(&[0xFE, b'a']);
        self.emit(&[0xC2]);           // JP NZ, solve_atom_not_builtin
        self.fixup("solve_atom_not_builtin");
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x7E]);           // LD A, (HL) - third char
        self.emit(&[0xFE, b'i']);
        self.emit(&[0xC2]);           // JP NZ, solve_atom_not_builtin
        self.fixup("solve_atom_not_builtin");
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x7E]);           // LD A, (HL) - fourth char
        self.emit(&[0xFE, b'l']);
        self.emit(&[0xC2]);           // JP NZ, solve_atom_not_builtin
        self.fixup("solve_atom_not_builtin");
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x7E]);           // LD A, (HL) - fifth char (should be null)
        self.emit(&[0xB7]);           // OR A
        self.emit(&[0xC2]);           // JP NZ, solve_atom_not_builtin
        self.fixup("solve_atom_not_builtin");
        // It's "fail"
        self.emit(&[0xE1]);           // POP HL (discard saved pointer)
        self.emit(&[0xC3]);           // JP builtin_fail
        self.fixup("builtin_fail");

        // Not a built-in atom, query database
        // Need to restore tagged HL for query_db
        self.label("solve_atom_not_builtin");
        self.emit(&[0xE1]);           // POP HL (untagged pointer)
        // Re-tag with ATM
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xF6, tag::ATM]); // OR ATM tag
        self.emit(&[0x67]);           // LD H, A
        self.emit(&[0x22]);           // LD (QUERY_TERM), HL
        self.emit_word(QUERY_TERM);
        self.emit(&[0xC3]);           // JP query_db
        self.fixup("query_db");

        // Check for built-in compound terms in solve context
        self.label("solve_check_compound_builtin");
        // HL is STR-tagged, strip tag to get structure address
        self.emit(&[0xE5]);           // PUSH HL (save original)
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xE6, 0x3F]);     // AND 0x3F
        self.emit(&[0x67]);           // LD H, A
        // Get functor pointer
        self.emit(&[0x5E]);           // LD E, (HL)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x56]);           // LD D, (HL)
        self.emit(&[0xEB]);           // EX DE, HL (HL = functor)
        // Get first char of functor
        self.emit(&[0x7E]);           // LD A, (HL)
        self.emit(&[0xE1]);           // POP HL (restore original STR pointer)

        // Check for 'w' (write) - need to check full "write" to avoid matching e.g. "wrap"
        self.emit(&[0xFE, b'w']);
        self.emit(&[0xCA]);           // JP Z, solve_check_write
        self.fixup("solve_check_write");

        // Check for 'i' (is)
        self.emit(&[0xFE, b'i']);
        self.emit(&[0xCA]);           // JP Z, solve_check_is
        self.fixup("solve_check_is");

        // Not a built-in, query database
        self.emit(&[0xC3]);           // JP query_db
        self.fixup("query_db");

        // Check if functor is exactly "write" (need to re-extract functor from HL)
        self.label("solve_check_write");
        // HL is original STR pointer, re-extract functor
        self.emit(&[0xE5]);           // PUSH HL (save STR pointer)
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xE6, 0x3F]);     // AND 0x3F (strip tag)
        self.emit(&[0x67]);           // LD H, A
        self.emit(&[0x5E]);           // LD E, (HL) (functor low)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x56]);           // LD D, (HL) (functor high)
        self.emit(&[0xEB]);           // EX DE, HL (HL = functor)
        // Check "write" (we know first char is 'w')
        self.emit(&[0x23]);           // INC HL (point to 'r')
        self.emit(&[0x7E]);           // LD A, (HL)
        self.emit(&[0xFE, b'r']);
        self.emit(&[0xC2]);           // JP NZ, solve_write_not_write
        self.fixup("solve_write_not_write");
        self.emit(&[0x23]);           // INC HL (point to 'i')
        self.emit(&[0x7E]);           // LD A, (HL)
        self.emit(&[0xFE, b'i']);
        self.emit(&[0xC2]);           // JP NZ, solve_write_not_write
        self.fixup("solve_write_not_write");
        self.emit(&[0x23]);           // INC HL (point to 't')
        self.emit(&[0x7E]);           // LD A, (HL)
        self.emit(&[0xFE, b't']);
        self.emit(&[0xC2]);           // JP NZ, solve_write_not_write
        self.fixup("solve_write_not_write");
        self.emit(&[0x23]);           // INC HL (point to 'e')
        self.emit(&[0x7E]);           // LD A, (HL)
        self.emit(&[0xFE, b'e']);
        self.emit(&[0xC2]);           // JP NZ, solve_write_not_write
        self.fixup("solve_write_not_write");
        self.emit(&[0x23]);           // INC HL (point to null)
        self.emit(&[0x7E]);           // LD A, (HL)
        self.emit(&[0xB7]);           // OR A
        self.emit(&[0xC2]);           // JP NZ, solve_write_not_write
        self.fixup("solve_write_not_write");
        // It's "write" - pop saved HL and execute
        self.emit(&[0xE1]);           // POP HL
        self.emit(&[0xC3]);           // JP solve_builtin_write
        self.fixup("solve_builtin_write");

        self.label("solve_write_not_write");
        // Not "write", restore HL and query database
        self.emit(&[0xE1]);           // POP HL
        self.emit(&[0x22]);           // LD (QUERY_TERM), HL
        self.emit_word(QUERY_TERM);
        self.emit(&[0xC3]);           // JP query_db
        self.fixup("query_db");

        // Check if functor is exactly "is" (similar approach)
        self.label("solve_check_is");
        // HL is original STR pointer, re-extract functor
        self.emit(&[0xE5]);           // PUSH HL
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xE6, 0x3F]);     // AND 0x3F
        self.emit(&[0x67]);           // LD H, A
        self.emit(&[0x5E]);           // LD E, (HL)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x56]);           // LD D, (HL)
        self.emit(&[0xEB]);           // EX DE, HL (HL = functor)
        // Check "is" (we know first char is 'i')
        self.emit(&[0x23]);           // INC HL (point to 's')
        self.emit(&[0x7E]);           // LD A, (HL)
        self.emit(&[0xFE, b's']);
        self.emit(&[0xC2]);           // JP NZ, solve_is_not_is
        self.fixup("solve_is_not_is");
        self.emit(&[0x23]);           // INC HL (point to null)
        self.emit(&[0x7E]);           // LD A, (HL)
        self.emit(&[0xB7]);           // OR A
        self.emit(&[0xC2]);           // JP NZ, solve_is_not_is
        self.fixup("solve_is_not_is");
        // It's "is" - pop saved HL and execute
        self.emit(&[0xE1]);           // POP HL
        self.emit(&[0xC3]);           // JP solve_builtin_is
        self.fixup("solve_builtin_is");

        self.label("solve_is_not_is");
        // Not "is", restore HL and query database
        self.emit(&[0xE1]);           // POP HL
        self.emit(&[0x22]);           // LD (QUERY_TERM), HL
        self.emit_word(QUERY_TERM);
        self.emit(&[0xC3]);           // JP query_db
        self.fixup("query_db");

        // Handle write(X) in solve context
        self.label("solve_builtin_write");
        // HL is STR-tagged pointer
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xE6, 0x3F]);     // AND 0x3F (strip tag)
        self.emit(&[0x67]);           // LD H, A
        // Skip functor (2 bytes) and arity (1 byte) to get first arg
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x23]);           // INC HL
        // Load arg
        self.emit(&[0x5E]);           // LD E, (HL)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x56]);           // LD D, (HL)
        self.emit(&[0xEB]);           // EX DE, HL (HL = arg)
        self.emit(&[0xC3]);           // JP builtin_write
        self.fixup("builtin_write");

        // Handle is(X, Expr) in solve context
        self.label("solve_builtin_is");
        // HL is STR-tagged pointer
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xE6, 0x3F]);     // AND 0x3F (strip tag)
        self.emit(&[0x67]);           // LD H, A
        // Skip functor (2 bytes) and arity (1 byte) to get first arg (X)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x23]);           // INC HL
        // Load X (first arg)
        self.emit(&[0x5E]);           // LD E, (HL)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x56]);           // LD D, (HL)
        self.emit(&[0xD5]);           // PUSH DE (save X)
        // Get Expr (second arg)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x5E]);           // LD E, (HL)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x56]);           // LD D, (HL)
        self.emit(&[0xEB]);           // EX DE, HL (HL = Expr)
        // Evaluate the expression
        self.emit(&[0xCD]);           // CALL eval_arith
        self.fixup("eval_arith");
        // HL = result (INT tagged)
        self.emit(&[0xEB]);           // EX DE, HL (DE = result)
        self.emit(&[0xE1]);           // POP HL (HL = X)
        // Unify X with result
        self.emit(&[0xCD]);           // CALL unify
        self.fixup("unify");
        // Carry set = fail, clear = success
        self.emit(&[0xC9]);           // RET

        // Setup environment frame - allocate fresh variables for clause invocation
        // Input: A = number of variables to allocate
        // Creates fresh heap vars and stores their addresses in ENV_FRAME
        // Preserves: BC
        self.label("setup_env_frame");
        self.emit(&[0xB7]);           // OR A
        self.emit(&[0xC8]);           // RET Z (no variables to allocate)
        self.emit(&[0xC5]);           // PUSH BC (save BC)
        self.emit(&[0x47]);           // LD B, A (B = var count)
        self.emit(&[0xDD, 0x21]);     // LD IX, ENV_FRAME
        self.emit_word(ENV_FRAME);
        self.label("env_alloc_loop");
        self.emit(&[0xC5]);           // PUSH BC (save counter)
        self.emit(&[0xDD, 0xE5]);     // PUSH IX (save frame pointer)
        self.emit(&[0xCD]);           // CALL heap_alloc_var
        self.fixup("heap_alloc_var");
        // HL = new var address
        self.emit(&[0xDD, 0xE1]);     // POP IX (restore frame pointer)
        self.emit(&[0xDD, 0x75, 0x00]); // LD (IX+0), L
        self.emit(&[0xDD, 0x74, 0x01]); // LD (IX+1), H
        self.emit(&[0xDD, 0x23]);     // INC IX
        self.emit(&[0xDD, 0x23]);     // INC IX
        self.emit(&[0xC1]);           // POP BC (restore counter)
        // DJNZ relative offset: back 21 bytes (1+2+3+2+3+3+2+2+1+2 = 21)
        self.emit(&[0x10, 0xEB]);     // DJNZ env_alloc_loop (-21 = 0xEB)
        self.emit(&[0xC1]);           // POP BC (restore original BC)
        self.emit(&[0xC9]);           // RET

        // Save ENV_FRAME to ENV_STACK (16 bytes)
        // Preserves all registers except DE (used internally)
        self.label("save_env_frame");
        self.emit(&[0xE5]);           // PUSH HL (save HL)
        self.emit(&[0xD5]);           // PUSH DE (save DE)
        // Get destination in env stack
        self.emit(&[0x2A]);           // LD HL, (ENV_STACK_PTR)
        self.emit_word(ENV_STACK_PTR);
        self.emit(&[0xEB]);           // EX DE, HL (DE = dest)
        // Source = ENV_FRAME
        self.emit(&[0x21]);           // LD HL, ENV_FRAME
        self.emit_word(ENV_FRAME);
        // Copy 16 bytes
        self.emit(&[0x01]);           // LD BC, 16
        self.emit_word(16);
        self.emit(&[0xED, 0xB0]);     // LDIR
        // Update ENV_STACK_PTR (DE now points past copied data)
        self.emit(&[0xEB]);           // EX DE, HL
        self.emit(&[0x22]);           // LD (ENV_STACK_PTR), HL
        self.emit_word(ENV_STACK_PTR);
        self.emit(&[0xD1]);           // POP DE (restore DE)
        self.emit(&[0xE1]);           // POP HL (restore HL)
        self.emit(&[0xC9]);           // RET

        // Restore ENV_FRAME from ENV_STACK (16 bytes)
        // Preserves all registers including carry flag
        self.label("restore_env_frame");
        self.emit(&[0xE5]);           // PUSH HL (save HL)
        self.emit(&[0xD5]);           // PUSH DE (save DE)
        self.emit(&[0xF5]);           // PUSH AF (save carry flag)
        // Move ENV_STACK_PTR back by 16 bytes
        self.emit(&[0x2A]);           // LD HL, (ENV_STACK_PTR)
        self.emit_word(ENV_STACK_PTR);
        self.emit(&[0x01]);           // LD BC, 16
        self.emit_word(16);
        self.emit(&[0xB7]);           // OR A (clear carry for SBC)
        self.emit(&[0xED, 0x42]);     // SBC HL, BC
        self.emit(&[0x22]);           // LD (ENV_STACK_PTR), HL
        self.emit_word(ENV_STACK_PTR);
        // Copy from env stack back to ENV_FRAME
        // HL = source (now points to saved data)
        self.emit(&[0x11]);           // LD DE, ENV_FRAME
        self.emit_word(ENV_FRAME);
        self.emit(&[0x01]);           // LD BC, 16
        self.emit_word(16);
        self.emit(&[0xED, 0xB0]);     // LDIR
        self.emit(&[0xF1]);           // POP AF (restore carry flag)
        self.emit(&[0xD1]);           // POP DE (restore DE)
        self.emit(&[0xE1]);           // POP HL (restore HL)
        self.emit(&[0xC9]);           // RET

        // Instantiate a goal term: resolve template vars to heap addresses
        // Input: HL = goal term (may contain template vars)
        // Output: HL = instantiated term (template vars resolved via ENV_FRAME)
        // Uses current ENV_FRAME to resolve template vars
        self.label("instantiate_goal");
        // Check tag
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xE6, 0xC0]);     // AND 0xC0
        // If not tag 0x00 (REF), check for STR
        self.emit(&[0xC2]);           // JP NZ, inst_check_str
        self.fixup("inst_check_str");
        // REF tag - could be template var or regular heap ref
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xFE, 0x3E]);     // CP 0x3E (template var marker)
        self.emit(&[0xC2]);           // JP NZ, inst_done (regular heap ref, leave as-is)
        self.fixup("inst_done");
        // Template var - resolve via ENV_FRAME
        self.emit(&[0x7D]);           // LD A, L (index)
        self.emit(&[0x87]);           // ADD A, A (index * 2)
        self.emit(&[0x5F]);           // LD E, A
        self.emit(&[0x16, 0x00]);     // LD D, 0
        self.emit(&[0x21]);           // LD HL, ENV_FRAME
        self.emit_word(ENV_FRAME);
        self.emit(&[0x19]);           // ADD HL, DE
        self.emit(&[0x5E]);           // LD E, (HL)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x56]);           // LD D, (HL)
        self.emit(&[0xEB]);           // EX DE, HL (HL = resolved heap address)
        self.emit(&[0xC9]);           // RET

        self.label("inst_check_str");
        self.emit(&[0xFE, tag::STR]); // CP STR tag
        self.emit(&[0xC2]);           // JP NZ, inst_done (ATM or INT, leave as-is)
        self.fixup("inst_done");

        // STR tag - compound term, need to copy with instantiated arguments
        // Strip tag to get structure address
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xE6, 0x3F]);     // AND 0x3F
        self.emit(&[0x67]);           // LD H, A
        // HL = original structure address

        // Read functor (2 bytes) and arity (1 byte)
        self.emit(&[0x4E]);           // LD C, (HL) functor low
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x46]);           // LD B, (HL) functor high
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x7E]);           // LD A, (HL) arity
        self.emit(&[0x23]);           // INC HL
        // HL = pointer to first arg, BC = functor, A = arity

        // Save source arg pointer
        self.emit(&[0x22]);           // LD (INST_SRC), HL
        self.emit_word(INST_SRC);

        // Save arg count
        self.emit(&[0x32]);           // LD (INST_COUNT), A
        self.emit_word(INST_COUNT);

        // Allocate new structure on heap - get base address first
        self.emit(&[0x2A]);           // LD HL, (SPM_H)
        self.emit_word(SPM_H);
        self.emit(&[0xE5]);           // PUSH HL (save new struct base addr)
        // Stack: [new_struct_addr]

        // Write functor to new struct (BC = functor from earlier)
        self.emit(&[0x71]);           // LD (HL), C (functor low)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x70]);           // LD (HL), B (functor high)
        self.emit(&[0x23]);           // INC HL

        // Write arity from INST_COUNT
        self.emit(&[0x3A]);           // LD A, (INST_COUNT)
        self.emit_word(INST_COUNT);
        self.emit(&[0x77]);           // LD (HL), A
        self.emit(&[0x23]);           // INC HL
        // Stack: [new_struct_addr]

        // Save dest arg pointer
        self.emit(&[0x22]);           // LD (INST_DST), HL
        self.emit_word(INST_DST);

        // IMPORTANT: Update SPM_H to point past entire structure BEFORE processing args
        // This ensures recursive calls for nested compounds allocate beyond this structure
        // HL currently points to first arg slot, need to add 2*arity bytes
        self.emit(&[0x3A]);           // LD A, (INST_COUNT)
        self.emit_word(INST_COUNT);
        self.emit(&[0x87]);           // ADD A, A (A = 2*arity)
        self.emit(&[0x5F]);           // LD E, A
        self.emit(&[0x16, 0x00]);     // LD D, 0
        self.emit(&[0x19]);           // ADD HL, DE (HL = end of structure)
        self.emit(&[0x22]);           // LD (SPM_H), HL (reserve space)
        self.emit_word(SPM_H);

        // Check if any args to copy
        self.emit(&[0x3A]);           // LD A, (INST_COUNT)
        self.emit_word(INST_COUNT);
        self.emit(&[0xB7]);           // OR A
        self.emit(&[0xCA]);           // JP Z, inst_str_done (no args)
        self.fixup("inst_str_done");

        // Loop: instantiate each arg
        self.label("inst_arg_loop");
        // Read arg from source
        self.emit(&[0x2A]);           // LD HL, (INST_SRC)
        self.emit_word(INST_SRC);
        self.emit(&[0x5E]);           // LD E, (HL)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x56]);           // LD D, (HL)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x22]);           // LD (INST_SRC), HL (advance src)
        self.emit_word(INST_SRC);

        // Save INST_* state before recursive call (for nested compounds)
        self.emit(&[0x2A]);           // LD HL, (INST_SRC)
        self.emit_word(INST_SRC);
        self.emit(&[0xE5]);           // PUSH HL
        self.emit(&[0x2A]);           // LD HL, (INST_DST)
        self.emit_word(INST_DST);
        self.emit(&[0xE5]);           // PUSH HL
        self.emit(&[0x3A]);           // LD A, (INST_COUNT)
        self.emit_word(INST_COUNT);
        self.emit(&[0x6F]);           // LD L, A
        self.emit(&[0x26, 0x00]);     // LD H, 0
        self.emit(&[0xE5]);           // PUSH HL

        // DE = arg value, call instantiate_goal recursively
        self.emit(&[0xEB]);           // EX DE, HL (HL = arg)
        self.emit(&[0xCD]);           // CALL instantiate_goal
        self.fixup("instantiate_goal");
        // HL = instantiated arg

        // Restore INST_* state after recursive call
        self.emit(&[0xEB]);           // EX DE, HL (DE = instantiated arg)
        self.emit(&[0xE1]);           // POP HL
        self.emit(&[0x7D]);           // LD A, L
        self.emit(&[0x32]);           // LD (INST_COUNT), A
        self.emit_word(INST_COUNT);
        self.emit(&[0xE1]);           // POP HL
        self.emit(&[0x22]);           // LD (INST_DST), HL
        self.emit_word(INST_DST);
        self.emit(&[0xE1]);           // POP HL
        self.emit(&[0x22]);           // LD (INST_SRC), HL
        self.emit_word(INST_SRC);
        // DE has instantiated arg

        // Write to dest (DE has instantiated arg already)
        self.emit(&[0x2A]);           // LD HL, (INST_DST)
        self.emit_word(INST_DST);
        self.emit(&[0x73]);           // LD (HL), E
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x72]);           // LD (HL), D
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x22]);           // LD (INST_DST), HL (advance dst)
        self.emit_word(INST_DST);

        // Decrement count
        self.emit(&[0x3A]);           // LD A, (INST_COUNT)
        self.emit_word(INST_COUNT);
        self.emit(&[0x3D]);           // DEC A
        self.emit(&[0x32]);           // LD (INST_COUNT), A
        self.emit_word(INST_COUNT);
        self.emit(&[0xC2]);           // JP NZ, inst_arg_loop
        self.fixup("inst_arg_loop");

        self.label("inst_str_done");
        // SPM_H was already updated during pre-allocation, no need to update again
        // (and doing so would be incorrect after recursive calls for nested compounds)

        // Return STR-tagged pointer to new structure
        self.emit(&[0xE1]);           // POP HL (new struct base addr)
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xF6, tag::STR]); // OR STR tag
        self.emit(&[0x67]);           // LD H, A
        self.emit(&[0xC9]);           // RET

        self.label("inst_done");
        self.emit(&[0xC9]);           // RET (ATM/INT/heap REF - return unchanged)
    }

    /// Built-in predicates
    fn emit_builtins(&mut self) {
        // print_term - print a term based on its type
        // Input: HL = term
        self.label("print_term");
        // First dereference in case it's a bound variable
        self.emit(&[0xCD]);           // CALL deref
        self.fixup("deref");
        // Check tag
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xE6, 0xC0]);     // AND 0xC0
        // REF (0x00) - unbound variable
        self.emit(&[0xCA]);           // JP Z, print_var
        self.fixup("print_var");
        // INT (0x40)
        self.emit(&[0xFE, tag::INT]);
        self.emit(&[0xCA]);           // JP Z, print_term_int
        self.fixup("print_term_int");
        // ATM (0x80)
        self.emit(&[0xFE, tag::ATM]);
        self.emit(&[0xCA]);           // JP Z, print_term_atom
        self.fixup("print_term_atom");
        // STR (0xC0) - compound term
        self.emit(&[0xC3]);           // JP print_term_struct
        self.fixup("print_term_struct");

        // Print unbound variable
        self.label("print_var");
        self.emit(&[0x3E, b'_']);
        self.emit(&[0xCD]);
        self.fixup("putchar");
        // Print address as hex
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xCD]);
        self.fixup("print_hex_byte");
        self.emit(&[0x7D]);           // LD A, L
        self.emit(&[0xCD]);
        self.fixup("print_hex_byte");
        self.emit(&[0xC9]);           // RET

        // Print integer
        self.label("print_term_int");
        // Strip tag, get 14-bit value
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xE6, 0x3F]);     // AND 0x3F
        self.emit(&[0x67]);           // LD H, A
        self.emit(&[0xC3]);           // JP print_int
        self.fixup("print_int");

        // Print atom - HL is pointer to null-terminated string in atom table
        self.label("print_term_atom");
        // Strip tag to get raw pointer
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xE6, 0x3F]);     // AND 0x3F
        self.emit(&[0x67]);           // LD H, A
        self.emit(&[0xC3]);           // JP print_string
        self.fixup("print_string");

        // Print compound term
        self.label("print_term_struct");
        // Strip tag to get structure address
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xE6, 0x3F]);     // AND 0x3F
        self.emit(&[0x67]);           // LD H, A
        // Print functor (pointer at HL points to atom string)
        self.emit(&[0xE5]);           // PUSH HL (save structure address)
        self.emit(&[0x5E]);           // LD E, (HL)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x56]);           // LD D, (HL)
        self.emit(&[0xEB]);           // EX DE, HL (HL = functor pointer)
        self.emit(&[0xCD]);           // CALL print_string
        self.fixup("print_string");
        self.emit(&[0xE1]);           // POP HL (restore structure address)
        self.emit(&[0x3E, b'(']);     // LD A, '('
        self.emit(&[0xCD]);
        self.fixup("putchar");
        // Get arity
        self.emit(&[0x23]);           // INC HL (skip functor low)
        self.emit(&[0x23]);           // INC HL (skip functor high)
        self.emit(&[0x46]);           // LD B, (HL) (arity)
        self.emit(&[0x23]);           // INC HL (point to first arg)
        // Print args
        self.label("print_args_loop");
        self.emit(&[0x78]);           // LD A, B
        self.emit(&[0xB7]);           // OR A
        self.emit(&[0xCA]);           // JP Z, print_args_done
        self.fixup("print_args_done");
        // Save state
        self.emit(&[0xC5]);           // PUSH BC
        self.emit(&[0xE5]);           // PUSH HL
        // Load and print arg
        self.emit(&[0x5E]);           // LD E, (HL)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x56]);           // LD D, (HL)
        self.emit(&[0xEB]);           // EX DE, HL
        self.emit(&[0xCD]);           // CALL print_term (recursive)
        self.fixup("print_term");
        // Restore state
        self.emit(&[0xE1]);           // POP HL
        self.emit(&[0xC1]);           // POP BC
        self.emit(&[0x23]);           // INC HL (skip arg low)
        self.emit(&[0x23]);           // INC HL (skip arg high)
        self.emit(&[0x05]);           // DEC B
        // Print comma if more args
        self.emit(&[0xCA]);           // JP Z, print_args_done
        self.fixup("print_args_done");
        self.emit(&[0x3E, b',']);
        self.emit(&[0xCD]);
        self.fixup("putchar");
        self.emit(&[0xC3]);           // JP print_args_loop
        self.fixup("print_args_loop");

        self.label("print_args_done");
        self.emit(&[0x3E, b')']);
        self.emit(&[0xCD]);
        self.fixup("putchar");
        self.emit(&[0xC9]);           // RET

        // Print A as hex byte
        self.label("print_hex_byte");
        self.emit(&[0xF5]);           // PUSH AF
        self.emit(&[0x0F]);           // RRCA
        self.emit(&[0x0F]);           // RRCA
        self.emit(&[0x0F]);           // RRCA
        self.emit(&[0x0F]);           // RRCA
        self.emit(&[0xCD]);           // CALL print_hex_nibble
        self.fixup("print_hex_nibble");
        self.emit(&[0xF1]);           // POP AF
        // Fall through to print low nibble

        self.label("print_hex_nibble");
        self.emit(&[0xE6, 0x0F]);     // AND 0x0F
        self.emit(&[0xC6, b'0']);     // ADD '0'
        self.emit(&[0xFE, b'9' + 1]); // CP '9'+1
        self.emit(&[0xDA]);           // JP C, print_hex_digit
        self.fixup("print_hex_digit");
        self.emit(&[0xC6, 7]);        // ADD 7 ('A'-'9'-1)
        self.label("print_hex_digit");
        self.emit(&[0xCD]);
        self.fixup("putchar");
        self.emit(&[0xC9]);

        // write/1 - succeeds always after printing
        // Input: HL = term to print
        self.label("builtin_write");
        self.emit(&[0xCD]);           // CALL print_term
        self.fixup("print_term");
        self.emit(&[0xA7]);           // AND A (clear carry = success)
        self.emit(&[0xC9]);           // RET

        // nl/0 - print newline and succeed
        self.label("builtin_nl");
        self.emit(&[0xCD]);           // CALL print_nl
        self.fixup("print_nl");
        self.emit(&[0xA7]);           // AND A (clear carry = success)
        self.emit(&[0xC9]);           // RET

        // true/0 - always succeeds
        self.label("builtin_true");
        self.emit(&[0xA7]);           // AND A (clear carry = success)
        self.emit(&[0xC9]);           // RET

        // fail/0 - always fails
        self.label("builtin_fail");
        self.emit(&[0x37]);           // SCF (set carry = fail)
        self.emit(&[0xC9]);           // RET

        // ================================================================
        // Arithmetic Evaluation
        // ================================================================

        // Evaluate an arithmetic expression
        // Input: HL = term (INT, REF, or STR compound)
        // Output: HL = result (INT tagged)
        self.label("eval_arith");
        // First dereference
        self.emit(&[0xCD]);           // CALL deref
        self.fixup("deref");
        // Check tag
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xE6, 0xC0]);     // AND 0xC0

        // INT (0x40)? Return as-is
        self.emit(&[0xFE, tag::INT]);
        self.emit(&[0xC8]);           // RET Z

        // REF (0x00)? Unbound variable - return 0
        self.emit(&[0xB7]);           // OR A
        self.emit(&[0xC2]);           // JP NZ, eval_arith_compound
        self.fixup("eval_arith_compound");
        self.emit(&[0x21]);
        self.emit_word(tag::INT as u16);
        self.emit(&[0xC9]);           // RET

        // STR (0xC0) - compound term (binary operator)
        // Layout: functor(2), arity(1), arg1(2), arg2(2)
        // Functor points to single-char atom like "+", "-", etc.
        self.label("eval_arith_compound");
        // IMPORTANT: Save IX because we'll use it for stack indexing
        self.emit(&[0xDD, 0xE5]);     // PUSH IX
        // Strip tag to get structure address
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xE6, 0x3F]);
        self.emit(&[0x67]);           // LD H, A
        // HL = struct base

        // Get operator char and save to IYL (preserved across calls)
        self.emit(&[0x5E]);           // LD E, (HL) - functor low
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x56]);           // LD D, (HL) - functor high
        self.emit(&[0x2B]);           // DEC HL - back to start
        // DE = functor atom pointer, get first char
        self.emit(&[0x1A]);           // LD A, (DE) - operator char
        self.emit(&[0xFD, 0x6F]);     // LD IYL, A - save operator in IYL

        // Save struct base on stack
        self.emit(&[0xE5]);           // PUSH HL

        // Get left operand (at offset 3)
        self.emit(&[0x23]);           // INC HL (skip functor low)
        self.emit(&[0x23]);           // INC HL (skip functor high)
        self.emit(&[0x23]);           // INC HL (skip arity)
        self.emit(&[0x5E]);           // LD E, (HL)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x56]);           // LD D, (HL)
        self.emit(&[0xEB]);           // EX DE, HL (HL = left operand)

        // Save IY before recursive call (for nested expressions)
        self.emit(&[0xFD, 0xE5]);     // PUSH IY
        // Evaluate left operand
        self.emit(&[0xCD]);           // CALL eval_arith (recursive)
        self.fixup("eval_arith");
        // HL = left result (INT tagged)
        // Restore IY
        self.emit(&[0xFD, 0xE1]);     // POP IY

        // Save left result on stack
        self.emit(&[0xE5]);           // PUSH HL
        // Stack: [left_result(2), struct_base(2), saved_IX(2)]

        // Get right operand (at offset 5 from struct base)
        // Retrieve struct base from stack using IX temporarily
        self.emit(&[0xDD, 0x21]);     // LD IX, 0
        self.emit_word(0);
        self.emit(&[0xDD, 0x39]);     // ADD IX, SP
        // [IX+0,1] = left_result, [IX+2,3] = struct_base, [IX+4,5] = saved_IX
        self.emit(&[0xDD, 0x6E, 0x02]); // LD L, (IX+2)
        self.emit(&[0xDD, 0x66, 0x03]); // LD H, (IX+3)
        // HL = struct_base, add 5 for right operand
        self.emit(&[0x11]);           // LD DE, 5
        self.emit_word(5);
        self.emit(&[0x19]);           // ADD HL, DE
        self.emit(&[0x5E]);           // LD E, (HL)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x56]);           // LD D, (HL)
        self.emit(&[0xEB]);           // EX DE, HL (HL = right operand)

        // Save IY before recursive call (for nested expressions)
        self.emit(&[0xFD, 0xE5]);     // PUSH IY
        // Evaluate right operand
        self.emit(&[0xCD]);           // CALL eval_arith (recursive)
        self.fixup("eval_arith");
        // HL = right result (INT tagged)
        // Restore IY
        self.emit(&[0xFD, 0xE1]);     // POP IY

        // Now: HL = right (INT tagged), IYL = operator
        // Stack: [left_result(2), struct_base(2), saved_IX(2)]
        self.emit(&[0xEB]);           // EX DE, HL (DE = right)
        self.emit(&[0xE1]);           // POP HL (left result)
        self.emit(&[0xF1]);           // POP AF (discard struct_base)
        self.emit(&[0xDD, 0xE1]);     // POP IX (restore saved IX)
        // Now: HL = left (INT tagged), DE = right (INT tagged), IYL = operator

        // Strip INT tags from HL and DE
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xE6, 0x3F]);     // AND 0x3F
        self.emit(&[0x67]);           // LD H, A (left value)
        self.emit(&[0x7A]);           // LD A, D
        self.emit(&[0xE6, 0x3F]);     // AND 0x3F
        self.emit(&[0x57]);           // LD D, A (right value)
        // HL = left value (14-bit), DE = right value (14-bit)

        // Get operator from IYL
        self.emit(&[0xFD, 0x7D]);     // LD A, IYL

        // Dispatch on operator
        // Check '+'
        self.emit(&[0xFE, b'+']);
        self.emit(&[0xCA]);           // JP Z, arith_add
        self.fixup("arith_add");

        // Check '-'
        self.emit(&[0xFE, b'-']);
        self.emit(&[0xCA]);           // JP Z, arith_sub
        self.fixup("arith_sub");

        // Check '*'
        self.emit(&[0xFE, b'*']);
        self.emit(&[0xCA]);           // JP Z, arith_mul
        self.fixup("arith_mul");

        // Check '/'
        self.emit(&[0xFE, b'/']);
        self.emit(&[0xCA]);           // JP Z, arith_div
        self.fixup("arith_div");

        // Check 'm' (mod)
        self.emit(&[0xFE, b'm']);
        self.emit(&[0xCA]);           // JP Z, arith_mod
        self.fixup("arith_mod");

        // Unknown operator - return 0
        self.emit(&[0x21]);           // LD HL, 0 | INT
        self.emit_word(tag::INT as u16);
        self.emit(&[0xC9]);           // RET

        // ADD: HL = HL + DE
        self.label("arith_add");
        self.emit(&[0x19]);           // ADD HL, DE
        self.emit(&[0xC3]);           // JP arith_tag_result
        self.fixup("arith_tag_result");

        // SUB: HL = HL - DE
        self.label("arith_sub");
        self.emit(&[0xB7]);           // OR A (clear carry)
        self.emit(&[0xED, 0x52]);     // SBC HL, DE
        self.emit(&[0xC3]);           // JP arith_tag_result
        self.fixup("arith_tag_result");

        // MUL: HL = HL * DE (16-bit multiply via shift-and-add)
        self.label("arith_mul");
        self.emit(&[0x44]);           // LD B, H
        self.emit(&[0x4D]);           // LD C, L (BC = multiplicand)
        self.emit(&[0x21]);           // LD HL, 0 (accumulator)
        self.emit_word(0);
        self.label("arith_mul_loop");
        self.emit(&[0x7A]);           // LD A, D
        self.emit(&[0xB3]);           // OR E
        self.emit(&[0xCA]);           // JP Z, arith_tag_result (done when DE=0)
        self.fixup("arith_tag_result");
        self.emit(&[0xCB, 0x3A]);     // SRL D
        self.emit(&[0xCB, 0x1B]);     // RR E (DE >>= 1, carry = LSB)
        self.emit(&[0x30, 0x01]);     // JR NC, +1 (skip add if bit was 0)
        self.emit(&[0x09]);           // ADD HL, BC
        self.emit(&[0xCB, 0x21]);     // SLA C
        self.emit(&[0xCB, 0x10]);     // RL B (BC <<= 1)
        self.emit(&[0xC3]);           // JP arith_mul_loop
        self.fixup("arith_mul_loop");

        // DIV: HL = HL / DE (16-bit divide via repeated subtraction)
        self.label("arith_div");
        // Check for divide by zero
        self.emit(&[0x7A]);           // LD A, D
        self.emit(&[0xB3]);           // OR E
        self.emit(&[0xCA]);           // JP Z, arith_div_zero
        self.fixup("arith_div_zero");
        // BC = quotient counter
        self.emit(&[0x01]);           // LD BC, 0
        self.emit_word(0);
        self.label("arith_div_loop");
        self.emit(&[0xB7]);           // OR A (clear carry)
        self.emit(&[0xED, 0x52]);     // SBC HL, DE
        self.emit(&[0x38, 0x04]);     // JR C, arith_div_done (HL went negative)
        self.emit(&[0x03]);           // INC BC
        self.emit(&[0xC3]);           // JP arith_div_loop
        self.fixup("arith_div_loop");
        self.label("arith_div_done");
        self.emit(&[0x60]);           // LD H, B
        self.emit(&[0x69]);           // LD L, C
        self.emit(&[0xC3]);           // JP arith_tag_result
        self.fixup("arith_tag_result");
        // Divide by zero - return 0
        self.label("arith_div_zero");
        self.emit(&[0x21]);           // LD HL, 0
        self.emit_word(0);
        self.emit(&[0xC3]);           // JP arith_tag_result
        self.fixup("arith_tag_result");

        // MOD: HL = HL mod DE (remainder via repeated subtraction)
        self.label("arith_mod");
        // Check for mod by zero
        self.emit(&[0x7A]);           // LD A, D
        self.emit(&[0xB3]);           // OR E
        self.emit(&[0xCA]);           // JP Z, arith_tag_result (return HL as-is if zero divisor)
        self.fixup("arith_tag_result");
        self.label("arith_mod_loop");
        self.emit(&[0xB7]);           // OR A (clear carry)
        self.emit(&[0xED, 0x52]);     // SBC HL, DE
        self.emit(&[0xD2]);           // JP NC, arith_mod_loop (continue while HL >= 0)
        self.fixup("arith_mod_loop");
        // HL went negative, add DE back to get remainder
        self.emit(&[0x19]);           // ADD HL, DE
        self.emit(&[0xC3]);           // JP arith_tag_result
        self.fixup("arith_tag_result");

        // Tag result with INT
        self.label("arith_tag_result");
        // HL = result value, need to add INT tag (0x40 in high bits)
        // But only if result fits in 14 bits
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xE6, 0xC0]);     // AND 0xC0 (check high 2 bits)
        self.emit(&[0x20, 0x03]);     // JR NZ, arith_overflow (result too big)
        self.emit(&[0xCB, 0xF4]);     // SET 6, H (add INT tag)
        self.emit(&[0xC9]);           // RET
        // Overflow - clamp to max (0x3FFF with INT tag = 0x7FFF)
        self.label("arith_overflow");
        self.emit(&[0x21]);           // LD HL, 0x7FFF
        self.emit_word(0x7FFF);
        self.emit(&[0xC9]);           // RET
    }

    /// Main REPL loop
    fn emit_repl_loop(&mut self) {
        self.label("repl_loop");
        // Print prompt
        self.emit(&[0x21]);           // LD HL, prompt_str
        self.fixup("prompt_str");
        self.emit(&[0xCD]);           // CALL print_string
        self.fixup("print_string");

        // Read line
        self.emit(&[0xCD]);           // CALL read_line
        self.fixup("read_line");

        // Initialize lexer
        self.emit(&[0xCD]);           // CALL lex_init
        self.fixup("lex_init");

        // DON'T reset heap for facts - we want them to persist
        // Trail is managed in query handling (unwind + reset before each unification attempt)

        // Parse first token
        self.emit(&[0xCD]);           // CALL lex_next
        self.fixup("lex_next");

        // Check for empty input
        self.emit(&[0xFE, tok::EOF]); // CP EOF
        self.emit(&[0xCA]);           // JP Z, repl_loop (if empty, just loop)
        self.fixup("repl_loop");

        // Check for ?- (query)
        self.emit(&[0xFE, tok::QUERY]); // CP QUERY
        self.emit(&[0xCA]);           // JP Z, repl_query
        self.fixup("repl_query");

        // Otherwise treat as fact or rule assertion
        // Need to re-initialize lexer since we consumed a token
        self.emit(&[0xCD]);           // CALL lex_init
        self.fixup("lex_init");
        // Set parse mode = 1 (template variables for clauses)
        self.emit(&[0x3E, 0x01]);     // LD A, 1
        self.emit(&[0x32]);           // LD (PARSE_MODE), A
        self.emit_word(PARSE_MODE);
        // Clear variable table for this clause
        self.emit(&[0xCD]);           // CALL var_table_clear
        self.fixup("var_table_clear");
        // Parse the head term
        self.emit(&[0xCD]);           // CALL parse_term
        self.fixup("parse_term");
        // HL now has the parsed head term
        self.emit(&[0xE5]);           // PUSH HL (save head)

        // Check for :- (rule) or . (fact)
        self.emit(&[0xCD]);           // CALL lex_next
        self.fixup("lex_next");
        self.emit(&[0xFE, tok::NECK]); // CP NECK (:-)
        self.emit(&[0xCA]);           // JP Z, parse_rule
        self.fixup("parse_rule");

        // It's a fact (no body)
        self.emit(&[0xE1]);           // POP HL (head)
        self.emit(&[0xCD]);           // CALL assert_fact
        self.fixup("assert_fact");
        self.emit(&[0xC3]);           // JP repl_assert_done
        self.fixup("repl_assert_done");

        // Parse a rule: head :- body1, body2, ...
        self.label("parse_rule");
        // Stack: [head]
        // Parse body goals directly into SCRATCH area
        // Use SCRATCH_POS memory variable instead of IX (IX is used by lexer!)
        self.emit(&[0x21]);           // LD HL, SCRATCH
        self.emit_word(SCRATCH);
        self.emit(&[0x22]);           // LD (SCRATCH_POS), HL
        self.emit_word(SCRATCH_POS);
        self.emit(&[0x06, 0x00]);     // LD B, 0 (body count)

        self.label("parse_rule_body");
        self.emit(&[0xC5]);           // PUSH BC (save body count)
        self.emit(&[0xCD]);           // CALL parse_goal
        self.fixup("parse_goal");
        // HL = body goal
        self.emit(&[0xC1]);           // POP BC (restore body count)
        // Store body goal at (SCRATCH_POS)
        self.emit(&[0xD5]);           // PUSH DE (save DE)
        self.emit(&[0xEB]);           // EX DE, HL (DE = body goal)
        self.emit(&[0x2A]);           // LD HL, (SCRATCH_POS)
        self.emit_word(SCRATCH_POS);
        self.emit(&[0x73]);           // LD (HL), E (store low byte)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x72]);           // LD (HL), D (store high byte)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x22]);           // LD (SCRATCH_POS), HL (update position)
        self.emit_word(SCRATCH_POS);
        self.emit(&[0xD1]);           // POP DE (restore DE)
        self.emit(&[0x04]);           // INC B (body_count++)
        // Check for comma (more goals) or end
        self.emit(&[0xC5]);           // PUSH BC
        self.emit(&[0xCD]);           // CALL lex_next
        self.fixup("lex_next");
        self.emit(&[0xC1]);           // POP BC
        self.emit(&[0xFE, tok::COMMA]); // CP COMMA
        self.emit(&[0xCA]);           // JP Z, parse_rule_body
        self.fixup("parse_rule_body");

        // End of body - goals are stored in SCRATCH in order [body1, body2, ..., bodyn]
        // B = body_count
        // Pop head from stack
        self.emit(&[0xE1]);           // POP HL (head term)
        // Call assert_rule with HL = head, B = body_count, body goals in SCRATCH
        self.emit(&[0xCD]);           // CALL assert_rule
        self.fixup("assert_rule");

        self.label("repl_assert_done");
        // Print "ok"
        self.emit(&[0x21]);           // LD HL, ok_str
        self.fixup("ok_str");
        self.emit(&[0xCD]);           // CALL print_string
        self.fixup("print_string");
        self.emit(&[0xC3]);           // JP repl_loop
        self.fixup("repl_loop");

        // Handle query - may be a conjunction (goal1, goal2, ...)
        self.label("repl_query");
        // Reset state for new query:
        // 1. Clear choice point stack
        self.emit(&[0x21]);           // LD HL, 0
        self.emit_word(0x0000);
        self.emit(&[0x22]);           // LD (SPM_B), HL
        self.emit_word(SPM_B);
        // 2. Unwind trail to reset variable bindings from previous query
        self.emit(&[0x11]);           // LD DE, TRAIL_START (target)
        self.emit_word(TRAIL_START);
        self.emit(&[0xCD]);           // CALL unwind_trail
        self.fixup("unwind_trail");
        // 3. Reset trail pointer
        self.emit(&[0x21]);           // LD HL, TRAIL_START
        self.emit_word(TRAIL_START);
        self.emit(&[0x22]);           // LD (SPM_TR), HL
        self.emit_word(SPM_TR);
        // 4. DON'T reset heap - clause database references heap terms!
        // The heap will grow with each query, but that's OK for a REPL session
        // 5. Set parse mode = 0 (heap variables for queries)
        self.emit(&[0xAF]);           // XOR A (A = 0)
        self.emit(&[0x32]);           // LD (PARSE_MODE), A
        self.emit_word(PARSE_MODE);
        // 6. Clear variable table for this query
        self.emit(&[0xCD]);           // CALL var_table_clear
        self.fixup("var_table_clear");
        // Initialize SCRATCH_POS for storing query goals
        self.emit(&[0x21]);           // LD HL, SCRATCH
        self.emit_word(SCRATCH);
        self.emit(&[0x22]);           // LD (SCRATCH_POS), HL
        self.emit_word(SCRATCH_POS);
        self.emit(&[0x06, 0x00]);     // LD B, 0 (goal count)

        // Parse first query goal (can be term or "X is Expr")
        self.label("parse_query_goal");
        self.emit(&[0xC5]);           // PUSH BC (save goal count)
        self.emit(&[0xCD]);           // CALL parse_goal
        self.fixup("parse_goal");
        // HL = parsed goal
        self.emit(&[0xC1]);           // POP BC (restore goal count)
        // Store goal at (SCRATCH_POS)
        self.emit(&[0xD5]);           // PUSH DE (save DE)
        self.emit(&[0xEB]);           // EX DE, HL (DE = goal)
        self.emit(&[0x2A]);           // LD HL, (SCRATCH_POS)
        self.emit_word(SCRATCH_POS);
        self.emit(&[0x73]);           // LD (HL), E
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x72]);           // LD (HL), D
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x22]);           // LD (SCRATCH_POS), HL
        self.emit_word(SCRATCH_POS);
        self.emit(&[0xD1]);           // POP DE
        self.emit(&[0x04]);           // INC B (goal_count++)
        // Check for comma (more goals)
        self.emit(&[0xC5]);           // PUSH BC
        self.emit(&[0xCD]);           // CALL lex_next
        self.fixup("lex_next");
        self.emit(&[0xC1]);           // POP BC
        self.emit(&[0xFE, tok::COMMA]); // CP COMMA
        self.emit(&[0xCA]);           // JP Z, parse_query_goal
        self.fixup("parse_query_goal");

        // All query goals parsed, now solve them
        // B = goal count, goals are in SCRATCH
        // If single goal, check for built-in first
        self.emit(&[0x78]);           // LD A, B
        self.emit(&[0xFE, 0x01]);     // CP 1
        self.emit(&[0xC2]);           // JP NZ, solve_query_goals (multiple goals)
        self.fixup("solve_query_goals");

        // Single goal - check for built-in first
        self.emit(&[0x21]);           // LD HL, SCRATCH (address, not contents!)
        self.emit_word(SCRATCH);
        self.emit(&[0x5E]);           // LD E, (HL)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x56]);           // LD D, (HL)
        self.emit(&[0xEB]);           // EX DE, HL (HL = goal)
        self.emit(&[0xE5]);           // PUSH HL (save for query_db fallback)

        // Check tag to determine type
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xE6, 0xC0]);     // AND 0xC0

        // Is it a simple atom (ATM)?
        self.emit(&[0xFE, tag::ATM]); // CP ATM
        self.emit(&[0xCA]);           // JP Z, check_builtin_atom
        self.fixup("check_builtin_atom");

        // Is it a compound term (STR)?
        self.emit(&[0xFE, tag::STR]); // CP STR
        self.emit(&[0xCA]);           // JP Z, check_builtin_compound
        self.fixup("check_builtin_compound");

        // Not a built-in, query the database
        self.emit(&[0xC3]);           // JP do_query_db
        self.fixup("do_query_db");

        // Solve multiple query goals
        self.label("solve_query_goals");
        self.emit(&[0x21]);           // LD HL, SCRATCH
        self.emit_word(SCRATCH);
        // B already has goal count
        self.label("solve_query_loop");
        self.emit(&[0xC5]);           // PUSH BC (save count)
        self.emit(&[0xE5]);           // PUSH HL (save goal ptr)
        self.emit(&[0x5E]);           // LD E, (HL)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x56]);           // LD D, (HL)
        self.emit(&[0xEB]);           // EX DE, HL (HL = goal)
        self.emit(&[0xCD]);           // CALL solve_goal
        self.fixup("solve_goal");
        self.emit(&[0xE1]);           // POP HL (goal ptr)
        self.emit(&[0xC1]);           // POP BC
        // If goal failed, query fails
        self.emit(&[0xDA]);           // JP C, repl_query_no
        self.fixup("repl_query_no");
        // Advance to next goal
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x05]);           // DEC B
        self.emit(&[0xC2]);           // JP NZ, solve_query_loop
        self.fixup("solve_query_loop");
        // All goals succeeded
        self.emit(&[0xC3]);           // JP repl_query_yes
        self.fixup("repl_query_yes");

        self.label("repl_query_yes");
        self.emit(&[0x21]);           // LD HL, yes_str
        self.fixup("yes_str");
        self.emit(&[0xCD]);           // CALL print_string
        self.fixup("print_string");
        self.emit(&[0xC3]);           // JP repl_loop
        self.fixup("repl_loop");

        // Check for built-in atoms: nl, true, fail
        // Must check FULL atom name, not just first char (e.g., 'foo' vs 'fail')
        self.label("check_builtin_atom");
        // HL is atom pointer (with ATM tag), strip tag and get string pointer
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xE6, 0x3F]);     // AND 0x3F (strip tag)
        self.emit(&[0x67]);           // LD H, A
        // Now HL points to the null-terminated atom string
        // Save HL for later checks
        self.emit(&[0xE5]);           // PUSH HL

        // Check for "nl" (n,l,\0)
        self.emit(&[0x7E]);           // LD A, (HL) - first char
        self.emit(&[0xFE, b'n']);
        self.emit(&[0xC2]);           // JP NZ, check_atom_true
        self.fixup("check_atom_true");
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x7E]);           // LD A, (HL) - second char
        self.emit(&[0xFE, b'l']);
        self.emit(&[0xC2]);           // JP NZ, check_atom_true
        self.fixup("check_atom_true");
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x7E]);           // LD A, (HL) - third char (should be null)
        self.emit(&[0xB7]);           // OR A
        self.emit(&[0xC2]);           // JP NZ, check_atom_true
        self.fixup("check_atom_true");
        // It's "nl"
        self.emit(&[0xE1]);           // POP HL (discard saved pointer)
        self.emit(&[0xC3]);           // JP exec_builtin_nl
        self.fixup("exec_builtin_nl");

        // Check for "true" (t,r,u,e,\0)
        self.label("check_atom_true");
        self.emit(&[0xE1]);           // POP HL (restore atom pointer)
        self.emit(&[0xE5]);           // PUSH HL (save again for next check)
        self.emit(&[0x7E]);           // LD A, (HL) - first char
        self.emit(&[0xFE, b't']);
        self.emit(&[0xC2]);           // JP NZ, check_atom_fail
        self.fixup("check_atom_fail");
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x7E]);           // LD A, (HL) - second char
        self.emit(&[0xFE, b'r']);
        self.emit(&[0xC2]);           // JP NZ, check_atom_fail
        self.fixup("check_atom_fail");
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x7E]);           // LD A, (HL) - third char
        self.emit(&[0xFE, b'u']);
        self.emit(&[0xC2]);           // JP NZ, check_atom_fail
        self.fixup("check_atom_fail");
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x7E]);           // LD A, (HL) - fourth char
        self.emit(&[0xFE, b'e']);
        self.emit(&[0xC2]);           // JP NZ, check_atom_fail
        self.fixup("check_atom_fail");
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x7E]);           // LD A, (HL) - fifth char (should be null)
        self.emit(&[0xB7]);           // OR A
        self.emit(&[0xC2]);           // JP NZ, check_atom_fail
        self.fixup("check_atom_fail");
        // It's "true"
        self.emit(&[0xE1]);           // POP HL (discard saved pointer)
        self.emit(&[0xC3]);           // JP exec_builtin_true
        self.fixup("exec_builtin_true");

        // Check for "fail" (f,a,i,l,\0)
        self.label("check_atom_fail");
        self.emit(&[0xE1]);           // POP HL (restore atom pointer)
        self.emit(&[0xE5]);           // PUSH HL (save for query_db)
        self.emit(&[0x7E]);           // LD A, (HL) - first char
        self.emit(&[0xFE, b'f']);
        self.emit(&[0xC2]);           // JP NZ, check_atom_not_builtin
        self.fixup("check_atom_not_builtin");
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x7E]);           // LD A, (HL) - second char
        self.emit(&[0xFE, b'a']);
        self.emit(&[0xC2]);           // JP NZ, check_atom_not_builtin
        self.fixup("check_atom_not_builtin");
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x7E]);           // LD A, (HL) - third char
        self.emit(&[0xFE, b'i']);
        self.emit(&[0xC2]);           // JP NZ, check_atom_not_builtin
        self.fixup("check_atom_not_builtin");
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x7E]);           // LD A, (HL) - fourth char
        self.emit(&[0xFE, b'l']);
        self.emit(&[0xC2]);           // JP NZ, check_atom_not_builtin
        self.fixup("check_atom_not_builtin");
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x7E]);           // LD A, (HL) - fifth char (should be null)
        self.emit(&[0xB7]);           // OR A
        self.emit(&[0xC2]);           // JP NZ, check_atom_not_builtin
        self.fixup("check_atom_not_builtin");
        // It's "fail"
        self.emit(&[0xE1]);           // POP HL (discard saved pointer)
        self.emit(&[0xC3]);           // JP exec_builtin_fail
        self.fixup("exec_builtin_fail");

        // Not a built-in atom, query database
        self.label("check_atom_not_builtin");
        self.emit(&[0xE1]);           // POP HL (discard untagged pointer)
        self.emit(&[0xC3]);           // JP do_query_db
        self.fixup("do_query_db");

        // Check for built-in compound terms: write/1
        self.label("check_builtin_compound");
        // Strip tag to get structure address
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xE6, 0x3F]);     // AND 0x3F
        self.emit(&[0x67]);           // LD H, A
        // Load functor pointer (2 bytes at structure start)
        self.emit(&[0x5E]);           // LD E, (HL)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x56]);           // LD D, (HL)
        // DE = functor pointer, dereference to get first char
        self.emit(&[0x1A]);           // LD A, (DE)

        // Check for 'w' (write) - need to check full "write" to avoid matching e.g. "wrap"
        self.emit(&[0xFE, b'w']);
        self.emit(&[0xCA]);           // JP Z, check_builtin_write
        self.fixup("check_builtin_write");

        // Check for 'i' (is) - need to also check second char to avoid matching other i-words
        self.emit(&[0xFE, b'i']);
        self.emit(&[0xCA]);           // JP Z, check_builtin_is
        self.fixup("check_builtin_is");

        // Not a built-in compound, query database
        self.emit(&[0xC3]);           // JP do_query_db
        self.fixup("do_query_db");

        // Check if functor is exactly "write" (w,r,i,t,e,\0)
        self.label("check_builtin_write");
        // DE has functor pointer (at 'w'), check remaining chars
        self.emit(&[0x13]);           // INC DE (point to 'r')
        self.emit(&[0x1A]);           // LD A, (DE)
        self.emit(&[0xFE, b'r']);
        self.emit(&[0xC2]);           // JP NZ, do_query_db (not "write")
        self.fixup("do_query_db");
        self.emit(&[0x13]);           // INC DE (point to 'i')
        self.emit(&[0x1A]);           // LD A, (DE)
        self.emit(&[0xFE, b'i']);
        self.emit(&[0xC2]);           // JP NZ, do_query_db
        self.fixup("do_query_db");
        self.emit(&[0x13]);           // INC DE (point to 't')
        self.emit(&[0x1A]);           // LD A, (DE)
        self.emit(&[0xFE, b't']);
        self.emit(&[0xC2]);           // JP NZ, do_query_db
        self.fixup("do_query_db");
        self.emit(&[0x13]);           // INC DE (point to 'e')
        self.emit(&[0x1A]);           // LD A, (DE)
        self.emit(&[0xFE, b'e']);
        self.emit(&[0xC2]);           // JP NZ, do_query_db
        self.fixup("do_query_db");
        self.emit(&[0x13]);           // INC DE (point to null terminator)
        self.emit(&[0x1A]);           // LD A, (DE)
        self.emit(&[0xB7]);           // OR A (should be null)
        self.emit(&[0xC2]);           // JP NZ, do_query_db
        self.fixup("do_query_db");
        // It's "write" - execute it
        self.emit(&[0xC3]);           // JP exec_builtin_write
        self.fixup("exec_builtin_write");

        // Check if functor is exactly "is"
        self.label("check_builtin_is");
        // DE still has functor pointer, A was 'i'
        self.emit(&[0x13]);           // INC DE (point to second char)
        self.emit(&[0x1A]);           // LD A, (DE)
        self.emit(&[0xFE, b's']);
        self.emit(&[0xC2]);           // JP NZ, do_query_db (not "is")
        self.fixup("do_query_db");
        self.emit(&[0x13]);           // INC DE (point to third char)
        self.emit(&[0x1A]);           // LD A, (DE)
        self.emit(&[0xB7]);           // OR A (should be null)
        self.emit(&[0xC2]);           // JP NZ, do_query_db (not "is")
        self.fixup("do_query_db");
        // It's "is" - execute it
        self.emit(&[0xC3]);           // JP exec_builtin_is
        self.fixup("exec_builtin_is");

        // Execute nl
        self.label("exec_builtin_nl");
        self.emit(&[0xE1]);           // POP HL (discard query term)
        self.emit(&[0xCD]);           // CALL builtin_nl
        self.fixup("builtin_nl");
        self.emit(&[0xC3]);           // JP repl_query_done
        self.fixup("repl_query_done");

        // Execute true
        self.label("exec_builtin_true");
        self.emit(&[0xE1]);           // POP HL
        self.emit(&[0xCD]);           // CALL builtin_true
        self.fixup("builtin_true");
        self.emit(&[0xC3]);           // JP repl_query_done
        self.fixup("repl_query_done");

        // Execute fail
        self.label("exec_builtin_fail");
        self.emit(&[0xE1]);           // POP HL
        self.emit(&[0xCD]);           // CALL builtin_fail
        self.fixup("builtin_fail");
        self.emit(&[0xC3]);           // JP repl_query_done
        self.fixup("repl_query_done");

        // Execute write(X) - need to get first argument
        self.label("exec_builtin_write");
        self.emit(&[0xE1]);           // POP HL (get back query term)
        // Strip tag again
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xE6, 0x3F]);     // AND 0x3F
        self.emit(&[0x67]);           // LD H, A
        // Skip to first arg (offset 3: functor(2) + arity(1))
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x23]);           // INC HL
        // Load first arg
        self.emit(&[0x5E]);           // LD E, (HL)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x56]);           // LD D, (HL)
        self.emit(&[0xEB]);           // EX DE, HL (HL = arg)
        self.emit(&[0xCD]);           // CALL builtin_write
        self.fixup("builtin_write");
        self.emit(&[0xC3]);           // JP repl_query_done
        self.fixup("repl_query_done");

        // Execute is(X, Expr) - evaluate Expr and unify with X
        self.label("exec_builtin_is");
        self.emit(&[0xE1]);           // POP HL (get query term)
        // Strip tag to get structure address
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xE6, 0x3F]);     // AND 0x3F
        self.emit(&[0x67]);           // LD H, A
        // Get first arg (X) at offset 3
        self.emit(&[0xE5]);           // PUSH HL (save struct addr)
        self.emit(&[0x23]);           // INC HL (skip functor low)
        self.emit(&[0x23]);           // INC HL (skip functor high)
        self.emit(&[0x23]);           // INC HL (skip arity)
        self.emit(&[0x5E]);           // LD E, (HL) (X low)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x56]);           // LD D, (HL) (X high)
        self.emit(&[0xD5]);           // PUSH DE (save X)
        // Get second arg (Expr) at offset 5
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x5E]);           // LD E, (HL) (Expr low)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x56]);           // LD D, (HL) (Expr high)
        self.emit(&[0xEB]);           // EX DE, HL (HL = Expr)
        // Evaluate the expression
        self.emit(&[0xCD]);           // CALL eval_arith
        self.fixup("eval_arith");
        // HL = result (INT tagged)
        // Stack: [X, struct_addr]
        self.emit(&[0xEB]);           // EX DE, HL (DE = result)
        self.emit(&[0xE1]);           // POP HL (HL = X)
        self.emit(&[0xE3]);           // EX (SP), HL (HL = struct_addr, stack = X)
        self.emit(&[0xE1]);           // POP HL (HL = X)
        // Now: HL = X, DE = result
        // Unify X with result
        self.emit(&[0xCD]);           // CALL unify
        self.fixup("unify");
        // Carry set = fail, clear = success
        self.emit(&[0xC3]);           // JP repl_query_done
        self.fixup("repl_query_done");

        // Query database
        self.label("do_query_db");
        self.emit(&[0xE1]);           // POP HL (get query term)
        self.emit(&[0xCD]);           // CALL query_db
        self.fixup("query_db");

        // Handle query result
        self.label("repl_query_done");
        // Check result: carry clear = success, carry set = fail
        self.emit(&[0xDA]);           // JP C, repl_query_no (carry set = fail)
        self.fixup("repl_query_no");

        // Found a match - print "yes"
        self.emit(&[0x21]);           // LD HL, yes_str
        self.fixup("yes_str");
        self.emit(&[0xCD]);           // CALL print_string
        self.fixup("print_string");
        self.emit(&[0xC3]);           // JP repl_loop
        self.fixup("repl_loop");

        // No match - print "no"
        self.label("repl_query_no");
        self.emit(&[0x21]);           // LD HL, no_str
        self.fixup("no_str");
        self.emit(&[0xCD]);           // CALL print_string
        self.fixup("print_string");
        self.emit(&[0xC3]);           // JP repl_loop
        self.fixup("repl_loop");
    }
}

impl Default for ReplGenerator {
    fn default() -> Self {
        Self::new()
    }
}

/// Generate REPL binary
pub fn generate_repl() -> Vec<u8> {
    let mut gen = ReplGenerator::new();
    gen.generate()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_repl_generation() {
        let code = generate_repl();
        // Should generate some code
        assert!(!code.is_empty());
        // First instruction should be LD SP
        assert_eq!(code[0], 0x31);
    }

    #[test]
    fn test_memory_layout() {
        // Verify regions don't overlap
        assert!(HEAP_END < TRAIL_START);
        assert!(TRAIL_END < STACK_START);
        assert!(STACK_END < INPUT_BUF);
    }
}
