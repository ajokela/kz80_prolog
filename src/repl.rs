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
            current_addr: 0x0000,
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
        self.label("print_int");
        // Check sign
        self.emit(&[0xCB, 0x7C]);     // BIT 7, H
        self.emit(&[0x28, 0x0B]);     // JR Z, print_int_pos
        // Print minus
        self.emit(&[0x3E, b'-']);     // LD A, '-'
        self.emit(&[0xCD]);           // CALL putchar
        self.fixup("putchar");
        // Negate HL
        self.emit(&[0xAF]);           // XOR A
        self.emit(&[0x95]);           // SUB L
        self.emit(&[0x6F]);           // LD L, A
        self.emit(&[0x9F]);           // SBC A, A
        self.emit(&[0x94]);           // SUB H
        self.emit(&[0x67]);           // LD H, A

        self.label("print_int_pos");
        // Use stack to reverse digits
        self.emit(&[0x01]);           // LD BC, 0
        self.emit_word(0x0000);

        self.label("print_int_loop");
        // Divide by 10
        self.emit(&[0x11]);           // LD DE, 10
        self.emit_word(10);
        self.emit(&[0xCD]);           // CALL div16
        self.fixup("div16");
        // A = remainder, HL = quotient
        self.emit(&[0xC6, b'0']);     // ADD A, '0'
        self.emit(&[0xF5]);           // PUSH AF
        self.emit(&[0x03]);           // INC BC
        // Check if HL is 0
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xB5]);           // OR L
        self.emit(&[0xC2]);           // JP NZ, print_int_loop
        self.fixup("print_int_loop");

        // Print digits
        self.label("print_int_out");
        self.emit(&[0x78]);           // LD A, B
        self.emit(&[0xB1]);           // OR C
        self.emit(&[0xC8]);           // RET Z
        self.emit(&[0xF1]);           // POP AF
        self.emit(&[0xCD]);           // CALL putchar
        self.fixup("putchar");
        self.emit(&[0x0B]);           // DEC BC
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
        self.emit(&[0x38, 0x0B]);     // JR C, div16_done (B < D)
        self.emit(&[0x20, 0x04]);     // JR NZ, div16_sub (B > D)
        self.emit(&[0x79]);           // LD A, C
        self.emit(&[0xBB]);           // CP E
        self.emit(&[0x38, 0x05]);     // JR C, div16_done (C < E)

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
        // Sets IX to INPUT_BUF
        self.label("lex_init");
        self.emit(&[0xDD, 0x21]);     // LD IX, INPUT_BUF
        self.emit_word(INPUT_BUF);
        self.emit(&[0xC9]);           // RET

        // Get next token
        // Output: A = token type, HL = value (for INT) or atom index (for ATOM)
        self.label("lex_next");
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

        // Atom parsing (simplified - just return first char as index)
        self.label("lex_atom");
        self.emit(&[0xDD, 0x6E, 0x00]); // LD L, (IX+0)
        self.emit(&[0x26, 0x00]);     // LD H, 0
        self.emit(&[0xDD, 0x23]);     // INC IX
        // Skip remaining alphanumeric
        self.label("lex_atom_skip");
        self.emit(&[0xDD, 0x7E, 0x00]); // LD A, (IX+0)
        self.emit(&[0xCD]);           // CALL is_alnum
        self.fixup("is_alnum");
        self.emit(&[0xCA]);           // JP Z, lex_atom_done
        self.fixup("lex_atom_done");
        self.emit(&[0xDD, 0x23]);     // INC IX
        self.emit(&[0xC3]);           // JP lex_atom_skip
        self.fixup("lex_atom_skip");
        self.label("lex_atom_done");
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
        // Simple atom
        self.emit(&[0xE1]);           // POP HL
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xF6, tag::ATM]); // OR ATM tag
        self.emit(&[0x67]);           // LD H, A
        self.emit(&[0xC9]);           // RET

        // Compound term - simplified, just return atom for now
        self.label("parse_compound");
        self.emit(&[0xE1]);           // POP HL
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xF6, tag::ATM]); // OR ATM tag
        self.emit(&[0x67]);           // LD H, A
        self.emit(&[0xC9]);           // RET

        // Variable
        self.label("parse_var");
        // Create unbound variable on heap
        self.emit(&[0xE5]);           // PUSH HL (save var name)
        self.emit(&[0xCD]);           // CALL heap_alloc_var
        self.fixup("heap_alloc_var");
        self.emit(&[0xD1]);           // POP DE (discard name)
        self.emit(&[0xC9]);           // RET

        // Anonymous variable
        self.label("parse_anon");
        self.emit(&[0xCD]);           // CALL heap_alloc_var
        self.fixup("heap_alloc_var");
        self.emit(&[0xC9]);           // RET

        // List - simplified
        self.label("parse_list");
        // Just return nil for now
        self.emit(&[0x21]);           // LD HL, nil atom
        self.emit_word(tag::ATM as u16); // Tagged nil
        self.emit(&[0xC9]);           // RET
    }

    /// Unification algorithm
    fn emit_unification(&mut self) {
        // Dereference a term
        // Input: HL = term
        // Output: HL = dereferenced term
        self.label("deref");
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xE6, 0xC0]);     // AND 0xC0 (get tag)
        self.emit(&[0xC0]);           // RET NZ (not REF)
        // It's a REF, check if bound
        self.emit(&[0x5E]);           // LD E, (HL)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x56]);           // LD D, (HL)
        self.emit(&[0x2B]);           // DEC HL
        // Check if self-referential
        self.emit(&[0x7B]);           // LD A, E
        self.emit(&[0xBD]);           // CP L
        self.emit(&[0xC0]);           // RET NZ
        self.emit(&[0x7A]);           // LD A, D
        self.emit(&[0xBC]);           // CP H
        self.emit(&[0xC8]);           // RET Z (unbound)
        // Follow reference
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
        // Check if term1 is unbound REF
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xE6, 0xC0]);     // AND 0xC0
        self.emit(&[0xC2]);           // JP NZ, unify_check_t2
        self.fixup("unify_check_t2");
        // term1 is unbound, bind it to term2
        self.emit(&[0x73]);           // LD (HL), E
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x72]);           // LD (HL), D
        self.emit(&[0x2B]);           // DEC HL
        self.emit(&[0xCD]);           // CALL trail
        self.fixup("trail");
        self.emit(&[0xC3]);           // JP unify_success
        self.fixup("unify_success");

        // Check if term2 is unbound REF
        self.label("unify_check_t2");
        self.emit(&[0x7A]);           // LD A, D
        self.emit(&[0xE6, 0xC0]);     // AND 0xC0
        self.emit(&[0xC2]);           // JP NZ, unify_fail
        self.fixup("unify_fail");
        // term2 is unbound, bind it to term1
        self.emit(&[0xEB]);           // EX DE, HL
        self.emit(&[0x73]);           // LD (HL), E
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x72]);           // LD (HL), D
        self.emit(&[0x2B]);           // DEC HL
        self.emit(&[0xCD]);           // CALL trail
        self.fixup("trail");
        self.emit(&[0xC3]);           // JP unify_success
        self.fixup("unify_success");

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

    /// Backtracking
    fn emit_backtracking(&mut self) {
        // Backtrack - try next alternative
        self.label("backtrack");
        // Check if any choice points
        self.emit(&[0x2A]);           // LD HL, (SPM_B)
        self.emit_word(SPM_B);
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xB5]);           // OR L
        self.emit(&[0xC8]);           // RET Z (no choice points - fail completely)

        // Restore from choice point
        // Simplified: just return failure for now
        self.emit(&[0x37]);           // SCF (set carry = fail)
        self.emit(&[0xC9]);           // RET
    }

    /// Clause database operations
    fn emit_clause_db(&mut self) {
        // Assert a fact - store term HL in clause database
        // Input: HL = term to assert
        // Modifies: DE, BC
        self.label("assert_fact");
        self.emit(&[0xE5]);           // PUSH HL (save term)
        // Get current clause slot
        self.emit(&[0x2A]);           // LD HL, (CLAUSE_NEXT)
        self.emit_word(CLAUSE_NEXT);
        self.emit(&[0xD1]);           // POP DE (DE = term)
        // Store term at clause slot
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

        // Query database - try to find a matching clause
        // Input: HL = query term
        // Output: Carry clear = found match, Carry set = no match
        // Also sets SEARCH_IDX for backtracking
        self.label("query_db");
        // Save query term for backtracking
        self.emit(&[0x22]);           // LD (QUERY_TERM), HL
        self.emit_word(QUERY_TERM);
        // Start from first clause (index 0)
        self.emit(&[0x21]);           // LD HL, 0
        self.emit_word(0x0000);
        self.emit(&[0x22]);           // LD (SEARCH_IDX), HL
        self.emit_word(SEARCH_IDX);
        // Fall through to try_next_clause

        // Try next clause - continue search from SEARCH_IDX
        // Output: Carry clear = found match, Carry set = no more clauses
        self.label("try_next_clause");
        // Check if we've exhausted all clauses
        self.emit(&[0x2A]);           // LD HL, (SEARCH_IDX)
        self.emit_word(SEARCH_IDX);
        self.emit(&[0xEB]);           // EX DE, HL (DE = current index)
        self.emit(&[0x2A]);           // LD HL, (CLAUSE_COUNT)
        self.emit_word(CLAUSE_COUNT);
        // Compare DE with HL (index vs count)
        self.emit(&[0x7A]);           // LD A, D
        self.emit(&[0xBC]);           // CP H
        self.emit(&[0xC2]);           // JP NZ, clause_cmp_ne
        self.fixup("clause_cmp_ne");
        self.emit(&[0x7B]);           // LD A, E
        self.emit(&[0xBD]);           // CP L
        self.emit(&[0xCA]);           // JP Z, query_fail (index == count)
        self.fixup("query_fail");

        self.label("clause_cmp_ne");
        // Not equal, continue if index < count
        // Calculate clause address: CLAUSE_DB + index * 2
        self.emit(&[0xEB]);           // EX DE, HL (HL = index)
        self.emit(&[0x29]);           // ADD HL, HL (HL = index * 2)
        self.emit(&[0x11]);           // LD DE, CLAUSE_DB
        self.emit_word(CLAUSE_DB);
        self.emit(&[0x19]);           // ADD HL, DE (HL = CLAUSE_DB + index*2)
        // Load clause term
        self.emit(&[0x5E]);           // LD E, (HL)
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x56]);           // LD D, (HL) (DE = clause term)

        // Increment search index for next iteration
        self.emit(&[0x2A]);           // LD HL, (SEARCH_IDX)
        self.emit_word(SEARCH_IDX);
        self.emit(&[0x23]);           // INC HL
        self.emit(&[0x22]);           // LD (SEARCH_IDX), HL
        self.emit_word(SEARCH_IDX);

        // Reset trail before unification attempt
        self.emit(&[0x21]);           // LD HL, TRAIL_START
        self.emit_word(TRAIL_START);
        self.emit(&[0x22]);           // LD (SPM_TR), HL
        self.emit_word(SPM_TR);

        // Try to unify query with clause
        // Load query term into HL
        self.emit(&[0x2A]);           // LD HL, (QUERY_TERM)
        self.emit_word(QUERY_TERM);
        // DE already has clause term
        self.emit(&[0xCD]);           // CALL unify
        self.fixup("unify");
        // If unify succeeded (carry clear), return success
        self.emit(&[0xD0]);           // RET NC (return if no carry = success)
        // Unification failed, try next clause
        self.emit(&[0xC3]);           // JP try_next_clause
        self.fixup("try_next_clause");

        // No more clauses - query fails
        self.label("query_fail");
        self.emit(&[0x37]);           // SCF (set carry = fail)
        self.emit(&[0xC9]);           // RET
    }

    /// Built-in predicates
    fn emit_builtins(&mut self) {
        // write/1 - print a term
        self.label("builtin_write");
        // For now, just print "term"
        self.emit(&[0x3E, b't']);
        self.emit(&[0xCD]);
        self.fixup("putchar");
        self.emit(&[0x3E, b'e']);
        self.emit(&[0xCD]);
        self.fixup("putchar");
        self.emit(&[0x3E, b'r']);
        self.emit(&[0xCD]);
        self.fixup("putchar");
        self.emit(&[0x3E, b'm']);
        self.emit(&[0xCD]);
        self.fixup("putchar");
        self.emit(&[0xC9]);

        // nl/0 - print newline
        self.label("builtin_nl");
        self.emit(&[0xC3]);           // JP print_nl
        self.fixup("print_nl");
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
        // Only reset for queries

        // Reset trail
        self.emit(&[0x21]);           // LD HL, TRAIL_START
        self.emit_word(TRAIL_START);
        self.emit(&[0x22]);           // LD (SPM_TR), HL
        self.emit_word(SPM_TR);

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

        // Otherwise treat as fact assertion
        // A = token type, HL = value from lex_next
        // Handle simple atoms: if A == ATOM, build term and assert
        self.emit(&[0xFE, tok::ATOM]); // CP ATOM
        self.emit(&[0xC2]);           // JP NZ, repl_fact_error
        self.fixup("repl_fact_error");

        // It's an atom - tag it and assert
        // HL has the atom index, add ATM tag
        self.emit(&[0x7C]);           // LD A, H
        self.emit(&[0xF6, tag::ATM]); // OR ATM tag
        self.emit(&[0x67]);           // LD H, A
        // Now HL is a tagged atom term
        self.emit(&[0xCD]);           // CALL assert_fact
        self.fixup("assert_fact");

        // Print "ok"
        self.emit(&[0x21]);           // LD HL, ok_str
        self.fixup("ok_str");
        self.emit(&[0xCD]);           // CALL print_string
        self.fixup("print_string");
        self.emit(&[0xC3]);           // JP repl_loop
        self.fixup("repl_loop");

        // Fact error - unsupported fact type
        self.label("repl_fact_error");
        self.emit(&[0x21]);           // LD HL, error_str
        self.fixup("error_str");
        self.emit(&[0xCD]);           // CALL print_string
        self.fixup("print_string");
        self.emit(&[0xC3]);           // JP repl_loop
        self.fixup("repl_loop");

        // Handle query
        self.label("repl_query");
        // Parse the query term
        self.emit(&[0xCD]);           // CALL parse_term
        self.fixup("parse_term");

        // HL now has the parsed query term
        // Query the database
        self.emit(&[0xCD]);           // CALL query_db
        self.fixup("query_db");

        // Check result: carry clear = found, carry set = not found
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
