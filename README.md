# kz80_prolog

A Prolog compiler for the Z80 processor, targeting the RetroShield platform.

## Features

- Edinburgh Prolog syntax
- Facts and rules (Horn clauses)
- Unification and backtracking
- Arithmetic evaluation (`is`, `+`, `-`, `*`, `/`, `mod`)
- Comparisons (`<`, `>`, `=<`, `>=`, `=:=`, `=\=`)
- Lists with cons cells (`[H|T]`)
- Cut (`!`) for control flow
- Basic I/O (`write`, `nl`)

## Usage

```bash
# Compile a Prolog program to Z80 binary
kz80_prolog program.pl -o program.bin

# Print AST (for debugging)
kz80_prolog --ast program.pl

# Print tokens (for debugging)
kz80_prolog --tokens program.pl
```

## Example

```prolog
% family.pl
parent(tom, bob).
parent(bob, pat).

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).

?- ancestor(tom, pat).
```

## Architecture

The compiler uses a **Simplified Prolog Machine (SPM)** approach:

1. **Lexer** - Tokenizes Prolog source
2. **Parser** - Builds AST (recursive descent)
3. **Analyzer** - Classifies variables (permanent/temporary)
4. **Compiler** - Generates SPM bytecode
5. **Code Generator** - Produces 8KB ROM image with interpreter

### Memory Layout

```
ROM (8KB):
  0x0000-0x00FF  Startup
  0x0100-0x0EFF  SPM interpreter
  0x0F00-0x13FF  Runtime library
  0x1400-0x1BFF  Compiled bytecode
  0x1C00-0x1FFF  Atom table

RAM (6KB):
  0x2000-0x27FF  Heap
  0x2800-0x2FFF  Trail
  0x3000-0x35FF  Stack
  0x3600-0x37FF  Registers
```

## Building

```bash
cargo build --release
```

## Testing

```bash
cargo test
```

## License

BSD-3-Clause
