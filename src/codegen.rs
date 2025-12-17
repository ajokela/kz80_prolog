//! Code generator for Z80 ROM image output.
//!
//! Combines the runtime interpreter with compiled bytecode to produce
//! a complete ROM image for the Z80.

use std::fs::File;
use std::io::{self, Write};
use std::path::Path;

use crate::compile::CompiledProgram;
use crate::runtime::{memory, Runtime};

/// ROM image generator.
pub struct CodeGenerator {
    /// ROM image buffer
    rom: Vec<u8>,
}

impl CodeGenerator {
    /// Create a new code generator.
    pub fn new() -> Self {
        Self {
            rom: vec![0u8; memory::ROM_END as usize + 1],
        }
    }

    /// Generate complete ROM image from compiled program.
    pub fn generate(&mut self, compiled: &CompiledProgram) -> &[u8] {
        // Generate runtime interpreter
        let mut runtime = Runtime::new();
        runtime.generate();

        // Copy runtime to ROM
        let runtime_code = runtime.code();
        for (i, &byte) in runtime_code.iter().enumerate() {
            if i < self.rom.len() {
                self.rom[i] = byte;
            }
        }

        // Copy bytecode to code section
        let code_start = memory::CODE_START as usize;
        for (i, &byte) in compiled.code.iter().enumerate() {
            let addr = code_start + i;
            if addr < self.rom.len() {
                self.rom[addr] = byte;
            }
        }

        // Build atom table
        self.build_atom_table(compiled);

        // Patch entry point if there's a query
        if let Some(query_offset) = compiled.query_offset {
            self.patch_query_entry(query_offset);
        }

        &self.rom
    }

    /// Build atom table in ROM.
    fn build_atom_table(&mut self, compiled: &CompiledProgram) {
        let table_start = memory::ATOM_TABLE as usize;
        let mut _offset = 0;

        // Format: [count:2] [offset0:2] [offset1:2] ... [string0\0] [string1\0] ...
        let atom_count = compiled.atoms.len();

        // Write count
        self.rom[table_start] = (atom_count & 0xFF) as u8;
        self.rom[table_start + 1] = (atom_count >> 8) as u8;
        _offset += 2;

        // Reserve space for offset table
        let strings_start = table_start + 2 + atom_count * 2;
        let mut string_offset = 0u16;

        // Write offsets and strings
        for (idx, name) in compiled.atoms.iter() {
            // Write offset to string
            let offset_addr = table_start + 2 + (idx as usize) * 2;
            if offset_addr + 1 < self.rom.len() {
                self.rom[offset_addr] = (string_offset & 0xFF) as u8;
                self.rom[offset_addr + 1] = (string_offset >> 8) as u8;
            }

            // Write string
            for (i, byte) in name.bytes().enumerate() {
                let addr = strings_start + string_offset as usize + i;
                if addr < self.rom.len() {
                    self.rom[addr] = byte;
                }
            }

            // Null terminator
            let null_addr = strings_start + string_offset as usize + name.len();
            if null_addr < self.rom.len() {
                self.rom[null_addr] = 0;
            }

            string_offset += name.len() as u16 + 1;
        }
    }

    /// Patch the runtime to start at the query entry point.
    fn patch_query_entry(&mut self, query_offset: u16) {
        // The runtime initializes SPM_PC to CODE_START
        // We need to patch it to point to the query
        let actual_addr = memory::CODE_START + query_offset;

        // Find where SPM_PC is initialized in the runtime and patch it
        // This is a simplified approach - in a full implementation,
        // we'd have the runtime export the patch location

        // Look for the pattern: LD HL, CODE_START; LD (SPM_PC), HL
        // CODE_START = 0x1400
        let pattern = [0x21, 0x00, 0x14]; // LD HL, 0x1400

        for i in 0..self.rom.len() - 3 {
            if self.rom[i..i + 3] == pattern {
                // Patch with actual query address
                self.rom[i + 1] = (actual_addr & 0xFF) as u8;
                self.rom[i + 2] = (actual_addr >> 8) as u8;
                break;
            }
        }
    }

    /// Write ROM to file.
    pub fn write_to_file(&self, path: &Path) -> io::Result<()> {
        let mut file = File::create(path)?;
        file.write_all(&self.rom)?;
        Ok(())
    }

    /// Get ROM size.
    pub fn rom_size(&self) -> usize {
        self.rom.len()
    }

    /// Get used ROM size (non-zero bytes from start).
    pub fn used_size(&self) -> usize {
        // Find last non-zero byte
        for i in (0..self.rom.len()).rev() {
            if self.rom[i] != 0 {
                return i + 1;
            }
        }
        0
    }
}

impl Default for CodeGenerator {
    fn default() -> Self {
        Self::new()
    }
}

/// Generate a ROM image from a compiled program.
pub fn generate_rom(compiled: &CompiledProgram) -> Vec<u8> {
    let mut codegen = CodeGenerator::new();
    codegen.generate(compiled).to_vec()
}

/// Generate and write ROM to file.
pub fn generate_rom_file(compiled: &CompiledProgram, path: &Path) -> io::Result<()> {
    let mut codegen = CodeGenerator::new();
    codegen.generate(compiled);
    codegen.write_to_file(path)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compile::compile_program;
    use crate::parse;

    #[test]
    fn test_generate_rom() {
        let program = parse("parent(tom, bob). ?- parent(tom, bob).").unwrap();
        let compiled = compile_program(&program);
        let rom = generate_rom(&compiled);

        // ROM should be 8KB
        assert_eq!(rom.len(), 8192);

        // First byte should be JP instruction
        assert_eq!(rom[0], 0xC3);
    }

    #[test]
    fn test_used_size() {
        let program = parse("foo.").unwrap();
        let compiled = compile_program(&program);

        let mut codegen = CodeGenerator::new();
        codegen.generate(&compiled);

        // Used size should be less than full ROM
        assert!(codegen.used_size() < codegen.rom_size());
        assert!(codegen.used_size() > 0);
    }
}
