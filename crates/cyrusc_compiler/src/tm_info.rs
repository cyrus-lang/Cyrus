// Copyright (c) 2026 Cyrus Team. All rights reserved.
// Use of this source code is governed by a LGPLv3.0
// a copy of which can be found in the LICENSE file.

use std::fmt;

#[derive(Debug, Clone)]
pub struct TargetMachineInfo {
    pub triple: String,
    pub cpu_name: String,
    pub data_layout: String,
    pub pointer_size_bits: u32,
    pub opt_level: String,
    pub reloc_mode: String,
    pub code_model: String,
    pub link_static: bool,
    pub pie: bool,
    pub endianness: String,
}

impl fmt::Display for TargetMachineInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Target Machine Information:")?;
        writeln!(f, "---------------------------")?;
        writeln!(f, "{:<20}: {}", "Triple", self.triple)?;
        writeln!(f, "{:<20}: {}", "CPU Name", self.cpu_name)?;
        writeln!(f, "{:<20}: {}", "Data Layout", self.data_layout)?;
        writeln!(f, "{:<20}: {}", "Endiannes", self.endianness)?;
        writeln!(f, "{:<20}: {} bits", "Pointer Size", self.pointer_size_bits)?;
        writeln!(f, "{:<20}: {}", "Optimization Level", self.opt_level)?;
        writeln!(f, "{:<20}: {}", "Relocation Mode", self.reloc_mode)?;
        writeln!(f, "{:<20}: {}", "Code Model", self.code_model)?;
        writeln!(f, "{:<20}: {}", "Link Static", self.link_static)?;
        writeln!(f, "{:<20}: {}", "PIE", self.pie)
    }
}
