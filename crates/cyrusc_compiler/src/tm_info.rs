/* 
 * Copyright (c) 2026 The Cyrus Team
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */
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
