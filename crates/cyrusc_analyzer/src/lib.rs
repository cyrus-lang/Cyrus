/*
 * Copyright (c) 2026 The Cyrus Language
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

pub(crate) mod analyze;
pub mod context;
pub(crate) mod diagnostics;
pub(crate) mod env;
pub(crate) mod generics;
pub(crate) mod infer;
pub(crate) mod lint;
pub(crate) mod lower;
pub(crate) mod monomorph;
pub(crate) mod normalizer;
pub(crate) mod resolve;
pub(crate) mod substitute;
pub(crate) mod typecheck;
pub(crate) mod types;
