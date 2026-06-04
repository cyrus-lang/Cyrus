// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

#![allow(nonstandard_style)]

use crate::{args::Args, dispatch::dispatch};
use clap::Parser;

mod args;
mod commands;
mod convert;
mod dispatch;
mod enums;
mod options;
mod scaffold;

pub fn main() {
    let args = Args::parse();

    dispatch(args);
}
