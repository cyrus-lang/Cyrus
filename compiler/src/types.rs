use ast::token::{Token, TokenKind};
use gccjit::Type;
use utils::compiler_error;

use crate::Compiler;

impl<'a> Compiler<'a> {
    pub fn void_type(&self) -> Type {
        self.context.new_type::<()>()
    }

    pub fn i32_type(&self) -> Type {
        self.context.new_type::<i32>()
    }

    pub fn i64_type(&self) -> Type {
        self.context.new_type::<i64>()
    }

    pub fn u32_type(&self) -> Type {
        self.context.new_type::<u32>()
    }

    pub fn u64_type(&self) -> Type {
        self.context.new_type::<u64>()
    }

    pub fn f32_type(&self) -> Type {
        self.context.new_type::<f32>()
    }

    pub fn f64_type(&self) -> Type {
        self.context.new_type::<f32>()
    }

    pub fn token_to_type(&self, token_kind: TokenKind) -> Type {
        match token_kind {
            TokenKind::I32 => self.i32_type(),
            TokenKind::I64 => self.i64_type(),
            TokenKind::U32 => self.u32_type(),
            TokenKind::U64 => self.u64_type(),
            TokenKind::Void => self.void_type(),
            _ => compiler_error!("Invalid token given to cast to a GCCJIT type."),
        }
    }
}
