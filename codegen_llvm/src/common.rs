use ast::token::TokenKind;
use utils::compiler_error;

use crate::CodeGenLLVM;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn vis_type_as_linkage(&mut self, vis_type: VisType) -> Linkage {
        match func_decl.vis_type {
            VisType::Extern => Linkage::External,
            VisType::Pub => Linkage::AvailableExternally,
            VisType::Internal => Linkage::Private,
            VisType::Inline => todo!(),
        }
    }

    pub(crate) fn token_as_data_type(&mut self, token_kind: TokenKind) -> Result<_, _> {
        match token_kind {
            TokenKind::I8 => Ok(self.context.i8_type()),
            TokenKind::I16 => Ok(self.context.i16_type()),
            TokenKind::I32 => Ok(self.context.i32_type()),
            TokenKind::I64 => Ok(self.context.i64_type()),
            TokenKind::I128 => Ok(self.context.i128_type()),
            TokenKind::U8 => Ok(self.context.i8_type()),
            TokenKind::U16 => Ok(self.context.i16_type()),
            TokenKind::U32 => Ok(self.context.i32_type()),
            TokenKind::U64 => Ok(self.context.i64_type()),
            TokenKind::U128 => Ok(self.context.i128_type()),
            TokenKind::Char => Ok(self.context.i8_type()),
            TokenKind::Float => Ok(self.context.f32_type()),
            TokenKind::Double => Ok(self.context.f64_type()),
            TokenKind::Void => Ok(self.context.void_type()),
            TokenKind::Bool => Ok(self.context.bool_type()),
            TokenKind::String => todo!(),
            TokenKind::UserDefinedType(identifier) => todo!(),
            TokenKind::AddressOf(inner_token_kind) => todo!(),
            TokenKind::Dereference(inner_token_kind) => todo!(),
            TokenKind::Array(_, _) => todo!(),
            _ => {
                compiler_error!(format!("Invalid token type: {}", ty), self.file_path.clone());
            }
        }
    }
}
