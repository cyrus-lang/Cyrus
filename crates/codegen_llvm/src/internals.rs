use std::{env, fs::File, io::Write, process::exit};
use crate::{CodeGenLLVM, diag::*, funcs::FuncMetadata};
use ast::{
    ast::{FuncDecl, FuncParam, FuncParams, Identifier, VisType},
    token::{Location, Span, TokenKind},
};
use rust_embed::Embed;
use utils::generate_random_hex::generate_random_hex;

#[derive(Embed)]
#[folder = "../../internals/"]
struct Asset;

impl<'ctx> CodeGenLLVM<'ctx> {
    pub(crate) fn build_internals(&mut self) {
        if cfg!(target_os = "linux") {
            let asset_name = "internals_linux.o";
            match Asset::get(asset_name) {
                Some(embedded_file) => {
                    let temp_file_path = format!("{}/{}.o", env::temp_dir().to_str().unwrap(), generate_random_hex());
                    let mut temp_file = File::create(temp_file_path.clone()).unwrap();
                    temp_file.write(&embedded_file.data).unwrap();
                    self.internal_object_modules.push(temp_file_path);
                }
                None => {
                    display_single_diag(Diag {
                        level: DiagLevel::Error,
                        kind: DiagKind::Custom("Failed to get internal object file.".to_string()),
                        location: None,
                    });
                    exit(1);
                }
            }
        }
    }

    pub(crate) fn load_internal_funcs(&mut self) {
        let internal_funcs: Vec<(&str, FuncMetadata)> = vec![("println", {
            let func_decl = FuncDecl {
                name: "internal_println".to_string(),
                params: FuncParams {
                    list: vec![FuncParam {
                        identifier: Identifier {
                            name: "v".to_string(),
                            span: Span::default(),
                            loc: Location::default(),
                        },
                        ty: Some(TokenKind::String),
                        default_value: None,
                        span: Span::default(),
                        loc: Location::default(),
                    }],
                    variadic: Some(TokenKind::Dereference(Box::new(TokenKind::Void))),
                },
                return_type: None,
                vis_type: VisType::Inline,
                renamed_as: Some("println".to_string()),
                span: Span::default(),
                loc: Location::default(),
            };
            let ptr = self.build_func_decl(func_decl.clone());
            FuncMetadata { ptr, func_decl }
        })];

        for (func_name, func_metadata) in internal_funcs {
            self.func_table.insert(
                func_name.to_string(),
                FuncMetadata {
                    ptr: func_metadata.ptr,
                    func_decl: func_metadata.func_decl,
                },
            );
        }
    }
}
