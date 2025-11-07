use crate::{
    callconv::CallConv, export::ExportKind, flags::OptionalFlag, inline::Inlining, linkage::Linkage,
    placement::SectionAttr, prologue::Prologue, visibility::Visibility,
};

#[derive(Debug, Clone)]
pub struct FunctionModifiers {
    pub visibility: Visibility,
    pub linkage: Option<Linkage>,
    pub inlining: Option<Inlining>,
    pub prologue: Option<Prologue>,
    pub export: Option<ExportKind>,
    pub callconv: Option<CallConv>,
    pub optional_flags: Vec<OptionalFlag>,
    pub placement: Vec<SectionAttr>,
}

impl FunctionModifiers {
    pub fn new() -> Self {
        Self {
            visibility: Visibility::default(),
            linkage: None,
            inlining: None,
            prologue: None,
            export: None,
            callconv: None,
            optional_flags: Vec::new(),
            placement: Vec::new(),
        }
    }

    pub fn validate(&self) -> Result<(), String> {
        if let (Some(Prologue::Naked), Some(_)) = (self.prologue, self.inlining) {
            return Err("Cannot combine 'naked' prologue with any inlining modifier.".into());
        }

        if let Some(Prologue::Naked) = self.prologue {
            if let Some(cc) = &self.callconv {
                if *cc != CallConv::Naked {
                    return Err("Naked prologue must use callconv(naked).".into());
                }
            } else {
                return Err("Naked prologue requires callconv(naked).".into());
            }
        }

        Ok(())
    }

    pub fn add_optional_flag(&mut self, flag: OptionalFlag) {
        self.optional_flags.push(flag);
    }

    pub fn add_placement(&mut self, section: SectionAttr) {
        self.placement.push(section);
    }
}
