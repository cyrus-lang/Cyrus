use crate::generics::mapping_ctx::GenericMappingCtx;

pub type ParentGenericMappingCtxID = usize;
pub trait GenericMappingCtxArena: Send + Sync {
    fn insert(&mut self, generic_mapping_ctx: GenericMappingCtx) -> ParentGenericMappingCtxID;
    fn get(&self, idx: usize) -> Option<&GenericMappingCtx>;
}

pub struct GenericMappingCtxArenaImpl(Vec<GenericMappingCtx>);

impl GenericMappingCtxArenaImpl {
    pub fn new() -> Self {
        Self { 0: Vec::new() }
    }
}

impl GenericMappingCtxArena for GenericMappingCtxArenaImpl {
    fn insert(&mut self, generic_mapping_ctx: GenericMappingCtx) -> ParentGenericMappingCtxID {
        self.0.push(generic_mapping_ctx);
        self.0.len() - 1
    }

    fn get(&self, idx: usize) -> Option<&GenericMappingCtx> {
        self.0.get(idx)
    }
}

unsafe impl Sync for GenericMappingCtxArenaImpl {}
unsafe impl Send for GenericMappingCtxArenaImpl {}
