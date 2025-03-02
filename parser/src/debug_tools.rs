use crate::Parser;

impl<'a> Parser<'a> {
    pub fn info_tokens(&self) {
        let cur = self.current_token.clone();
        let peek = self.peek_token.clone();
        println!("cur: {}, peek: {}", cur.kind, peek.kind);
    }
}