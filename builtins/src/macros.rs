use object::object::BuiltinFunc;
use std::collections::HashMap;

pub type BuiltinHashMap = HashMap<&'static str, BuiltinFunc>;

#[macro_export]
macro_rules! builtin_builder {
    ($( $key:expr => $val:expr ),*) => {
        {
            let mut map: BuiltinHashMap = HashMap::new();
            $(map.insert($key, $val);)*
            map
        }
    };
}
