#[macro_export]
macro_rules! partial_match {
    ($value:expr, { $($pattern:pat => $body:block),* $(,)? }) => {
        match $value {
            $($pattern => $body),*,
            _ => {}
        }
    };
}
