#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OptionalFlag {
    NoReturn,
    NoUnwind,
    Cold,
    Hot,
    OptSize,
    OptNone,
    NoSanitize(String),
}
