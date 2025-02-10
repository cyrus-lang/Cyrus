extern fn printf(fmt: string, ...): void as _printf;

pub fn printf(fmt: string, ...) {
    _printf(fmt);
}
