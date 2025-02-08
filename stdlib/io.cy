extern fn printf(fmt: string): void as local_printf;

pub fn printf(fmt: string) {
    local_printf(fmt);
}
