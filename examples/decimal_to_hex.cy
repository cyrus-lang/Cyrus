pub fn decimal_to_hex(n: i64) {
    #charset: string = "0123456789ABCDEF";

    #i = 5;
    #c: char = charset[i];

    cprintf("%c\n", c);

    // #hex: string = "";
    // #rem: i64 = 0;
    // #ch: char = charset[0];

    // cprintf("%d\n", rem);
    // cprintf("%d\n", remainder);
    // hex = ch + hex;
    // decimal = decimal / 16;

    // return hex;
}

pub fn main() {
    decimal_to_hex(33);
}