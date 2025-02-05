pub fn decimal_to_hex(n: i64): string {
    #charset: string = "0123456789ABCDEF";
    
    #hex: string = "";
    #decimal: i64 = n;

    for decimal > 0 {
        #remainder: f64 = decimal % 16;
        #ch: char = charset[remainder];
        hex = ch + hex;
        decimal = decimal / 16;
    }

    return hex;
}
