pub fn main(): i32 {
    #a: i32 = 3;

    if (a >= 5) {
        cprintf("aaaaaaaaaaaaaaaa: %d\n", a);

        if (a == 7) {
            cprintf("a is %d\n", a);
        }
    } 
    else if (a <= 5) {
        cprintf("ddddddddddddddd: %d\n", a);
    }
    else {
        cprintf("bbbbbbbbbbbbbbb: %d\n", a);
    }

    return 0;
}