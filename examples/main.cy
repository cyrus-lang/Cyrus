pub fn main(): i32 {
    #a: i32 = 8;

    if (a == 4) {
        cprintf("aaaaaaaaaaaaaaaa: %d\n", a);

        if (a == 7) {
            cprintf("a is %d\n", a);
        }
    } 
    else if (a == 6) {
        cprintf("ddddddddddddddd: %d\n", a);
    }
    else if (a == 8) {
        cprintf("cccccccccccc: %d\n", a);
    }
    else {
        cprintf("bbbbbbbbbbbbbbb: %d\n", a);
    }

    return 0;
}