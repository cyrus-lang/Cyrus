pub fn main(): i32 {
    #a: i32 = 8;

    for {
        if (a == 4) {
            cprintf("aaaaaaaaaaaaaaaa: %d\n", a);
        } 
        else {
            cprintf("bbbbbbbbbbbbbbb: %d\n", a);
            break;
        }
    }

    return 0;
}