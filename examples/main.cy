pub fn main(): i32 {
    #a: i32 = 5;

    if (a == 5) {
        cprintf("a: %d\n", a);

        if (a == 10) {
            cprintf("a is 6\n");
        }       
    }

    return 0;
}