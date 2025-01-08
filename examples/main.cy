pub fn main(): i32 {
    #a = 10;

    if (a == 10) {
        cprintf("first if %d\n", a);
    } else {
        cprintf("first else\n");
        if (a > 5) {
            cprintf("chained if\n");
        }
    }

    return 0;   
}