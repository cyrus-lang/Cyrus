pub fn main(): i32 {
    #a: i64 = 10;

    if (++a == 11) {
        cprintf("%d\n", a);
    } else {
        cprintf("a is 11 now\n");
    }
    
    return 0;   
}