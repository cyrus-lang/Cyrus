pub fn main(): i32 {
    #a: i32 = 10;

    {
        #a: i32 = 15;
        cprintf("result: %d\n", a);
    }  

    cprintf("result: %d\n", a);

    return 0;
}