pub fn main(): i32 {
    #a: i32 = 35;
    #b: i32 = 20;

    if (a == b) {
        cprintf("a is equal to b\n");
    }
    else if (a < b)                 
    {
        cprintf("a is smaller than b\n");
    }
    else if (a > b)
    {
        cprintf("a is larger than b\n");
    }
    else 
    {
        cprintf("something else\n");
    }

    cprintf("end.\n");
    
    return 0;
}