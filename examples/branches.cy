pub fn main(): i32 {
    #a: i32 = 25;
    #b: i32 = 20;

    if (a == b) {
        cprintf("yes\n");
    }
    else if (a < b)
    {
        cprintf("a is smaller than b\n");
    }
    else 
    {
        cprintf("no\n");
    }

    cprintf("This line is executed at end block.\n");
    
    return 0;
}