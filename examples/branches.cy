pub fn main(): i32 {
    #a: i32 = 15;
    #b: i32 = 20;

    if (a == b) {
        cprintf("if\n");
    }
    else if (a < b)                 
    {
        cprintf("branch\n");
    }
    else 
    {
        cprintf("you must find this\n");
    }

    cprintf("end.\n");
    
    return 0;
}