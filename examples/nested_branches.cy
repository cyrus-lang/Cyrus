pub fn main(): i32 {
    #a = 6;

    if (a == 10) {
        cprintf("first if %d\n", a);
    } 
    else {
        cprintf("first else\n");
        
        if (a > 5) {
            cprintf("a is larget than 5\n");
        }
    }

    return 0;   
}