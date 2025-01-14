pub fn main(): i32 {
    cprintf("Program started.\n");

    #i: i32 = 10;

    for {
        cprintf("i->%d\n", i);

        if (i == 5) {
            break;
        }

        i--;
    }

    cprintf("Program finished.\n");
    
    return 0;   
}