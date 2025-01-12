pub fn main(): i32 {
    cprintf("Program started.\n");
    
    for #i = 0; i < 5; i++ {
       if (i == 3) {
        cprintf("break loop\n");
        break;    
       }

       cprintf("usual: %d\n", i);
    }

    cprintf("Program finished.\n");
    
    return 0;   
}