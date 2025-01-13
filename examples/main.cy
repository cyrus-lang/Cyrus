pub fn main(): i32 {
    cprintf("Program started.\n");
    
    for #i = 0; i < 8; i++ {
        for #j = 0; j < 20; j++ {
            cprintf("j: %d\n", j);
            
            break;
        }

        break;
    }

    cprintf("Program finished.\n");
    
    return 0;   
}