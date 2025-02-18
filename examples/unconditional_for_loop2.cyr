pub fn main(): i32 {
    cprintf("Program started.\n");

    #i: i32 = 10;

    for {
        cprintf("i->%d\n", i);

        if (i == 5) {
            break;
        }

        #j = 0;

        for {
            cprintf("executed once\n", i);

            j++;

            if (j == 3) {
                break;
            }
        }

        i--;
    }

    cprintf("Program finished.\n");
    
    return 0;   
}