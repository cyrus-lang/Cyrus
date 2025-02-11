import io;

pub fn main(): i32 {
    IO.printf("Program started.\n");

    #i: i32 = 10;

    for {
        IO.printf("i->%d\n", i);

        if (i == 5) {
            break;
        }

        i--;
    }

    IO.printf("Program finished.\n");
    
    return 0;   
}