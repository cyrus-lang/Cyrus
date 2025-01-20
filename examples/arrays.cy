pub fn main(): i32 {
    #my_var: i32[3] = [10, 20, 30];

    my_var[0] = 555;
    
    cprintf("%d\n", my_var[0]);

    return 0;
}