pub fn main(): i32 {
    #value = 10;
    #my_var: *i32 = &value;

    cprintf("%d\n", *my_var);

    return 0;
}