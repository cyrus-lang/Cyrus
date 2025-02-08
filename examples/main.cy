pub fn main(): i32 {
    #my_arr: i32[2][2] = [
        [50, 2],
        [30, 4]
    ];

    my_arr[0] = [35, 45]; // This isn't working fine.

    my_arr[0][0] = 17;

    cprintf("%d\n", my_arr[0][0]);
    return 0;
}
