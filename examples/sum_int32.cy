pub fn sum(a: i32, b: i32): i32 {
    return a + b;
}

pub fn main(): i32 {
    #val1: i32 = 10;
    #val2: i32 = 15;

    #result: i32 = sum(val1, val2);

    printf("Sum(%d, %d): %d\n", val1, val2, result);

    return 0;   
}