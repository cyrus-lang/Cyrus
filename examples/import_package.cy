import sum_int32;

pub fn main(): i32 {
    cprintf("main.cy executed.\n");

    #result: i32 = sum(10, 50);
    cprintf("result: %d\n", result);
    
    return 0;    
}