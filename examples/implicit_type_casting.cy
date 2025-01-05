pub fn sample() {
    #a: f64 = 3.14;
    #b: i32 = 5.5; /* implicitly casts to int
                      so the balue of the b becomes 5 
                   */

    printf("Result: %f \n", a + b);
}
pub fn main(): i32 {
   

    return 0;   
}