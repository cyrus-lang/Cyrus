package main;

// Import some libraries
imoprt http:simple_http_server;
imoprt assert;

// Variable definition
#a: i32 = 10;
#b: i64 = 20;
#c: u32 = 30;
#d: u64 = 40;
#e: usize = 2;
#f: f32 = 20.5;
#g: f64 = 30.5;
#h: string = "Cyrus";
#i: char = 'C';
#j: bool = true;

if (a == 2) {
    println(1);
}
else if (a == 3) 
{
    println("a is 3");
}
else 
{
    println("blbllbblbl");
}

// Function definition
fn divide(num1: i32, num2: i32): i32 {
    if num2 == 0 {
        throw "devidide by zero is not possible";
    }

    ret num1 / num2;
}

// Calling function
divide(a, i32(b));


// Array definition
#names = ["Cyrus", "Rust", "Ruby", "Go", "C#"];

for #i = 0; i < 10; i++ {
    println(i);
}

if (next() == 0) {
    println(10 / 2 * 3 + 1);
}

println("It works excellently!");

struct Point {
    x: i32 = 1, // set def val
    y: i32, // def val of i32 is zero for e.g
}

#point: Point = {
    x: 10,
    y: 15
};

struct ThreeDPoint > Point { // extends Point struct
    z: i32
}

impl Point {
    fn to_string(): string {
        ret "x:" + string(self.x) + " y:" + string(self.y);
    }
}

// Arrays
#a: string[] = [
    "Cyrus", "C", "Rust",
    "Ruby", "TypeScript", "Crystal",
    "C#"
];

println(a[0]); 
println(a[1]); 
println(a[1..3]); 

// Ranges
#my_range: range = (1..10);

println(my_range.max); //=> 10
println(my_range.min); //=> 10
println(my_range.include(3)); //=> true
println(my_range.include(0)); //=> false

// Match

#input: string = "Cyrus";

match input {
    "Cyrus" {
        println("Hi Cyrus !_!");
    }
    "Rust" {
        println("Hi Rust 0.0");
    }
    "Python" | 
    "Ruby" {
        println("Hi Hi");
    }
    default {
        println("not recognized ^-^.");
    }
}