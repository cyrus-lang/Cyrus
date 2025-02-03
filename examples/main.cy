struct Person {
    name: string;
    age: i32;

    fn info(self: Person, another_arg: string) {
        cprintf("arg: %s\n", another_arg);
    }

    fn static_func_call() {
        cprintf("This is a simple static func\n");
    }
}

pub fn main() {
    #person = Person {
        name: "Taha",
        age: 17,
    };

    #a = person.name;
    
    // cprintf("Program started\n");
    // person.info("Sample");
    // cprintf("Name: %s\n", );
}