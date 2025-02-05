struct Person {
    name: string;
    age: i32;

    fn info(self: *Person) {
        cprintf("%p\n", &self);
        // cprintf("Name: %s\n", self.name);
        // cprintf("Age: %d\n", self.age);
    }

    fn static_func_call(): *Person {
        // #a = &Person {
        //     name: "Taha",
        //     age: 17,
        // };

        // return a;
    }
}

pub fn main() {
    #person = Person {
        name: "Taha",
        age: 17,
    };

    person.info();
}