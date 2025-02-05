struct Person {
    name: string;
    age: i32;

    fn info(self: *Person) {
        *self.name;
        // cprintf("Name: %s\n", *self.name);
        // cprintf("Age: %d\n", self.age);
    }
}

pub fn main() {
    #person = &Person {
        name: "Taha",
        age: 17,
    };

    person.info();
}