struct User {
    name: string;
    age: i32;

    pub fn do_something() {
        cprintf("Do Something\n");
    }

    pub fn info() {
        cprintf("Name: \n");
        cprintf("Age: \n");
    }
}

pub fn main() {
    #user = User {
        name: "Taha",
        age: 17
    };

    cprintf("name: %s\n", user.name);
    cprintf("age: %d\n", user.age);
}
