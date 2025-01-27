struct User {
    name: string;
    age: i32;

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

    user.info();
}
