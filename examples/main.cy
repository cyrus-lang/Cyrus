struct Person {
    name: string;
    lname: string;

    fn info(self: Person) {
        // self.name = "Taha";

        // cprintf("info(): %s\n", p.name);
    }

    fn static_func_call() {
        cprintf("This is a simple static func\n");
    }
}

pub fn main() {
    #p = Person {
        name: "Unknown",
        lname: "Dostifam"
    };

    p.info();
    cprintf("name: %s\n", p.name);

}
