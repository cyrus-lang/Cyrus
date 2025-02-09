import io;
import memory;

pub fn main() {
    printf("Program started.\n");

    #a: *void = Memory.malloc(32);
    *a = 10;
    Memory.free(a);

    printf("Program end.\n");
}