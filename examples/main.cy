import io;
import memory;

pub fn main() {
    #a = Memory.malloc(32);;

    Memory.free(a);

    printf("Hello World\n");
}