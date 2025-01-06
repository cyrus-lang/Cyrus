fn main() {
    cc::Build::new()
        .file("../shared_library/io.c")
        .file("../shared_library/memory.c")
        .compile("cyrus_shared_library");
}
