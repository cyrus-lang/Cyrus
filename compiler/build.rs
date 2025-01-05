fn main() {
    cc::Build::new()
        .file("../shared_library/io.c")
        .compile("cyrus_io");
}
