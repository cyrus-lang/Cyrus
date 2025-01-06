fn main() {
    cc::Build::new()
        .file("../shared_library/io.c")
        .file("../shared_library/fmt.c")
        .compile("cyrus_shared_library");
}
