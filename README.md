# Cyrus Lang

Cyrus is a general-purpose, statically-typed, manually memory-managed programming language designed for performance-critical applications. It leverages GCCJIT as its compiler backend, providing efficient code generation and optimization capabilities. The language syntax is heavily influenced by C, offering familiarity to developers experienced with C-like languages.

**Key Features:**

- **General-purpose:** Suitable for a wide range of applications, including systems programming, game development, and scientific computing.
- **Statically-typed:** Enforces type safety at compile time, reducing runtime errors and improving code reliability.
- **GCCJIT:** Enables efficient code generation and optimization across various platforms and architectures.
- **C-inspired syntax:** Offers a familiar and concise syntax for developers accustomed to C-like languages.

## Target Audience

Cyrus is primarily aimed at experienced programmers who value performance, control, and low-level system interactions. Developers seeking to build high-performance applications that require precise memory management will find this language well-suited to their needs.

## Running It

With the help of Docker, you can easily set it up:

```
docker build -t my-cyrus-lang .
docker run --rm my-cyrus-lang cargo --version
docker run --rm -v "$PWD:/app" -w /app my-cyrus-lang make dump
docker run --rm -v "$PWD:/app" -w /app my-cyrus-lang make run
docker run --rm -it my-cyrus-lang /bin/sh
```

## Open to Contribution

We value your contributions so much! If you're interested in helping to shape the future of Cyrus Lang, feel free to fork the repository, propose improvements, or report any issues you encounter.
