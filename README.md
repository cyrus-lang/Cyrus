# Cyrus Programming Language

[![Project Website](https://img.shields.io/badge/Website-Online-blueviolet)](https://cyrus-lang-v2.netlify.app)

Cyrus is a statically typed, mid-level, general-purpose, C-friendly programming language designed for productivity and high throughput in performance-critical situations.  This project is under heavy development, with its journey starting in 2025.

## Key Features

* **Statically Typed:** Ensures type safety at compile time, leading to fewer runtime errors and improved code reliability.
* **Mid-Level:** Offers a balance between high-level abstractions for ease of use and low-level control for performance optimization.
* **General Purpose:** Suitable for a wide range of applications, including systems programming, application development, and more.
* **C-Friendly:** Designed to interoperate seamlessly with C code, allowing developers to leverage existing C libraries and codebases.
* **Productivity & High Throughput:** Aims to provide a productive development experience while maintaining high performance.
* **Memory Management:** Employs a combination of manual memory management and reference counting to achieve both ease of use and performance.
* **Compiler Backends:** Supports two compiler backends:
  * **C Backend:** Generates C code, which can then be compiled with a C compiler.
  
  * **LLVM Backend:** Leverages the LLVM compiler infrastructure for optimized code generation.
* **Started in 2025:** This is a relatively new language, with development starting in 2025.

## Memory Management Details

Cyrus uses a hybrid approach to memory management:

* **Manual Memory Management:** Provides developers with explicit control over memory allocation and deallocation when needed, crucial for performance-critical sections.

* **Reference Counting:** Automatically manages the lifetime of objects by counting the number of references to them, simplifying memory management in many common cases and preventing memory leaks.

## Getting Started

Since Cyrus is under heavy development, there are no stable releases yet. If you want to contribute, please refer to the [Contributing](#contributing) section.

## Documentation

Detailed documentation is still under development.  The most up-to-date information will be available on the [project website](https://cyrus-lang-v2.netlify.app) as it becomes available.

## Contributing

We welcome contributions!  If you're interested in helping to develop Cyrus, please see our [Contribution Guidelines](https://github.com/cyrus-lang/Cyrus/blob/main/CONTRIBUTING.md) for information on how to get started.  This is a great time to get involved in the early stages of a programming language project.
