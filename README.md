<h1 style="display: flex; align-items: center; gap: 10px;">
  <img src="./docs/logo.png" width="28" height="28" alt="Cyrus Logo">
  Cyrus Programming Language
</h1>

<img src="./docs/cyrus-example-code.png" align="right" width="570px" alt="Cyrus example code">

<p>Cyrus is built around:</p>
<ul>
  <li>Explicit control over the machine</li>
  <li>Clean, human-readable syntax</li>
  <li>Minimal abstractions</li>
  <li>High-performance execution</li>
  <li>Precise low-level interactions</li>
  <li>A strictly imperative/procedural paradigm</li>
</ul>

<p>Cyrus started from a frustration with the growing complexity of modern languages. Rust and Go offer a lot, but they also bring steep learning curves, heavy runtimes, or abstractions that aren't always useful for systems programming.</p>

<p>Cyrus takes a different approach: simplicity and explicitness should not require giving up performance.</p>

<p>This language is built for:</p>
<ul>
  <li>Systems developers who like C but want a more modern language without garbage collection.</li>
  <li>Developers who find Rust’s borrow checker too restrictive for some systems code but still want modern tooling and performance.</li>
  <li>Embedded and OS-level engineers who need no hidden runtime overhead and direct control over memory.</li>
  <li>Anyone looking for an expressive, low-level language with straightforward semantics.</li>
</ul>

## Philosophy: Developer in Control

Cyrus follows a simple principle: **The developer is in charge.**

The language does not try to hide the hardware or make decisions on your behalf. It also does not aim to eliminate every class of bug through heavy compiler-enforced safety systems.

Instead, Cyrus builds on familiar systems programming concepts and makes their behavior explicit. Allocations, mutations, memory indirection, and dispatch are visible in the code, so the relationship between the program and the machine stays clear.

## Explicit Semantics

Cyrus favors explicit intent. Important behavior should be visible in the code rather than happening quietly in the background.

### Mutability You Can See

A variable's mutability is visible at its declaration:

```cyrus
const max_retries = 5; // immutable, forever
var current_try = 0;   // mutable
```

### No Silent Truncation

Cyrus avoids implicit conversions that can lose information. Safe widening (such as `int32` to `int64`) happens automatically, but conversions that may lose data or change signedness require an explicit `@cast`:

```cyrus
const a: int32 = 300;
const b: int64 = a;               // OK: Safe widening
const c: int32 = @cast(int32, b); // OK: Explicit cast required
```

### Trackable Pointers

In Cyrus, pointer indirection is visible in the syntax. `.` is used for direct access and `->` for pointer indirection:

```cyrus
var box = Box { value: 1 };
var box2 = box.methodA(5)->methodB(50);
var box3 = box2->fieldA;

// '->' marks the pointer dereference
```

### Manual, Predictable Memory

There is no garbage collector. You allocate and free memory yourself. The `defer` keyword runs cleanup when the scope exits, in LIFO order:

```cyrus
import std::mem::libc{LibcAllocator};
import std::mem{Allocator};

pub fn main() {
    const allocator = LibcAllocator.new();

    var buffer: int32* = allocator.alloc(64 * @sizeof(int32));
    defer allocator.free(buffer);

    buffer[0] = 10;
}
```

## Total Machine Control

Cyrus gives you direct tools for working with memory and hardware, along with the responsibility that comes with them.

- **Bring your own allocator:** Custom allocators (stack, bump, libc) are first-class citizens using the `Allocator` interface.
- **Type punning:** Use `union` to reinterpret raw memory layouts without casting.
- **Pointer arithmetic:** Full support for GEP-based indexing and pointer math (`ptr + 5`).
- **Bare-metal ready:** Inline assembly, `extern` for seamless C ABI compatibility, and `naked` functions for writing interrupt handlers directly.

## Safety

This level of control comes with tradeoffs. `data races`, `dangling pointers`, and `use after free` errors are still possible by design.

The runtime provides `bounds checking`, `null pointer access`, and `integer overflow` checks. For additional debugging, Cyrus supports sanitizers such as `ASan` and `TSan`.

Variables are `zero initialized` by default. To opt out, use the `undefined` keyword to disable zero initialization.

## Language Overview

For complete and up-to-date language details, see the [Documentation](https://cyrus-lang.ir/en/docs/getting-started/introduction), which is updated as the project evolves.

### Structs

Structs are user-defined composite types that group related data together. They map directly to memory with no hidden object headers or vtables.

```cyrus
struct User {
    pub name: uint8*,
    pub age: uint32,
    id: uint32 // private by default
}
```

Methods are defined inside the struct body using explicit receivers:

```cyrus
struct Counter {
    pub value: int32,

    pub fn increment(self: Self*) {
        self->value += 1;
    }

    pub fn reset(var self: Self) {
        self.value = 0; // operates on a local copy
    }
}

```

Cyrus also supports anonymous **unnamed structs** for inline use or quick configurations. Named and unnamed structs are fully compatible if their field names and types match exactly:

```cyrus
struct Point {
    pub x: int32,
    pub y: int32
}

pub fn main() {
    const pt: Point = struct { x: 10, y: 20 }; // OK
}
```

### Enums

Enums are algebraic sum types where each variant can carry its own payload. Cyrus supports multiple variant kinds: **unit** (no payload), **tuple** (positional fields), **struct** (named fields), and **valued** (constants).

```cyrus
enum Message {
    Quit,                          // Unit variant
    Move(int32, int32),            // Tuple variant
    Write { text: const uint8* },  // Struct variant
    Status = 200                   // Valued variant
}

```

You can match on enums using a `switch` statement with partial matches allowed as long as you handle or return from the rest:

```cyrus
pub fn handle(msg: Message) {
    switch (msg) {
        case .Quit => {
            printf("Quitting\n");
        }
        case .Move(x, y) => {
            printf("Move to (%d, %d)\n", x, y);
        }
        case .Write { text } => {
            printf("Message: %s\n", text);
        }
        case .Status(code) => {
            printf("Status code: %d\n", code);
        }
    }
}
```

### Unions

Unions share the same memory address across all fields. Because they are completely unchecked, they are ideal for memory-efficient C interop or low-level byte manipulation where you need direct control over raw data.

```cyrus
union IntBytes {
    value: int32,
    bytes: uint8[4]
}

pub fn main() {
    const data = IntBytes { value: 0x12345678 };
}
```

### Generics

Generics allow type-parametric code with compile-time checking. Only named types and functions can be generic; unnamed structs and anonymous functions are not supported.

Generic definitions are type-checked at compile time without requiring an instantiation. This catches errors earlier and avoids unnecessary work for heavily templated code.

#### Generic Functions

Add type parameters in angle brackets `<T>` to make a function generic:

```cyrus
fn identity_pair<T>(values: T[2]) T[2] {
    return values;
}

pub fn main() {
    const p1 = identity_pair({1, 2});                 // Inferred
    const p2 = identity_pair<int32>(int32[2] {3, 4}); // Explicit

    printf("%d %d\n", p1[0], p2[0]);
}
```

Generic functions support recursion:

```cyrus
fn count_to_five<T>(x: T) {
    printf("%d ", x);
    if (x < 5) {
        count_to_five(x + 1);
    }
}
```

#### Generic Types

Structs, unions, and enums can all take type parameters. Use `_` to let the compiler infer a type argument while you manually specify others:

```cyrus
struct Pair<K, V> {
    pub key: K,
    pub value: V,
}

pub fn main() {
    const entry = Pair<uint32, _> {
        key: 1,
        value: "Cyrus"
    };
}
```

Generic enums are useful for values such as options and results:

```cyrus
enum Option<T> {
    Some(T),
    None
}

fn print_opt(opt: Option<int32>) {
    switch (opt) {
        case .Some(val) => printf("Value: %d\n", val);
        case .None => printf("Nothing\n");
    }
}
```

#### Generic Methods

Methods can introduce their own type parameters, even when the struct itself isn't generic:

```cyrus
struct Math {
    pub fn add<T>(x: T, y: T) T {
        return x + y;
    }
}

struct Box<T> {
    value: T,
    pub fn new(val: T) Self {
        return Self { value: val };
    }
}
```

#### Default Type Parameters

Provide fallback types when inference fails:

```cyrus
struct Result<V, E = uint64> {
    pub value: V,
    pub error_code: E,
}

pub fn main() {
    var res = Result<uint8*, _> { value: "Success", error_code: 0 };
}
```

#### Generic Type Aliases

Create shorthands for complex generic types, or make aliases generic themselves:

```cyrus
struct Pair<K, V> {
    key: K,
    value: V,
}

type IntPair = Pair<int32, int32>; // Non-generic alias
type Handler<T> = fn(T) void;      // Generic alias

pub fn main() {
    const log: Handler<int32> = fn(x: int32) void {
        printf("%d", x);
    };
}
```

#### Generic Interfaces

Interfaces can define generic contracts. An object can implement a specific instantiation or be generic itself:

```cyrus
interface IValidator<T> {
    fn validate(&const self, value: T) bool;
}

struct AgeValidator : IValidator<int32> {
    pub fn validate(&const self, val: int32) bool {
        return val >= 18;
    }
}

interface Shape<T> {
    fn area(&const self) T;
}

struct Rectangle<T> : Shape<T> {
    width: T,
    height: T,

    pub fn area(&const self) T {
        return self->width * self->height;
    }
}
```

#### Separating Type Arguments

Type arguments belong to either the **type constructor** or the **method call**:

```cyrus
struct InfixCalc<T> {
    pub x: T,
    pub y: T,

    pub fn new(x: T, y: T) Self {
        return Self { x, y };
    }

    pub fn add_to_x<V>(self: Self*, value: V) {
        self->x += @cast(T, value);
    }

    pub fn static_sum<K>(a: T, b: K) T {
        return a + @cast(T, b);
    }
}

pub fn main() {
    var calc = InfixCalc.new(5, 7);

    calc.add_to_x<uint32>(1); // Method type arg only
    calc.add_to_x<uint64>(2);

    // Static call: provide both type and method args
    const result = InfixCalc<int64>.static_sum<uint32>(10, 20);
    const inferred = InfixCalc.static_sum(10, 20);

    printf("%d %d\n", calc.x, result);
}
```

### Polymorphism

Cyrus uses two strategies for polymorphism: **call-site monomorphization** for static generics and **creation-site dynamic dispatch** for runtime interface objects (fat pointers).

Polymorphism is static by default. For runtime polymorphism and vtables, use the `dynamic` keyword explicitly:

```cyrus
interface Speaker {
    fn speak(&const self);
}

struct Dog : Speaker {
    pub fn speak(&const self) {
        printf("Woof!\n");
    }
}

pub fn main() {
    // Explicit dynamic dispatch using the 'dynamic' keyword
    const speaker: Speaker = dynamic Dog{};

    speaker.speak();
}
```

### Memory Management

Allocators are standard interfaces, so system allocators, arenas, and custom strategies can be exchanged without changing the surrounding code.

```cyrus
import std::mem::arena{ArenaAllocator};
import std::mem::libc{LibcAllocator};
import std::mem{Allocator};
import std::libc{printf};

pub fn main() {
    const allocator: Allocator = dynamic LibcAllocator.new();

    var arena = ArenaAllocator.new(allocator, 16);
    defer arena.destroy();

    var sum: int32 = 0;

    for (var i = 0; i <= 50; i += 1) {
        var x: int32* = arena.alloc(@sizeof(int32));
        @assert(x, "alloc failed");

        *x = i;
        sum += *x;

        if (i == 20) {
            arena.reset();
        }
    }
}
```

### ABI / C Interoperation

Cyrus supports bidirectional C interoperation through the `extern(c)` ABI. C functions can be called from Cyrus, and Cyrus functions can be exposed to C code.

Calling C functions:

```cyrus
import std::libc{printf, malloc, free};

pub fn main() {
    const msg: const uint8* = "Hello from Cyrus!";
    printf("%s\n", msg);

    var buffer = malloc(64);
    defer free(buffer);
}
```

Exporting Cyrus functions to C:

```cyrus
extern(c) fn add(a: int32, b: int32) int32 {
    return a + b;
}

extern(c) fn greet(name: const uint8*) {
    printf("Hello, %s!\n", name);
}
```

### The Road Ahead: `translate-c`

We are researching `translate-c`, a tool for converting C headers and source files into Cyrus code. The goal is to make incremental migration of existing C projects practical without requiring a full rewrite.

The `translate-c` tool aims to handle:

- Function declarations and definitions
- Struct, union, and enum definitions
- Typedefs and macros (where possible)
- Preprocessor directives
- Platform-specific code paths

### ABI Stability

Cyrus does **not** guarantee a stable ABI. We reserve the right to change calling conventions, type layouts, and optimization strategies between versions to improve performance and code quality.

For this reason, we strongly recommend:

- Using the **C ABI** (`extern(c)`) for all cross-language boundaries
- Avoiding assumptions about internal Cyrus type layouts in interop code
- Treating Cyrus-to-Cyrus ABI as an implementation detail that may evolve

We consider this tradeoff worthwhile: long-term optimization and clean design take priority over short-term ABI stability. For a stable interface, use C as the common denominator.

## What Cyrus Refuses to Do

Cyrus deliberately leaves out features that add complexity. The goal is to keep the language predictable, transparent, and free of hidden behavior.

| Omission                   | Why we left it out                                                                                                           |
| -------------------------- | ---------------------------------------------------------------------------------------------------------------------------- |
| **Garbage Collection**     | GC hides allocation costs and introduces unpredictable pause times.                                                          |
| **Borrow Checker**         | Complex lifetime rules dictate how you write code. We prefer developer freedom.                                              |
| **Class Inheritance**      | Deep hierarchies obscure data flow. Cyrus prefers simple composition.                                                        |
| **Global Monkey-Patching** | Methods can't be added to a struct outside its defining module.                                                              |
| **Hidden Allocations**     | Every heap allocation requires you to explicitly call an allocator.                                                          |
| **Functional Paradigms**   | Higher-order magic, hidden closures, and persistent data structures obscure execution flow. Cyrus stays strictly procedural. |

## Where Cyrus Fits

Every language makes tradeoffs. Cyrus prioritizes legibility, mechanical sympathy, and developer control.

- **If C frustrates you** because it lacks a real module system, generics, and strict type safety, Cyrus gives you the control of C with the ergonomics of a modern language.
- **If Rust feels heavy** and you find yourself fighting the borrow checker for simple data structures, Cyrus gives you modern tooling without the lifetime restrictions.
- **If Go is too abstract** with its garbage collector and runtime, Cyrus brings you back down to the metal.

## Installation

The Cyrus compiler currently supports **Linux** (x86_64). Support for additional platforms will follow self-hosting.

### Install Nightly

Download the latest nightly binary for Linux from our [GitHub Actions artifacts](https://cyrus-lang.ir/en/docs/getting-started/install-compiler-binary#Install-compiler-binary).

### Build from Source

For instructions on building the compiler from source, see the [Build from Source guide](https://cyrus-lang.ir/en/docs/getting-started/build-from-source#Build-from-source).

## Current Status & Roadmap

Cyrus and its compiler are still under active development. The current focus is **self-hosting** the compiler, which will improve the toolchain and make development faster.

### Immediate Priorities

- **Self-hosting:** The Cyrus compiler is being rewritten in Cyrus itself. This will put the language design to a practical test, improve compiler performance, and make iteration faster.

### Post-Self-Hosting Roadmap

Once self-hosting is complete, we will focus on:

- **Slices:** Safe, bounds‑checked array views; implementation will follow self‑hosting.
- **Generic Instantiation Safety:** Currently behaves like C++ templates (checked at instantiation). After enhancing the analyzer, generics will be fully type-checked at compile time without instantiation, similar to Rust's approach.
- **Attributes:** Will be introduced after self-hosting to reduce the number of modifier keywords in the language.
- **Test Framework:** Internal test harness and comprehensive test suites will be built after self-hosting.
- **Matrix Type:** SIMD‑capable multi‑dimensional array support (similar to vector types) will be added post‑self‑hosting.
- **Variadic Arguments:** Type‑safe, variadic function parameters will be implemented after self‑hosting.

### Longer-Term Research

We are also evaluating several larger systems to see how well they fit the language's focus on explicit control:

- **Concurrency:** Evaluating fibers, coroutines, and OS-level primitives.
- **Error Handling:** Evaluating explicit, boilerplate-free mechanics that avoid hidden unwinding or runtime exceptions.
- **Compile-Time Execution, Reflection, and Metaprogramming:** Exploring controlled, predictable mechanisms for code generation and type inspection without introducing heavy runtime overhead.

> Cyrus is under active development and is not ready for production use. Syntax, semantics, and compiler behavior may change as the core infrastructure matures.

**Cyrus is not trying to be the safest language in the world.** It aims to be a transparent, controllable, and maintainable systems language.

## Code of Conduct

We aim to maintain an open and welcoming community for everyone contributing to or using Cyrus.

Please review our official [Code of Conduct](https://cyrus-lang.ir/en/docs/project/code-of-conduct#Cyrus-Code-of-Conduct) for detailed guidelines on expected behavior, community standards, and reporting procedures.

## Contributing

Contributions are welcome, from bug reports and documentation improvements to compiler development.

To get started, please check out our [Contribution Guide](https://cyrus-lang.ir/en/docs/project/contribution#Contributing-to-Cyrus) for instructions on setting up your development environment, pull request workflows, and coding standards.

## License

Cyrus is open-source software distributed under the [MIT License](https://github.com/cyrus-lang/Cyrus/blob/main/LICENSE). You are free to use, modify, and distribute this software under the terms of the license.
