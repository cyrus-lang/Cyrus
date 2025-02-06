# Cyrus Lang

Cyrus is a general-purpose, statically-typed, manually memory-managed programming language designed for performance-critical applications. It leverages **GCCJIT** as its compiler backend, providing efficient code generation and optimization capabilities. The language syntax is heavily influenced by C, offering familiarity to developers experienced with C-like languages.

## Key Features

- **General-purpose**: Suitable for a wide range of applications, including systems programming, game development, and scientific computing.
- **Statically-typed**: Enforces type safety at compile time, reducing runtime errors and improving code reliability.
- **GCCJIT**: Enables efficient code generation and optimization across various platforms and architectures.
- **C-inspired syntax**: Offers a familiar and concise syntax for developers accustomed to C-like languages.

## Target Audience

Cyrus is primarily aimed at experienced programmers who value performance, control, and low-level system interactions. Developers seeking to build high-performance applications that require precise memory management will find this language well-suited to their needs.

## Running It with Docker

Using Docker to run Cyrus Lang makes the setup and execution process quick and easy. Here's how you can get started:

### Building the Docker Image

First, build the Docker image for Cyrus Lang by running the following command in your terminal. This will use the `Dockerfile` inside the `docker` directory to build the image:

```bash
$ docker build -t my-cyrus-lang -f docker/Dockerfile .
```

## Running Commands in Docker

Once the image is built, you can run various commands inside the Docker container:

- **Check Cargo version**: To verify the installed version of Cargo (Rust's package manager), run:

    ```bash
    $ docker run --rm my-cyrus-lang cargo --version
    ```

- **Run make with the current project:** To execute a make command within the current project directory, mount the current directory into the Docker container and set the working directory to /app:

    ```bash
    $ docker run --rm -v "$PWD:/app" -w /app my-cyrus-lang make dump
    ```

    This command mounts the current directory ($PWD) into the container and runs make dump inside it.

- **Run your project:** To run your project using make within the container:
    ```bash
    $ docker run --rm -v "$PWD:/app" -w /app my-cyrus-lang make run
    ```

    Again, this mounts the current directory into the container and executes make run.

- **Access the container shell:** If you need to get inside the container to debug or manually interact with the environment, you can run:

    ```bash
    $ docker run --rm -it my-cyrus-lang /bin/sh
    ```

    This will drop you into a shell (`/bin/sh`) inside the Docker container.

### Volume Mounting Explained

The -v "$PWD:/app" part of the command mounts your current directory ($PWD) to the /app directory inside the container. This allows you to work with your local files without modifying the Docker image. The -w /app option sets /app as the working directory inside the container, so any commands you run are executed relative to that directory.

## Open to Contribution

We highly encourage contributions! If you're interested in helping to shape the future of Cyrus Lang, feel free to fork the repository, propose improvements, or report any issues you encounter. Together, we can make Cyrus even better!

Copyright 2025 - Cyrus Team
