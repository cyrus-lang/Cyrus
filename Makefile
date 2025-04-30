PROJECT_NAME = cyrus

# Build directory
BUILD_DIR = build

# Source directory
SOURCE_DIR = src

# Executable name 
EXECUTABLE = cyrus 

# Compiler
CXX = clang++

# Default target
all: build run

# CMake configuration target
cmake:
	@echo "===== Configuring CMake ====="
	@mkdir -p $(BUILD_DIR)
	@cd $(BUILD_DIR) && cmake .. 

# Build target
build: cmake
	@echo "===== Building project with make ====="
	@cd $(BUILD_DIR) && $(MAKE) 

# Run target
run: build
	@echo "===== Running executable ====="
	@cd $(BUILD_DIR) && ./$(EXECUTABLE) 

# Clean target
clean:
	@rm -rf $(BUILD_DIR)

# Rebuild target
rebuild: clean build

# Help target
help:
	@echo "Usage: make <target>"
	@echo "Targets:"
	@echo "  all     - Build and run the project (default)"
	@echo "  cmake   - Configure the project with CMake"
	@echo "  build   - Build the project"
	@echo "  run     - Run the executable"
	@echo "  clean   - Clean the project (remove build files)"
	@echo "  rebuild - Clean and rebuild the project"
	@echo "  help    - Display this help message"

.PHONY: all cmake build run clean rebuild help # Declare phony targets
