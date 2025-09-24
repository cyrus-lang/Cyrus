#!/bin/bash

INSTALL_DIR="/usr/bin"
EXECUTABLE_NAME="cyrus"
SOURCE_PATH="./cyrus"  

if [[ $EUID -ne 0 ]]; then
    echo "Please run this script as root or use sudo."
    exit 1
fi

if [[ ! -f "$SOURCE_PATH" ]]; then
    echo "Error: '$SOURCE_PATH' not found. Make sure the Cyrus binary is exists in this directory."
    exit 1
fi

cp "$SOURCE_PATH" "$INSTALL_DIR/$EXECUTABLE_NAME"

chmod +x "$INSTALL_DIR/$EXECUTABLE_NAME"

if command -v "$EXECUTABLE_NAME" &>/dev/null; then
    echo "Cyrus has been installed successfully!"
    echo ""
    echo "You can now run 'cyrus' from anywhere."
    echo ""
    echo "  Quick Usage:"
    echo "   - Check version:       cyrus version"
    echo "   - Show help:           cyrus help"
    echo ""
    echo "Happy coding with Cyrus Lang! 👾"
else
    echo "Installation failed."
    exit 1
fi
