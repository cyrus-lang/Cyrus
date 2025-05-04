
#include <iostream>
#include <string>
#include <sstream>
#include <iomanip>

#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#include <sys/ioctl.h>
#include <cstdio> 
#endif

namespace util
{
    /**
     * @brief Prints a line of text to the console with a specified foreground and background color.
     * This function uses platform-specific methods to change the text color.
     *
     * @param text The string to print to the console.
     * @param foregroundColor The color of the text.  Defaults to white.
     * @param backgroundColor The color of the background. Defaults to black.
     */
    void printColoredText(const std::string &text,
                          const std::string &foregroundColor,
                          const std::string &backgroundColor)
    {
#ifdef _WIN32
        // Windows implementation using SetConsoleTextAttribute
        HANDLE hConsole = GetStdHandle(STD_OUTPUT_HANDLE);
        if (hConsole == INVALID_HANDLE_VALUE)
        {
            std::cerr << "(Error) Could not get console handle." << std::endl;
            std::cout << text << std::endl; // Print without color on error.
            return;
        }

        // Color mapping for Windows
        WORD colorCode = 0;
        if (foregroundColor == "white")
            colorCode |= FOREGROUND_WHITE;
        else if (foregroundColor == "red")
            colorCode |= FOREGROUND_RED;
        else if (foregroundColor == "green")
            colorCode |= FOREGROUND_GREEN;
        else if (foregroundColor == "blue")
            colorCode |= FOREGROUND_BLUE;
        else if (foregroundColor == "yellow")
            colorCode |= FOREGROUND_RED | FOREGROUND_GREEN;
        else if (foregroundColor == "magenta")
            colorCode |= FOREGROUND_RED | FOREGROUND_BLUE;
        else if (foregroundColor == "cyan")
            colorCode |= FOREGROUND_GREEN | FOREGROUND_BLUE;
        // Add more colors as needed

        if (backgroundColor == "black")
            ; // No background color needed, default is black.
        else if (backgroundColor == "red")
            colorCode |= BACKGROUND_RED;
        else if (backgroundColor == "green")
            colorCode |= BACKGROUND_GREEN;
        else if (backgroundColor == "blue")
            colorCode |= BACKGROUND_BLUE;
        else if (backgroundColor == "yellow")
            colorCode |= BACKGROUND_RED | BACKGROUND_GREEN;
        else if (backgroundColor == "magenta")
            colorCode |= BACKGROUND_RED | BACKGROUND_BLUE;
        else if (backgroundColor == "cyan")
            colorCode |= BACKGROUND_GREEN | BACKGROUND_BLUE;
        else if (backgroundColor == "white")
            colorCode |= BACKGROUND_RED | BACKGROUND_GREEN | BACKGROUND_BLUE;

        if (!SetConsoleTextAttribute(hConsole, colorCode))
        {
            std::cerr << "(Error) Could not set console text attributes. Error Code: " << GetLastError() << std::endl;
            std::cout << text << std::endl; // Print without color on error.
            return;
        }

        std::cout << text << std::endl;

        // Reset to default color (white on black)
        SetConsoleTextAttribute(hConsole, FOREGROUND_WHITE | BACKGROUND_BLACK);

#else
        // ANSI color codes for Linux and macOS
        std::string foregroundCode = "37"; // Default: white
        std::string backgroundCode = "40"; // Default: black

        if (foregroundColor == "white")
            foregroundCode = "37";
        else if (foregroundColor == "red")
            foregroundCode = "31";
        else if (foregroundColor == "green")
            foregroundCode = "32";
        else if (foregroundColor == "blue")
            foregroundCode = "34";
        else if (foregroundColor == "yellow")
            foregroundCode = "33";
        else if (foregroundColor == "magenta")
            foregroundCode = "35";
        else if (foregroundColor == "cyan")
            foregroundCode = "36";

        if (backgroundColor == "black")
            backgroundCode = "40";
        else if (backgroundColor == "red")
            backgroundCode = "41";
        else if (backgroundColor == "green")
            backgroundCode = "42";
        else if (backgroundColor == "blue")
            backgroundCode = "44";
        else if (backgroundColor == "yellow")
            backgroundCode = "43";
        else if (backgroundColor == "magenta")
            backgroundCode = "45";
        else if (backgroundColor == "cyan")
            backgroundCode = "46";
        else if (backgroundColor == "white")
            backgroundCode = "47";

        std::cout << "\033[" << foregroundCode << ";" << backgroundCode << "m" << text << "\033[0m" << std::endl;
#endif
    }
} // namespace util