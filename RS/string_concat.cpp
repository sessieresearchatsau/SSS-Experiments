// Generates the string from Equation 14
// https://www.geeksforgeeks.org/how-to-convert-a-single-character-to-string-in-cpp/


#include <iostream>

int main() {
    std::string str = "";
    for (int i = 0; i <= 3; i++) {
        for (int j = 0; j <= 2; j++) {
            char new_char = 'A' + i + j;
            str = str + new_char;
        }
    }
    std::cout << str << std::endl;
    return 0;
}