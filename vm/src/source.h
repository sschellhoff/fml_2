#pragma once

#include <fstream>
#include <string>
#include <sstream>
#include <iostream>

class Source {
public:
    Source (std::ifstream &stream) : stream(stream) {
    }
    char get () {
        auto c = this->stream.get();
        if (c == '\n') {
            this->line += 1;
            this->column = 0;
        } else {
            this->column += 1;
        }
        return c == std::char_traits<wchar_t>::eof() ? '\0' : c;
    }
    char fetch () {
        auto c = this->stream.peek();
        return c == std::char_traits<wchar_t>::eof() ? '\0' : c;
    }
    bool isAtEnd () {
        return this->fetch() == '\0';
    }
    bool isWhitespace (char c) {
        return c == ' ' || c == '\t' || c == '\n';
    }
    void skipWhitespace () {
        while (true) {
            char c = this->fetch();
            if (c == std::char_traits<wchar_t>::eof()) {
                return;
            }
            if (!this->isWhitespace(c)) {
                return;
            }
            if (c == '\n') {
                this->line += 1;
                this->column = 0;
            } else {
                this->column += 1;
            }
            this->get();
        }
    }
private:
    std::ifstream &stream;
    int line = 0;
    int column = 0;
};
