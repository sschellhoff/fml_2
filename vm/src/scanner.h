#pragma once

#include "source.h"
#include <iostream>

class Scanner {
public:
    Scanner (Source &source) : source(source) {
    }

    std::string getWord() {
        this->source.skipWhitespace();
        std::stringstream ss;
        while  (!this->source.isAtEnd()) {
            if (this->source.isWhitespace(this->source.fetch())) {
                break;
            }
            ss << this->source.get();
        }
        return ss.str();
    }

    bool isAtEnd () {
        this->source.skipWhitespace();
        return this->source.isAtEnd();
    }

private:
    Source &source;
};
