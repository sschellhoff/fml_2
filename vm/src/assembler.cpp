#include "assembler.h"
#include "value.h"
#include <map>
#include <string> 
#include <iostream>

Assembler::Assembler(Scanner &scanner, Chunk &chunk) : scanner(scanner), chunk(chunk) {
    this->dataParsers = {
        {"i64", &Assembler::parseInt},
        {"f64", &Assembler::parseFloat},
        {"b", &Assembler::parseBool}
    };
    this->instructionParsers = {
        {"RETURN", &Assembler::parseReturn},
        {"ADD", &Assembler::parseAdd},
        {"SUB", &Assembler::parseSub},
        {"MULT", &Assembler::parseMult},
        {"DIV", &Assembler::parseDiv},
        {"MOD", &Assembler::parseMod},
        {"AND", &Assembler::parseAnd},
        {"OR", &Assembler::parseOr},
        {"EQ", &Assembler::parseEq},
        {"LT", &Assembler::parseLt},
        {"GT", &Assembler::parseGt},
        {"NEG", &Assembler::parseNeg},
        {"JUMP", &Assembler::parseJump},
        {"JUMPF", &Assembler::parseJumpf},
        {"MOVE", &Assembler::parseMove},
        {"LOADC", &Assembler::parseLoadc},
        {"RESERVE", &Assembler::parseReserve}
    };
}

void Assembler::parseData () {
    auto w = this->scanner.getWord();
    if (w != "data:") {
        std::cerr << "data:'" << w << "'" << std::endl;
		exit(1);
    }
    while (!this->scanner.isAtEnd()) {
        auto word = this->scanner.getWord();
        if (word == "code:") {
            this->parseCode();
            break;
        }
        if (this->dataParsers.count(word) == 0) {
            std::cerr << "ERROR" << word << std::endl;
		    exit(1);
        }
        auto func = this->dataParsers[word];
        (*this.*func)();
    }
}

void Assembler::Assembler::parseInt () {
    auto word = this->scanner.getWord();
    auto i = std::stoi(word);
    this->chunk.constants.push_back(getInt64(i));
}

void Assembler::parseFloat () {
    auto word = this->scanner.getWord();
    auto f = std::stof(word);
    this->chunk.constants.push_back(getFloat64(f));
}

void Assembler::parseBool () {
    auto word = this->scanner.getWord();
    if (word == "true") {
        this->chunk.constants.push_back(getBoolean(true));
    } else if (word == "false") {
        this->chunk.constants.push_back(getBoolean(true));
    } else {
		std::cerr << "couldnot  read boouean value" << std::endl;
		exit(1);
    }
}

void Assembler::parseCode () {
    while (!this->scanner.isAtEnd()) {
        auto word = this->scanner.getWord();
        if (word.starts_with("LABEL")) {
            if (this->targets.count(word) != 0) {
                std::cerr << "label already seen: " << word << std::endl;
		        exit(1);
            }
            word.pop_back();
            this->targets[word] = this->chunk.code.size();
            continue;
        } else if (this->instructionParsers.count(word) == 0) {
            std::cerr << "ERROR" << word << std::endl;
		    exit(1);
        }
        auto func = this->instructionParsers[word];
        (*this.*func)();
    }
    this->fixJumps();
}

int Assembler::parseNumberOperator () {
    auto word = this->scanner.getWord();
    return std::stoi(word);
}

void Assembler::parseReturn () {
    chunk.write(OpCode::OP_RETURN);
}

void Assembler::parseAdd () {
    auto target = this->parseNumberOperator();
    auto a = this->parseNumberOperator();
    auto b = this->parseNumberOperator();
    chunk.write(OpCode::OP_ADD, target , a, b);
}

void Assembler::parseSub () {
    auto target = this->parseNumberOperator();
    auto a = this->parseNumberOperator();
    auto b = this->parseNumberOperator();
    chunk.write(OpCode::OP_SUB, target , a, b);
}

void Assembler::parseMult () {
    auto target = this->parseNumberOperator();
    auto a = this->parseNumberOperator();
    auto b = this->parseNumberOperator();
    chunk.write(OpCode::OP_MULT, target, a, b);
}

void Assembler::parseDiv () {
    auto target = this->parseNumberOperator();
    auto a = this->parseNumberOperator();
    auto b = this->parseNumberOperator();
    chunk.write(OpCode::OP_DIV, target, a, b);
}

void Assembler::parseMod () {
    auto target = this->parseNumberOperator();
    auto a = this->parseNumberOperator();
    auto b = this->parseNumberOperator();
    chunk.write(OpCode::OP_MOD, target, a, b);
}

void Assembler::parseAnd () {
    auto target = this->parseNumberOperator();
    auto a = this->parseNumberOperator();
    auto b = this->parseNumberOperator();
    chunk.write(OpCode::OP_AND, target, a, b);
}

void Assembler::parseOr () {
    auto target = this->parseNumberOperator();
    auto a = this->parseNumberOperator();
    auto b = this->parseNumberOperator();
    chunk.write(OpCode::OP_OR, target, a, b);
}

void Assembler::parseEq () {
    auto target = this->parseNumberOperator();
    auto a = this->parseNumberOperator();
    auto b = this->parseNumberOperator();
    chunk.write(OpCode::OP_EQ, target, a, b);
}

void Assembler::parseLt () {
    auto target = this->parseNumberOperator();
    auto a = this->parseNumberOperator();
    auto b = this->parseNumberOperator();
    chunk.write(OpCode::OP_LT, target, a, b);
}

void Assembler::parseGt () {
    auto target = this->parseNumberOperator();
    auto a = this->parseNumberOperator();
    auto b = this->parseNumberOperator();
    chunk.write(OpCode::OP_GT, target, a, b);
}

void Assembler::parseNeg () {
    auto i = this->parseNumberOperator();
    chunk.write(OpCode::OP_NEG, i);
}

void Assembler::parseJump () {
    auto target = this->scanner.getWord();
    this->rememberJump(target);
    chunk.write(OpCode::OP_JUMP, 0);
}

void Assembler::parseJumpf () {
    auto target = this->scanner.getWord();
    this->rememberJump(target);
    auto a = this->parseNumberOperator();
    chunk.write(OpCode::OP_JUMPF, 0, a);
}

void Assembler::parseMove () {
    auto target = this->parseNumberOperator();
    auto source = this->parseNumberOperator();
    chunk.write(OpCode::OP_MOVE, target, source);
}

void Assembler::parseLoadc () {
    auto target = this->parseNumberOperator();
    auto source = this->parseNumberOperator();
    chunk.write(OpCode::OP_LOADC, target, source);
}

void Assembler::parseReserve () {
    auto i = this->parseNumberOperator();
    chunk.write(OpCode::OP_RESERVE, i);
}

void Assembler::rememberJump (std::string label) {
    this->jumps[label].push_back(this->chunk.code.size());
}

void Assembler::fixJumps () {
    for (auto &[label, instructionPositions] : this->jumps) {
        auto labelPosition = this->targets[label];
        for (auto instructionPosition : instructionPositions) {
            this->chunk.writeParam1(instructionPosition, labelPosition);
        }
    }
}
