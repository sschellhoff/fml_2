#pragma once

#include "opcode.h"
#include "value.h"
#include <vector>

class Chunk {
public:
    std::vector<uint64_t> code;
    std::vector<Value> constants;
    std::vector<Value> registers;

    void write(OpCode opcode);
    void write(OpCode opcode, uint16_t param1);
    void write(OpCode opcode, uint16_t param1, uint16_t param2);
    void write(OpCode opcode, uint16_t param1, uint16_t param2, uint16_t param3);
    void writeParam1(size_t instructionPosition, uint16_t param);

    void debug();
};

OpCode getOpcode(uint64_t instruction);

uint16_t getParam1(uint64_t instruction);
uint16_t getParam2(uint64_t instruction);
uint16_t getParam3(uint64_t instruction);
