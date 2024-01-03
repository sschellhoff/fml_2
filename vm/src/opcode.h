#pragma once
#include <cstdint>

enum class OpCode {
    OP_RETURN = 1,
    OP_ADD,
    OP_SUB,
    OP_MULT,
    OP_DIV,
    OP_MOD,
    OP_AND,
    OP_OR,
    OP_EQ,
    OP_LT,
    OP_GT,
    OP_NEG,
    OP_JUMP,
    OP_JUMPF,
    OP_MOVE
};

uint64_t asNum(OpCode opcode, uint8_t bytesToShift);

const char* debugOpcode(OpCode opcode);

const char* debugOpcode(uint16_t opcode);