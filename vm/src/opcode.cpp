#include "opcode.h"

uint64_t asNum(OpCode opcode, uint8_t bytesToShift) {
    uint16_t opcode_num = static_cast<uint16_t>(opcode);
    uint64_t widened_num = opcode_num;
    return widened_num << (8 * bytesToShift);
}

const char* debugOpcode(OpCode opcode) {
    switch(opcode) {
        case OpCode::OP_RETURN: return "RETURN";
        case OpCode::OP_ADD: return "ADD";
        case OpCode::OP_SUB: return "SUB";
        case OpCode::OP_MULT: return "MULT";
        case OpCode::OP_MOD: return "MOD";
        case OpCode::OP_DIV: return "DIV";
        case OpCode::OP_NEG: return "NEG";
        case OpCode::OP_JUMP: return "JUMP";
        case OpCode::OP_JUMPF: return "JUMPF";
        case OpCode::OP_MOVE: return "MOVE";
        case OpCode::OP_LOADC: return "LOADC";
        case OpCode::OP_AND: return "AND";
        case OpCode::OP_OR: return "OR";
        case OpCode::OP_EQ: return "EQ";
        case OpCode::OP_LT: return "LT";
        case OpCode::OP_GT: return "GT";
        //default: return "UNKNOWN";
    }
}

const char* debugOpcode(uint16_t opcode) {
    return debugOpcode(static_cast<OpCode>(opcode));
}