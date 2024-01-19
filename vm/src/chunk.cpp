#include "chunk.h"
#include <iostream>

void debugInstruction(uint64_t value) {
    uint16_t param3 = getParam3(value);
    uint16_t param2 = getParam2(value);
    uint16_t param1 = getParam1(value);
    OpCode opcode = getOpcode(value);

    std::cout << debugOpcode(opcode) << " " << std::hex << param1 << " " << std::hex << param2 << " " << std::hex << param3 << std::endl;
}

void debugValue(Value value) {
    switch(value.tag) {
        case ValueTag::UNIT:
            std::cout << "UNIT" << std::endl;
        break;
        case ValueTag::BOOLEAN:
            std::cout << "BOOL " << (value.as.boolean ? "true" : "false") << std::endl;
        break;
        case ValueTag::INT_64:
            std::cout << "INT64 " << std::dec << value.as.int64 << std::endl;
        break;
        case ValueTag::FLOAT_64:
            std::cout << "FLOAT64 " << std::dec << value.as.float64 << std::endl;
        case ValueTag::REF:
            std::cout << "REF " << std::endl;
    }
}

uint64_t shiftOpcode(OpCode opcode) {
    return asNum(opcode, 6);
}

uint64_t shiftParam1(uint16_t param) {
    uint64_t result = param;
    return result << (8 * 4);
}

uint64_t shiftParam2(uint16_t param) {
    uint64_t result = param;
    return result << (8 * 2);
}

uint64_t shiftParam3(uint16_t param) {
    uint64_t result = param;
    return result;
}

void Chunk::write(OpCode opcode) {
    code.push_back(shiftOpcode(opcode));
}

void Chunk::write(OpCode opcode, uint16_t param1) {
    code.push_back(shiftOpcode(opcode) | shiftParam1(param1));
}

void Chunk::write(OpCode opcode, uint16_t param1, uint16_t param2) {
    code.push_back(shiftOpcode(opcode) | shiftParam1(param1) | shiftParam2(param2));

}

void Chunk::write(OpCode opcode, uint16_t param1, uint16_t param2, uint16_t param3) {
    code.push_back(shiftOpcode(opcode) | shiftParam1(param1) | shiftParam2(param2) | shiftParam3(param3));
}

void Chunk::writeParam1(size_t instructionPosition, uint16_t param) {
    uint64_t instruction = this->code[instructionPosition];
    auto opcode = getOpcode(instruction);
    auto param1 = param;
    auto param2 = getParam2(instruction);
    auto param3 = getParam3(instruction);
    this->code[instructionPosition] = shiftOpcode(opcode) | shiftParam1(param1) | shiftParam2(param2) | shiftParam3(param3);
 }

void Chunk::debug() {
    std::cout << "======== Chunk ========" << std::endl;
    std::cout << "*** Constants ***" << std::endl;
    for (auto & constant : constants) {
        debugValue(constant);
    }
    std::cout << "*** Registers ***" << std::endl;
    for (auto & reg : registers) {
        debugValue(reg);
    }
    std::cout << "*** Code ***" << std::endl;
    for (auto & value : code) {
        debugInstruction(value);
    }
}

uint16_t getParam(uint64_t instruction, uint8_t pos) {
    uint16_t param = (instruction >> (8*pos)) & 0xffff;
    return param;
}

OpCode getOpcode(uint64_t instruction) {
    uint16_t opcode = getParam(instruction, 6);
    return static_cast<OpCode>(opcode);
}

uint16_t getParam1(uint64_t instruction) {
    return getParam(instruction, 4);
}

uint16_t getParam2(uint64_t instruction) {
    return getParam(instruction, 2);
}

uint16_t getParam3(uint64_t instruction) {
    return getParam(instruction, 0);
}
