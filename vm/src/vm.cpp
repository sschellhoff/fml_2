#include "vm.h"
#include <assert.h>

ProgramResult VM::interpret(Chunk* chunk) {
    this->chunk = chunk;
    this->ip = 0;
    return run();
}

ProgramResult VM::run() {
    while (true) {
        auto instruction = dispatch();
        auto opcode = getOpcode(instruction);
        switch (opcode) {
            case OpCode::OP_ADD:
                chunk->registers[getParam1(instruction)] = add(chunk->registers[getParam2(instruction)], chunk->registers[getParam3(instruction)]);
            break;
            case OpCode::OP_SUB:
                chunk->registers[getParam1(instruction)] = sub(chunk->registers[getParam2(instruction)], chunk->registers[getParam3(instruction)]);
            break;
            case OpCode::OP_MULT:
                chunk->registers[getParam1(instruction)] = mult(chunk->registers[getParam2(instruction)], chunk->registers[getParam3(instruction)]);
            break;
            case OpCode::OP_DIV:
                chunk->registers[getParam1(instruction)] = div(chunk->registers[getParam2(instruction)], chunk->registers[getParam3(instruction)]);
            break;
            case OpCode::OP_MOD:
                chunk->registers[getParam1(instruction)] = mod(chunk->registers[getParam2(instruction)], chunk->registers[getParam3(instruction)]);
            break;
            case OpCode::OP_AND:
                chunk->registers[getParam1(instruction)] = _and(chunk->registers[getParam2(instruction)], chunk->registers[getParam3(instruction)]);
            break;
            case OpCode::OP_OR:
                chunk->registers[getParam1(instruction)] = _or(chunk->registers[getParam2(instruction)], chunk->registers[getParam3(instruction)]);
            break;
            case OpCode::OP_EQ:
                chunk->registers[getParam1(instruction)] = eq(chunk->registers[getParam2(instruction)], chunk->registers[getParam3(instruction)]);
            break;
            case OpCode::OP_LT:
                chunk->registers[getParam1(instruction)] = lt(chunk->registers[getParam2(instruction)], chunk->registers[getParam3(instruction)]);
            break;
            case OpCode::OP_GT:
                chunk->registers[getParam1(instruction)] = gt(chunk->registers[getParam2(instruction)], chunk->registers[getParam3(instruction)]);
            break;
            case OpCode::OP_NEG:
                chunk->registers[getParam1(instruction)] = neg(chunk->registers[getParam2(instruction)]);
            break;
            case OpCode::OP_MOVE:
                chunk->registers[getParam1(instruction)] = chunk->registers[getParam2(instruction)];
            break;
            case OpCode::OP_LOADC:
                chunk->registers[getParam1(instruction)] = chunk->constants[getParam2(instruction)];
            break;
            case OpCode::OP_JUMP:
                ip = getParam1(instruction);
            break;
            case OpCode::OP_JUMPF: {
                auto target = getParam1(instruction);
                auto condition = chunk->registers[getParam2(instruction)];
                assert((condition.tag == ValueTag::BOOLEAN));
                if (!condition.as.boolean) {
                    ip = target;
                }
            }
            break;
            case OpCode::OP_RETURN:
                return ProgramResult::PROG_OK;
            break;
        }
    }
    return ProgramResult::PROG_OK;
}

uint64_t VM::dispatch() {
    return chunk->code[ip++];
}