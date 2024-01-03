#pragma once

#include "chunk.h"

enum class ProgramResult {
    PROG_OK,
    PROG_RUNTIME_ERROR,
};

class VM {
public:
    ProgramResult interpret(Chunk* chunk);
private:
    size_t ip;
    Chunk* chunk;

    ProgramResult run();
    uint64_t dispatch();
};
