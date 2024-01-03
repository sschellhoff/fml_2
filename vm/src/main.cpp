#include "chunk.h"
#include "vm.h"
#include <iostream>

int main(int argc, const char* argv[]) {
	Chunk chunk;
	chunk.constants.push_back(getInt64(0));
	chunk.constants.push_back(getInt64(1));
	chunk.constants.push_back(getInt64(5));
	chunk.registers.resize(3, getUnit());

	chunk.write(OpCode::OP_MOVE, 0, 1);
	chunk.write(OpCode::OP_MOVE, 1, 2);
	chunk.write(OpCode::OP_MOVE, 2, 0);
	chunk.write(OpCode::OP_GT, 2, 1, 2);
	chunk.write(OpCode::OP_JUMPF, 9, 2);
	chunk.write(OpCode::OP_MULT, 0, 0, 1);
	chunk.write(OpCode::OP_MOVE, 2, 1);
	chunk.write(OpCode::OP_SUB, 1, 1, 2);
	chunk.write(OpCode::OP_JUMP, 2);
	chunk.write(OpCode::OP_RETURN);

	chunk.debug();
	VM vm;
	vm.interpret(&chunk);
	chunk.debug();
	
	return 0;
}
