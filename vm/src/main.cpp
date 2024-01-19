#include "chunk.h"
#include "vm.h"
#include "source.h"
#include "scanner.h"
#include "assembler.h"
#include <iostream>
#include <fstream>

int main(int argc, const char* argv[]) {
	if (argc != 2) {
		std::cout << "Expected one filename as parameter" << std::endl;
		exit(1);
	}
	std::ifstream file(argv[1]);
	if (!file.is_open()) {
		std::cout << "Could not open file " << argv[1] << std::endl;
		exit(1);
	}

	Chunk chunk;

	Source source(file);
	Scanner scanner(source);
	Assembler assembler(scanner, chunk);
	assembler.parse();
	file.close();

	chunk.debug();
	VM vm;
	vm.interpret(&chunk);
	chunk.debug();
	
	return 0;
}
