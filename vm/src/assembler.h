#pragma  once

#include "scanner.h"
#include "chunk.h"

#include <map>
#include <vector>
#include <string>

class Assembler {
public:
    Assembler (Scanner &scanner, Chunk &chunk);
    void parse () {
        this->parseData();
    }
private:
    typedef void(Assembler::*ParseFunction)(void);

    Scanner &scanner;
    Chunk &chunk;
    std::map<std::string, ParseFunction> dataParsers;
    std::map<std::string, ParseFunction> instructionParsers;
    std::map<std::string, int> targets;
    std::map<std::string, std::vector<int> > jumps;

    void parseData ();
    void parseInt ();
    void parseFloat ();
    void parseBool ();
    void parseCode ();
    int parseNumberOperator ();

    void parseReturn ();
    void parseAdd ();
    void parseSub ();
    void parseMult ();
    void parseDiv ();
    void parseMod ();
    void parseAnd ();
    void parseOr ();
    void parseEq ();
    void parseLt ();
    void parseGt ();
    void parseNeg ();
    void parseJump ();
    void parseJumpf ();
    void parseMove ();
    void parseLoadc ();
    void parseReserve ();

    void rememberJump (std::string label);
    void fixJumps ();
};