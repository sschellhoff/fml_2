#pragma once

#include <cstdint>
#include <string>

using float64_t = double; // TODO change this type

enum class ValueTag {
    UNIT = 0,
    INT_64,
    FLOAT_64,
    BOOLEAN,
    REF,
};

struct Value {
    ValueTag tag;
    union {
        bool boolean;
        int64_t int64;
        float64_t float64;
    } as;
};

enum class RefTag {
    STR = 0,
};

//struct Ref {
//    RefTag tag;
//};
//
//struct FmlString {
//    Ref ref;
//    size_t length;
//    char * chars;
//};

Value add(Value lhs, Value rhs);
Value sub(Value lhs, Value rhs);
Value mult(Value lhs, Value rhs);
Value div(Value lhs, Value rhs);
Value mod(Value lhs, Value rhs);
Value _and(Value lhs, Value rhs);
Value _or(Value lhs, Value rhs);
Value eq(Value lhs, Value rhs);
Value lt(Value lhs, Value rhs);
Value gt(Value lhs, Value rhs);
Value neg(Value lhs);
Value getInt64(int64_t value);
Value getFloat64(float64_t value);
Value getBoolean(bool value);
Value getUnit();