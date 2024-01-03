#include "value.h"
#include <cassert>
#include <iostream>


Value add(Value lhs, Value rhs) {
    assert((lhs.tag == rhs.tag));

    switch (rhs.tag) {
        case ValueTag::INT_64:
            return getInt64(lhs.as.int64 + rhs.as.int64);
        break;
        case ValueTag::FLOAT_64:
            return getFloat64(lhs.as.float64 + rhs.as.float64);
        break;
        default:
            assert((true == false)); // Unsupported type
    }
}

Value sub(Value lhs, Value rhs) {
    assert((lhs.tag == rhs.tag));

    switch (rhs.tag) {
        case ValueTag::INT_64:
            return getInt64(lhs.as.int64 - rhs.as.int64);
        break;
        case ValueTag::FLOAT_64:
            return getFloat64(lhs.as.float64 - rhs.as.float64);
        break;
        default:
            assert((true == false)); // Unsupported type
    }
}

Value mult(Value lhs, Value rhs) {
    assert((lhs.tag == rhs.tag));

    switch (rhs.tag) {
        case ValueTag::INT_64:
            return getInt64(lhs.as.int64 * rhs.as.int64);
        break;
        case ValueTag::FLOAT_64:
            return getFloat64(lhs.as.float64 * rhs.as.float64);
        break;
        default:
            assert((true == false)); // Unsupported type
    }
}

Value div(Value lhs, Value rhs) {
    assert((lhs.tag == rhs.tag));

    switch (rhs.tag) {
        case ValueTag::INT_64:
            return getInt64(lhs.as.int64 / rhs.as.int64);
        break;
        case ValueTag::FLOAT_64:
            return getFloat64(lhs.as.float64 / rhs.as.float64);
        break;
        default:
            assert((true == false)); // Unsupported type
    }
}

Value mod(Value lhs, Value rhs) {
    assert((lhs.tag == rhs.tag));

    switch (rhs.tag) {
        case ValueTag::INT_64:
            return getInt64(lhs.as.int64 % rhs.as.int64);
        break;
        default:
            assert((true == false)); // Unsupported type
    }
}

Value _and(Value lhs, Value rhs) {
    assert((lhs.tag == ValueTag::BOOLEAN && rhs.tag == ValueTag::BOOLEAN));

    return getBoolean(lhs.as.boolean && rhs.as.boolean);
}

Value _or(Value lhs, Value rhs) {
    assert((lhs.tag == ValueTag::BOOLEAN && rhs.tag == ValueTag::BOOLEAN));

    return getBoolean(lhs.as.boolean || rhs.as.boolean);
}

Value eq(Value lhs, Value rhs) {
    assert((lhs.tag == rhs.tag));

    switch (rhs.tag) {
        case ValueTag::INT_64:
            return getBoolean(lhs.as.int64 == rhs.as.int64);
        break;
        case ValueTag::FLOAT_64:
            return getBoolean(lhs.as.float64 == rhs.as.float64);
        break;
        case ValueTag::BOOLEAN:
            return getBoolean(lhs.as.boolean == rhs.as.boolean);
        break;
        default:
            assert((true == false)); // Unsupported type
    }
}


Value lt(Value lhs, Value rhs) {
    assert((lhs.tag == rhs.tag));

    switch (rhs.tag) {
        case ValueTag::INT_64:
            return getBoolean(lhs.as.int64 < rhs.as.int64);
        break;
        case ValueTag::FLOAT_64:
            return getBoolean(lhs.as.float64 < rhs.as.float64);
        break;
        default:
            assert((true == false)); // Unsupported type
    }
}

Value gt(Value lhs, Value rhs) {
    assert((lhs.tag == rhs.tag));

    switch (rhs.tag) {
        case ValueTag::INT_64:
            return getBoolean(lhs.as.int64 > rhs.as.int64);
        break;
        case ValueTag::FLOAT_64:
            return getBoolean(lhs.as.float64 > rhs.as.float64);
        break;
        default:
            assert((true == false)); // Unsupported type
    }
}

Value neg(Value rhs) {
    switch (rhs.tag) {
        case ValueTag::INT_64:
            return getInt64(rhs.as.int64 * -1);
        break;
        case ValueTag::FLOAT_64:
            return getFloat64(rhs.as.float64 * -1.0);
        break;
        case ValueTag::BOOLEAN:
            return getBoolean(!rhs.as.boolean);
        break;
        default:
            assert((true == false)); // Unsupported type
    }
}

Value getInt64(int64_t value) {
    Value v;
    v.as.int64 = value;
    v.tag = ValueTag::INT_64;
    return v;
}

Value getFloat64(float64_t value) {
    Value v;
    v.as.float64 = value;
    v.tag = ValueTag::FLOAT_64;
    return v;
}

Value getBoolean(bool value) {
    Value v;
    v.as.boolean = value;
    v.tag = ValueTag::BOOLEAN;
    return v;
}

Value getUnit() {
    Value v;
    v.tag = ValueTag::UNIT;
    return v;
}