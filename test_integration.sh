#!/bin/bash
# Integration tests for kz80_prolog REPL
# Runs example .pl files through the emulator and checks results

# Don't use set -e since timeout returns 124

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

PASS=0
FAIL=0
SKIP=0

# Build the REPL binary
echo "Building REPL binary..."
if ! cargo build --release 2>/dev/null; then
    echo -e "${RED}Build failed${NC}"
    exit 1
fi
./target/release/kz80_prolog --repl -o /tmp/kz80_prolog_test.bin 2>/dev/null

EMULATOR="../emulator/retroshield"
if [ ! -x "$EMULATOR" ]; then
    echo -e "${RED}Error: Emulator not found at $EMULATOR${NC}"
    exit 1
fi

# Run a test and check for expected output
# Usage: run_test "test_name" "input" "expected_pattern"
run_test() {
    local name="$1"
    local input="$2"
    local expected="$3"

    local output
    output=$(echo "$input" | timeout 3 "$EMULATOR" /tmp/kz80_prolog_test.bin 2>&1) || true

    if echo "$output" | grep -q "$expected"; then
        echo -e "${GREEN}PASS${NC}: $name"
        ((PASS++))
    else
        echo -e "${RED}FAIL${NC}: $name"
        echo "  Expected: $expected"
        echo "  Output: $output"
        ((FAIL++))
    fi
}

# Run a .pl file and check for expected result
# Usage: run_pl_test "file.pl" "expected_pattern"
run_pl_test() {
    local file="$1"
    local expected="$2"
    local name="$(basename "$file")"

    if [ ! -f "$file" ]; then
        echo -e "${YELLOW}SKIP${NC}: $name (file not found)"
        ((SKIP++))
        return
    fi

    local output
    output=$(cat "$file" | timeout 3 "$EMULATOR" /tmp/kz80_prolog_test.bin 2>&1) || true

    if echo "$output" | grep -q "$expected"; then
        echo -e "${GREEN}PASS${NC}: $name"
        ((PASS++))
    else
        echo -e "${RED}FAIL${NC}: $name"
        echo "  Expected: $expected"
        echo "  Got last lines:"
        echo "$output" | tail -5 | sed 's/^/    /'
        ((FAIL++))
    fi
}

echo ""
echo "=== Example File Tests ==="
run_pl_test "examples/simple.pl" "yes"
run_pl_test "examples/hello.pl" "hello"
run_pl_test "examples/family.pl" "yes"
# factorial.pl requires comparison operators (not yet implemented)
# run_pl_test "examples/factorial.pl" "120"

echo ""
echo "=== Basic Facts and Queries ==="
run_test "simple fact" "foo(42).
?- foo(42)." "yes"

run_test "fact mismatch" "foo(42).
?- foo(99)." "no"

run_test "multiple facts" "bar(1).
bar(2).
bar(3).
?- bar(2)." "yes"

echo ""
echo "=== Rules with Variables ==="
run_test "simple rule" "inner(5).
outer(X) :- inner(X).
?- outer(5)." "yes"

run_test "rule mismatch" "inner(5).
outer(X) :- inner(X).
?- outer(6)." "no"

echo ""
echo "=== Arithmetic ==="
run_test "is addition" "?- 5 is 2 + 3." "yes"

run_test "is addition fail" "?- 6 is 2 + 3." "no"

run_test "is multiplication" "?- 12 is 3 * 4." "yes"

run_test "is subtraction" "?- 7 is 10 - 3." "yes"

run_test "is division" "?- 5 is 10 / 2." "yes"

run_test "is in rule" "add_one(X, Y) :- Y is X + 1.
?- add_one(5, 6)." "yes"

run_test "is in rule fail" "add_one(X, Y) :- Y is X + 1.
?- add_one(5, 7)." "no"

run_test "double call rule" "add_one(X, Y) :- Y is X + 1.
do_two(X, Z) :- add_one(X, T), add_one(T, Z).
?- do_two(5, 7)." "yes"

run_test "double call rule fail" "add_one(X, Y) :- Y is X + 1.
do_two(X, Z) :- add_one(X, T), add_one(T, Z).
?- do_two(5, 8)." "no"

echo ""
echo "=== Builtins ==="
run_test "write atom" "?- write(hello)." "hello"

run_test "write in rule" "greet(X) :- write(X).
?- greet(world)." "world"

run_test "nl builtin" "?- nl." "yes"

echo ""
echo "=== Recursive Rules ==="
run_test "ancestor direct" "parent(a, b).
ancestor(X, Y) :- parent(X, Y).
?- ancestor(a, b)." "yes"

run_test "ancestor transitive" "parent(a, b).
parent(b, c).
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).
?- ancestor(a, c)." "yes"

run_test "ancestor fail" "parent(a, b).
parent(b, c).
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).
?- ancestor(c, a)." "no"

echo ""
echo "=== Summary ==="
echo -e "Passed: ${GREEN}$PASS${NC}"
echo -e "Failed: ${RED}$FAIL${NC}"
echo -e "Skipped: ${YELLOW}$SKIP${NC}"

if [ $FAIL -gt 0 ]; then
    exit 1
fi
