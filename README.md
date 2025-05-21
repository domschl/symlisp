[![SymLisp CI](https://github.com/domschl/symlisp/actions/workflows/ci.yml/badge.svg)](https://github.com/domschl/symlisp/actions/workflows/ci.yml)

## SymLisp: A Scheme-like Interpreter for Symbolic Computation written entirely by AI systems

SymLisp is a lightweight, embeddable Scheme-like interpreter with a focus on symbolic computation capabilities. Its core interpreter logic is written in C, its symbolic algebra system and standard library extensions are written in its own Scheme dialect (all by Gemini Pro), and its Jupyter Kernel is written in Python (by Claude 3).

The system includes a growing symbolic algebra system, also written in Scheme, allowing for expression simplification and expansion.

### Features
*   Scheme-like syntax and core functionality.
*   Support for rational numbers (via GMP) and arbitrary-precision integers.
*   Basic symbolic manipulation: simplification and expansion of algebraic and trigonometric expressions.
*   UTF-8 support for strings and symbols.
*   A simple Read-Eval-Print Loop (REPL).
*   A Jupyter kernel for interactive notebook usage, supporting Markdown and LaTeX output for symbolic expressions.
*   MCP Server that allows usage of the interpreter by LLMs
*   A standard library written in Scheme, providing list utilities, infix parsing, and symbolic operations.

## Build Instructions

SymLisp uses CMake for building.

1.  **Prerequisites**:
    *   A C compiler (GCC or Clang recommended)
    *   CMake (version 3.10 or higher)
    *   GMP (GNU Multiple Precision Arithmetic Library)
    *   Valgrind (optional, for memory checking during tests)
    *   Python 3 and pip (for Jupyter kernel installation)

2.  **Compilation Steps**:
    ```bash
    git clone https://github.com/domschl/symlisp.git # Or your repository URL
    cd symlisp
    mkdir build
    cd build
    cmake ..
    make
    ```
    This will build the main interpreter (`symlisp`) and the test executable.

3.  **Running Tests**:
    ```bash
    ctest # From the build directory
    # For memory checking with Valgrind (if configured in CMakeLists.txt)
    ctest -T memcheck
    ```

## Usage

### REPL (Read-Eval-Print Loop)
After building, you can run the REPL:
```bash
./build/src/symlisp
```
This will start an interactive session where you can type SymLisp expressions.

### Jupyter Kernel
1.  **Installation**:
    Ensure you have Jupyter installed (`pip install jupyterlab notebook`).
    From the root directory of the SymLisp project:
    ```bash
    pip install -e ./jupyter/kernel
    ```
    This installs the kernel in editable mode.

2.  **Running**:
    You can then start Jupyter Lab or Notebook:
    ```bash
    jupyter lab
    # or
    jupyter notebook
    ```
    You should be able to create new notebooks using the "SymLisp" kernel.

### MCP (Model Context Protocol) Server

SymLisp includes an MCP server implementation that allows other applications to interact with the SymLisp interpreter through a standard JSON-RPC 2.0 interface over stdio.

### MCP Server Usage

To run the MCP server:

```bash
./build/mcp_symlisp/mcp_symlisp [options]
```

Command-line Options

- -n: Do not load the standard library
- -l path: Specify a custom path to the standard library folder (defaults to "../stdsymlisp")

#### Supported Methods

The MCP server supports the following JSON-RPC methods:

- eval: Evaluates SymLisp code

```json
{"jsonrpc": "2.0", "method": "eval", "params": {"code": "(+ 1 2)"}, "id": 1}
```

- version: Returns version information about the MCP server

```json
{"jsonrpc": "2.0", "method": "version", "id": 2}
```
- capabilities: Returns information about the server's capabilities

```json
{"jsonrpc": "2.0", "method": "capabilities", "id": 3}
```

#### Integration

The MCP server can be used to integrate SymLisp with other applications, tools, or language models. It provides a standardized way to evaluate SymLisp code and retrieve results in a structured JSON format.

## Contributing
Contributions are welcome! Please see the `CONTRIBUTING.md` file for more details on the development process, planned features, and how to contribute.

## Language Reference

SymLisp's functionality is provided by a combination of core built-in functions (implemented in C for performance and direct interpreter interaction) and a suite of standard library functions (implemented in SymLisp's own Scheme dialect, typically found in files within the `stdsymlisp/` directory). This reference aims to clarify the source and usage of these components. In the descriptions below, functions implemented in C are generally referred to as 'built-in'. Functions from the standard library will often indicate their source file (e.g., `lists.scm`, `symbolics.scm`) in their description or section heading. Special forms, which are fundamental syntactic constructs, are an intrinsic part of the interpreter.

### Special Forms
These are fundamental syntactic constructs recognized directly by the interpreter:
*   `quote` or `'`: Prevents evaluation of an expression. Takes one argument, `(quote <expression>)`.
*   `lambda`: Defines an anonymous procedure. Syntax: `(lambda (param1 param2 ...) body1 body2 ...)`
*   `define`: Defines variables and procedures in the current environment. Syntax: `(define variable value)` or `(define (procedure-name param1 ...) body1 ...)`
*   `if`: Conditional execution. Syntax: `(if test-expr then-expr else-expr)`. `else-expr` is optional.
*   `set!`: Assigns a new value to an existing binding. Syntax: `(set! variable value)`
*   `begin`: Sequences expressions, returning the value of the last. Syntax: `(begin expr1 expr2 ...)`
*   `let`, `let*`, `letrec`: Create local bindings.
    *   `let`: `(let ((var1 val1) (var2 val2) ...) body1 ...)` - bindings are parallel.
    *   `let*`: `(let* ((var1 val1) (var2 val2) ...) body1 ...)` - bindings are sequential.
    *   `letrec`: `(letrec ((var1 val1) (var2 val2) ...) body1 ...)` - bindings are recursive.
*   `cond`: Multi-branch conditional. Syntax: `(cond (test1 expr1 ...) (test2 expr2 ...) ... (else else-expr1 ...))`
*   `and`, `or`: Logical conjunction and disjunction (short-circuiting). Syntax: `(and expr1 expr2 ...)` or `(or expr1 expr2 ...)`
*   `define-syntax`: Defines macros (currently non-hygienic, limited scope). Syntax: `(define-syntax keyword (syntax-rules (literal ...) ((pattern) template) ...))`

### Core Language & Evaluation
*   `eval`: (built-in) Evaluates an expression. Syntax: `(eval expr [env])`. If `env` is provided, evaluates `expr` in that environment.
*   `apply`: (built-in) Applies a procedure to a list of arguments. Syntax: `(apply proc args-list)`.
*   `interaction-environment`: (built-in) Returns the global REPL environment. Syntax: `(interaction-environment)`
*   `environment`: (built-in) Creates a new, empty environment whose parent is the interaction environment. Syntax: `(environment)`
*   `error`: (built-in) Signals an error. Syntax: `(error message-string [obj ...])`. Additional `obj` are printed after the message.

### List Processing
This section includes core list manipulation functions. Many are built-in for efficiency, while others are part of the standard library, typically found in `stdsymlisp/lists.scm`.
*   `cons`: (built-in) Creates a new pair. Syntax: `(cons obj1 obj2)`
*   `list`: (built-in) Creates a new list. Syntax: `(list obj ...)`
*   `cons*`: (built-in) Creates a list with the last argument as the tail. Syntax: `(cons* obj1 obj2 ... objN)`. Equivalent to `(cons obj1 (cons obj2 ... (cons objN-1 objN) ...))`.
*   `car`: (built-in) Returns the first element of a pair. Syntax: `(car pair)`
*   `cdr`: (built-in) Returns the second element of a pair. Syntax: `(cdr pair)`
*   `caar`, `cadr`, `cddr`, `caddr`, etc.: (built-ins) Compositions of `car` and `cdr` up to four levels. e.g. `(cadr list)` is `(car (cdr list))`.
*   `set-car!`: (built-in) Modifies the `car` of `pair`. Syntax: `(set-car! pair obj)`
*   `set-cdr!`: (built-in) Modifies the `cdr` of `pair`. Syntax: `(set-cdr! pair obj)`
*   `pair?`: (built-in, predicate) Tests if `obj` is a pair. Syntax: `(pair? obj)`
*   `null?`: (built-in, predicate) Tests if `obj` is the empty list `'()`. Syntax: `(null? obj)`
*   `list?`: (built-in, predicate) Tests if `obj` is a proper list (ends with `null?`). Syntax: `(list? obj)`
*   `length`: (built-in) Returns the length of a proper `list`. Syntax: `(length list)`
*   `append`: (built-in) Concatenates lists. The last argument becomes the tail of the new list. Syntax: `(append list ...)`
*   `reverse`: (built-in) Returns a new list with elements in reverse order. Syntax: `(reverse list)`
*   `assoc`: (standard library, `lists.scm`) Finds the first pair in `alist` (an association list) whose `car` is `equal?` to `key`. Returns `#f` if not found. Syntax: `(assoc key alist)`
*   `memq`: (standard library, `lists.scm`) Searches `list` for `obj` using `eq?`. Returns the sublist starting with the first occurrence of `obj`, or `#f`. Syntax: `(memq obj list)`
*   `member`: (standard library, `lists.scm`) Searches `list` for `obj` using `equal?`. Returns the sublist starting with the first occurrence of `obj`, or `#f`. Syntax: `(member obj list)`
*   `exists`: (standard library, `lists.scm`) Returns the first true value returned by `(pred element)` for elements in `list`. If no element satisfies `pred`, returns `#f`. Syntax: `(exists pred list)`
*   `find-if`: (standard library, `lists.scm`) Returns the first element in `list` that satisfies `pred`. If no element satisfies `pred`, returns `#f`. Syntax: `(find-if pred list)`
*   `remove`: (standard library, `lists.scm`) Returns a new list with the first `equal?` occurrence of `item` removed. Syntax: `(remove item list)`
*   `count`: (standard library, `lists.scm`) Counts elements in `list` satisfying `pred`. Syntax: `(count pred list)`
*   `remove-duplicates`: (standard library, `lists.scm`) Removes duplicate elements from `list` (uses `equal?`). Syntax: `(remove-duplicates list)`
*   `list-take`: (standard library, `lists.scm`) Returns the first `k` elements of `list`. Syntax: `(list-take list k)`
*   `list-drop`: (standard library, `lists.scm`) Returns `list` after dropping the first `k` elements. Syntax: `(list-drop list k)`
*   `merge-sorted-lists`: (standard library, `lists.scm`) Merges two sorted lists according to `pred`. Syntax: `(merge-sorted-lists list1 list2 pred)`
*   `list-sort`: (standard library, `lists.scm`) Sorts `list` using `pred`. Syntax: `(list-sort pred list)`
*   `iota`: (standard library, `lists.scm`) Generates a list of numbers. Syntax: `(iota count [start step])`. `start` defaults to 0, `step` defaults to 1.

### Higher-Order Functions
*   `map`: (built-in) Applies `proc` to corresponding elements of one or more `list`s. Syntax: `(map proc list1 [list2 ...])`
*   `filter`: (built-in) Returns a new list containing elements from `list` that satisfy `pred`. Syntax: `(filter pred list)`
*   `for-each`: (built-in) Applies `proc` to corresponding elements of one or more `list`s for side effects. Returns an unspecified value. Syntax: `(for-each proc list1 [list2 ...])`
*   `fold-left`: (built-in) Left-associative fold. Accumulates a value by applying `proc` to each element of the `list`(s) and an accumulator, starting with `initial`. Syntax: `(fold-left proc initial list1 [list2 ...])`
*   `reduce`: (built-in) Alias for `fold-left`. Syntax: `(reduce proc initial list1 [list2 ...])`
*   `fold-right`: (built-in) Right-associative fold. Syntax: `(fold-right proc initial list1 [list2 ...])`
*   `filter-map`: (standard library, from `stdsymlisp/lists.scm`) Applies `proc` to each element of `list` and collects non-`#f` results into a new list. Syntax: `(filter-map proc list)`

### Numbers & Arithmetic
*   `+`: (built-in) Addition. Syntax: `(+ [num ...])`. `(+)` returns 0.
*   `-`: (built-in) Subtraction. Syntax: `(- num1 [num ...])`. `(- x)` returns the negation of `x`.
*   `*`: (built-in) Multiplication. Syntax: `(* [num ...])`. `(*)` returns 1.
*   `/`: (built-in) Division. Syntax: `(/ num1 [num ...])`. `(/ x)` returns `1/x`.
*   `modulo`: (built-in) Modulo operation (result has sign of `int2`, R7RS `floor` division). Syntax: `(modulo int1 int2)`
*   `remainder`: (built-in) Remainder operation (result has sign of `int1`, R7RS `truncate` division). Syntax: `(remainder int1 int2)`
*   `denominator`: (built-in) Returns the denominator of a rational `num`. Syntax: `(denominator num)`
*   `numerator`: (built-in) Returns the numerator of a rational `num`. Syntax: `(numerator num)`
*   `quotient`: (built-in) Integer division (truncates towards zero). Syntax: `(quotient int1 int2)`
*   `gcd`: (built-in) Greatest Common Divisor. Syntax: `(gcd int ...)` `(gcd)` returns 0.
*   `lcm`: (built-in) Least Common Multiple. Syntax: `(lcm int ...)` `(lcm)` returns 1.
*   `expt`: (built-in) `base` raised to the power of `exponent` (integer `exponent`s only). Syntax: `(expt base exponent)`
*   `square`: (built-in) Returns `num * num`. Syntax: `(square num)`
*   `exact-integer-sqrt`: (built-in) Returns a list `(list s r)` where `s*s + r = n`, `s` is the integer square root of `n`, and `r` is the remainder. Syntax: `(exact-integer-sqrt n)`
*   `abs`: (built-in) Absolute value. Syntax: `(abs num)`
*   `max`: (built-in) Maximum of numbers. Syntax: `(max num ...)`
*   `min`: (built-in) Minimum of numbers. Syntax: `(min num ...)`
*   `=`: (built-in) Numeric equality. Syntax: `(= num1 num2)`
*   `>`: (built-in) Numeric greater than. Syntax: `(> num1 num2)`
*   `<`: (built-in) Numeric less than. Syntax: `(< num1 num2)`
*   `>=`: (built-in) Numeric greater than or equal. Syntax: `(>= num1 num2)`
*   `<=`: (built-in) Numeric less than or equal. Syntax: `(<= num1 num2)`
*   `number?`: (built-in, predicate) Tests if `obj` is a number (integer or rational). Syntax: `(number? obj)`
*   `integer?`: (built-in, predicate) Tests if `obj` is an integer. Syntax: `(integer? obj)`
*   `odd?`: (built-in, predicate) Tests if `integer` is odd. Syntax: `(odd? integer)`
*   `even?`: (built-in, predicate) Tests if `integer` is even. Syntax: `(even? integer)`
*   `prime?`: (built-in, predicate) Probabilistic primality test (Miller-Rabin). Syntax: `(prime? integer)`
*   `zero?`: (standard library, from `stdsymlisp/lists.scm`, but numeric utility) Tests if `num` is zero. Syntax: `(zero? num)`
*   `number->string`: (built-in) Converts `num` to its string representation. For integers, `radix` (2-62) can be specified. Syntax: `(number->string num [radix])`
*   `string->number`: (built-in) Converts `str` to a number or `#f` if parsing fails. Supports integers (radix 2-62, e.g. `#b101`, `#o10`, `#d10`, `#x10`), fractions `N/D` (radix 10), and decimals (radix 10). Syntax: `(string->number str [radix])`
*   `float`: (built-in) Returns string representation of `num` as a floating-point number to a specified `precision` (decimal places). Syntax: `(float num [precision])`
*   `prime-factors`: (built-in) Returns a list of prime factors of integer `n`. Syntax: `(prime-factors n)`
*   `next-prime`: (built-in) Returns the smallest prime strictly greater than integer `n`. Syntax: `(next-prime n)`

### Strings
*   `string`: (built-in) Creates a string from `char`s. Syntax: `(string char ...)`
*   `string-length`: (built-in) Returns the number of Unicode code points in `str`. Syntax: `(string-length str)`
*   `string-ref`: (built-in) Returns the character at 0-based index `k` in `str`. Syntax: `(string-ref str k)`
*   `string-append`: (built-in) Concatenates strings. Syntax: `(string-append str ...)`
*   `substring`: (built-in) Extracts a substring from `start` (inclusive) to `end` (exclusive). Indices are code point based. Syntax: `(substring str start [end])`
*   `string-join`: (built-in) Joins a `list-of-strings` using `delimiter-string`. Syntax: `(string-join list-of-strings delimiter-string)`
*   `string-split`: (built-in) Splits `str` by a single `delimiter-char`. Consecutive delimiters yield empty strings. Syntax: `(string-split str delimiter-char)`
*   `string-tokenize`: (built-in) Splits `str` by any character in `delimiter-set-string`. Consecutive delimiters are treated as one. Syntax: `(string-tokenize str delimiter-set-string)`
*   `string-upcase`: (built-in) Converts `str` to uppercase (simple Unicode case mapping). Syntax: `(string-upcase str)`
*   `string-downcase`: (built-in) Converts `str` to lowercase (simple Unicode case mapping). Syntax: `(string-downcase str)`
*   `string=?`: (built-in) Case-sensitive string equality. Syntax: `(string=? str1 str2)`
*   `string<?`: (built-in) Case-sensitive string less than (lexicographical). Syntax: `(string<? str1 str2)`
*   `string>?`: (built-in) Case-sensitive string greater than (lexicographical). Syntax: `(string>? str1 str2)`
*   `string<=?`: (built-in) Case-sensitive string less than or equal (lexicographical). Syntax: `(string<=? str1 str2)`
*   `string>=?`: (built-in) Case-sensitive string greater than or equal (lexicographical). Syntax: `(string>=? str1 str2)`
*   `string-ci=?`: (built-in) Case-insensitive string equality. Syntax: `(string-ci=? str1 str2)`
*   `string-ci<?`: (built-in) Case-insensitive string less than (lexicographical). Syntax: `(string-ci<? str1 str2)`
*   `string-ci>?`: (built-in) Case-insensitive string greater than (lexicographical). Syntax: `(string-ci>? str1 str2)`
*   `string-ci<=?`: (built-in) Case-insensitive string less than or equal (lexicographical). Syntax: `(string-ci<=? str1 str2)`
*   `string-ci>=?`: (built-in) Case-insensitive string greater than or equal (lexicographical). Syntax: `(string-ci>=? str1 str2)`
*   `string?`: (built-in, predicate) Tests if `obj` is a string. Syntax: `(string? obj)`
*   `string->list`: (built-in) Converts `str` to a list of characters. Syntax: `(string->list str)`
*   `list->string`: (built-in) Converts a `list-of-chars` to a string. Syntax: `(list->string list-of-chars)`
*   `symbol->string`: (built-in) Converts `sym` to its string name. Syntax: `(symbol->string sym)`
*   `string->symbol`: (built-in) Converts `str` to a symbol (interned). Syntax: `(string->symbol str)`
*   `expr->string`: (built-in) Converts `expr` to its external string representation (parsable by `read` or `string->expr`). Syntax: `(expr->string expr)`
*   `string->expr`: (built-in) Parses `str` into a single SymLisp expression. Syntax: `(string->expr str)`
*   `string->infix-tokens`: (built-in) Tokenizes an infix `str` (primarily for internal use by `string->prefix-expr`). Syntax: `(string->infix-tokens str)`

### Characters
*   `char?`: (built-in, predicate) Tests if `obj` is a character. Syntax: `(char? obj)`
*   `char-alphabetic?`: (built-in, predicate) Tests if `char` is alphabetic. Syntax: `(char-alphabetic? char)`
*   `char-numeric?`: (built-in, predicate) Tests if `char` is numeric. Syntax: `(char-numeric? char)`
*   `char-whitespace?`: (built-in, predicate) Tests if `char` is whitespace. Syntax: `(char-whitespace? char)`
*   `char-upper-case?`: (built-in, predicate) Tests if `char` is an uppercase letter. Syntax: `(char-upper-case? char)`
*   `char-lower-case?`: (built-in, predicate) Tests if `char` is a lowercase letter. Syntax: `(char-lower-case? char)`
*   `char=?`: (built-in) Character equality. Syntax: `(char=? char1 char2)`
*   `char<?`: (built-in) Character less than (based on Unicode value). Syntax: `(char<? char1 char2)`
*   `char>?`: (built-in) Character greater than (based on Unicode value). Syntax: `(char>? char1 char2)`
*   `char<=?`: (built-in) Character less than or equal (based on Unicode value). Syntax: `(char<=? char1 char2)`
*   `char>=?`: (built-in) Character greater than or equal (based on Unicode value). Syntax: `(char>=? char1 char2)`
*   `char-ci=?`: (built-in) Case-insensitive character equality. Syntax: `(char-ci=? char1 char2)`
*   `char-ci<?`: (built-in) Case-insensitive character less than. Syntax: `(char-ci<? char1 char2)`
*   `char-ci>?`: (built-in) Case-insensitive character greater than. Syntax: `(char-ci>? char1 char2)`
*   `char-ci<=?`: (built-in) Case-insensitive character less than or equal. Syntax: `(char-ci<=? char1 char2)`
*   `char-ci>=?`: (built-in) Case-insensitive character greater than or equal. Syntax: `(char-ci>=? char1 char2)`
*   `char-upcase`: (built-in) Converts `char` to uppercase. Syntax: `(char-upcase char)`
*   `char-downcase`: (built-in) Converts `char` to lowercase. Syntax: `(char-downcase char)`

### Booleans & General Predicates
*   `#t`, `#f`: Literal boolean values for true and false.
*   `not`: (built-in) Logical negation. `(not #f)` is `#t`; `(not <any-other-value>)` is `#f`. Syntax: `(not obj)`
*   `boolean?`: (built-in, predicate) Tests if `obj` is `#t` or `#f`. Syntax: `(boolean? obj)`
*   `symbol?`: (built-in, predicate) Tests if `obj` is a symbol. Syntax: `(symbol? obj)`
*   `procedure?`: (built-in, predicate) Tests if `obj` is a procedure (either built-in or a lambda-defined closure). Syntax: `(procedure? obj)`
*   `eq?`: (built-in) Identity comparison (tests if `obj1` and `obj2` refer to the same memory location). Syntax: `(eq? obj1 obj2)`
*   `equal?`: (built-in) Structural equality (recursively compares the content of lists, strings, and numbers). Syntax: `(equal? obj1 obj2)`

### Input/Output & System
*   `display`: (built-in) Prints `obj` to standard output. Strings are printed without quotes. Syntax: `(display obj)`
*   `newline`: (built-in) Prints a newline character to standard output. Syntax: `(newline)`
*   `write`: (built-in) Prints `obj`'s external representation (parsable by `read`) to standard output. Syntax: `(write obj)`
*   `display-markdown`: (built-in) Displays `obj` as Markdown (primarily for use in the Jupyter kernel). Syntax: `(display-markdown obj)`
*   `display-html`: (built-in) Displays `obj` as HTML (primarily for use in the Jupyter kernel). Syntax: `(display-html obj)`
*   `load`: (built-in) Loads and evaluates expressions from a file specified by `filename-string`. Syntax: `(load filename-string)`
*   `read`: (built-in) Reads one S-expression from standard input. Syntax: `(read)`
*   `current-time`: (built-in) Returns a high-resolution time object (typically a list of seconds and microseconds/nanoseconds). Syntax: `(current-time)`
*   `gettimeofday`: (built-in) Returns `(list seconds microseconds)` representing the current time. Syntax: `(gettimeofday)`
*   `time`: (built-in) Returns seconds since the Unix epoch as a number. Syntax: `(time)`
*   `random-integer`: (built-in) Returns a random integer between `min` and `max` (inclusive). Syntax: `(random-integer min max)`
*   `error?`: (built-in, predicate) Tests if `obj` is an error object. Syntax: `(error? obj)`
*   `environment?`: (built-in, predicate) Tests if `obj` is an environment object. Syntax: `(environment? obj)`

### Symbolic Computation (Standard Library - `stdsymlisp/symbolics.scm`)
*   `simplify`: Simplifies `expr` according to algebraic and trigonometric rules. Syntax: `(simplify expr)`
*   `expand`: Expands `expr` (e.g., distributive property, binomial expansion). Syntax: `(expand expr)`
*   **Symbolic Constants**: The symbols `e` (Euler's number), `pi` (mathematical constant pi), and `i` (imaginary unit) are recognized as symbolic constants within the symbolic computation system.
*   **Expression Predicates**: These functions test the structure or type of a symbolic `expr`.
    *   `constant? expr`: Tests if `expr` is a numeric constant.
    *   `variable? expr`: Tests if `expr` is a symbol that acts as a variable.
    *   `atomic-expr? expr`: Tests if `expr` is an atom (constant or variable).
    *   `compound-expr? expr`: Tests if `expr` is a compound expression (a list representing a function call).
    *   `sum? expr`: Tests if `expr` is an addition operation (e.g., `(+ a b)`).
    *   `product? expr`: Tests if `expr` is a multiplication operation (e.g., `(* a b)`).
    *   `power? expr`: Tests if `expr` is an exponentiation operation (e.g., `(^ a b)`).
    *   `negation? expr`: Tests if `expr` is a negation (e.g., `(- a)`).
    *   `difference? expr`: Tests if `expr` is a subtraction (e.g., `(- a b)`).
    *   `quotient? expr`: Tests if `expr` is a division (e.g., `(/ a b)`).
    *   `abs? expr`: Tests if `expr` is an absolute value function call `(abs ...)`.
    *   `ln? expr`: Tests if `expr` is a natural logarithm function call `(ln ...)`.
    *   `exp? expr`: Tests if `expr` is an exponential function call `(exp ...)`.
    *   `sin? expr`: Tests if `expr` is a sine function call `(sin ...)`.
    *   `cos? expr`: Tests if `expr` is a cosine function call `(cos ...)`.
    *   `tan? expr`: Tests if `expr` is a tangent function call `(tan ...)`.
    *   `sqrt? expr`: Tests if `expr` is a square root function call `(sqrt ...)`.
*   **Expression Accessors**: These functions extract parts of a symbolic `expr`.
    *   `operator expr`: Returns the operator of a compound `expr` (e.g., `+` from `(+ a b)`).
    *   `operands expr`: Returns the list of operands of a compound `expr` (e.g., `(a b)` from `(+ a b)`).
    *   `terms expr`: For a sum `expr`, returns the list of terms.
    *   `factors expr`: For a product `expr`, returns the list of factors.
    *   `base expr`: For a power `expr`, returns the base.
    *   `exponent expr`: For a power `expr`, returns the exponent.
    *   `negated-expr expr`: For a negation `expr`, returns the expression being negated.
    *   `minuend expr`: For a difference `expr`, returns the minuend.
    *   `subtrahend expr`: For a difference `expr`, returns the subtrahend.
    *   `quotient-numerator expr`: For a quotient `expr`, returns the numerator.
    *   `quotient-denominator expr`: For a quotient `expr`, returns the denominator.
    *   `abs-arg expr`: For an `abs` call, returns its argument.
    *   `ln-arg expr`: For an `ln` call, returns its argument.
    *   `exp-arg expr`: For an `exp` call, returns its argument.
    *   `sin-arg expr`: For a `sin` call, returns its argument.
    *   `cos-arg expr`: For a `cos` call, returns its argument.
    *   `tan-arg expr`: For a `tan` call, returns its argument.
    *   `sqrt-arg expr`: For a `sqrt` call, returns its argument.

### Infix/Prefix Conversion (Standard Library - `stdsymlisp/infix.scm`)
*   `string->prefix-expr`: Converts an `infix-string` to a SymLisp prefix S-expression. Handles basic arithmetic operators (`+`, `-`, `*`, `/`), exponentiation (`^`), and function calls like `f(a,b)`. Syntax: `(string->prefix-expr infix-string)`
*   `prefix-expr->infix-string`: Converts a `prefix-expr` (SymLisp S-expression) back to an infix string representation. Syntax: `(prefix-expr->infix-string prefix-expr)`
*   `prefix-expr->markdown-latex`: Converts a `prefix-expr` to a Markdown string containing LaTeX math representation (e.g., produces "$ \sin(x) + 1 $" from `(+ (sin x) 1)`). Syntax: `(prefix-expr->markdown-latex prefix-expr)`

### Utilities (Standard Library - `stdsymlisp/lists.scm`)
*   `values`: Returns a list of its arguments. This is primarily used with `call-with-values` to handle multiple return values. Syntax: `(values v1 v2 ...)`
*   `call-with-values`: Calls `producer-thunk` (a thunk that is expected to call `values`). Then, `consumer-proc` (a procedure) is called with the values produced by `producer-thunk` as its arguments. Syntax: `(call-with-values producer-thunk consumer-proc)`

