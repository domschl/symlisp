[![SymLisp CI](https://github.com/domschl/symlisp/actions/workflows/ci.yml/badge.svg)](https://github.com/domschl/symlisp/actions/workflows/ci.yml)

## SymLisp: A Scheme-like Interpreter for Symbolic Computation written entirely by AI systems

SymLisp is a lightweight, embeddable Scheme-like interpreter with a focus on symbolic computation capabilities. It is written in C  (core interpreter logic), written by Gemini 2.5 Pro preview, it's own Scheme (Symbolics and standard library extensions), written by Gemini 2.5 Pro preview, and Python (Jupyter Kernel) by Claude 3.7.

The system includes a growing symbolic algebra system, also written in Scheme, allowing for expression simplification and expansion.

### Features
*   Scheme-like syntax and core functionality.
*   Support for rational numbers (via GMP) and arbitrary-precision integers.
*   Basic symbolic manipulation: simplification and expansion of algebraic and trigonometric expressions.
*   UTF-8 support for strings and symbols.
*   A simple Read-Eval-Print Loop (REPL).
*   A Jupyter kernel for interactive notebook usage, supporting Markdown and LaTeX output for symbolic expressions.
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

## Language Reference

SymLisp provides a set of built-in functions (implemented in C) and standard library functions (implemented in Scheme, typically loaded from the `stdsymlisp/` directory).

### Special Forms
These are fundamental syntactic constructs recognized directly by the interpreter:
*   `quote` or `'`: Prevents evaluation of an expression.
*   `lambda`: Defines an anonymous procedure.
*   `define`: Defines variables and procedures in the current environment.
*   `if`: Conditional execution.
*   `set!`: Assigns a new value to an existing binding.
*   `begin`: Sequences expressions, returning the value of the last.
*   `let`, `let*`, `letrec`: Local bindings.
*   `cond`: Multi-branch conditional.
*   `and`, `or`: Logical conjunction and disjunction (short-circuiting).
*   `define-syntax`: Defines macros (currently non-hygienic, limited scope).

### Core Language & Evaluation
*   `eval expr [env]`: (built-in) Evaluates `expr`. If `env` is provided, evaluates in that environment.
*   `apply proc args-list`: (built-in) Applies `proc` to `args-list`.
*   `interaction-environment`: (built-in) Returns the global REPL environment.
*   `environment`: (built-in) Creates a new, empty environment whose parent is the interaction environment.
*   `error message-string [obj ...]`: (built-in) Signals an error.

### List Processing
*   `cons obj1 obj2`: (built-in) Creates a new pair.
*   `list obj ...`: (built-in) Creates a new list.
*   `cons* obj1 obj2 ...`: (built-in) `(cons obj1 (cons obj2 ... (cons objN-1 objN) ...))`.
*   `car pair`: (built-in) Returns the first element of a pair.
*   `cdr pair`: (built-in) Returns the second element of a pair.
*   `caar`, `cadr`, `cddr`, `caddr`: (built-ins) Compositions of `car` and `cdr`.
*   `set-car! pair obj`: (built-in) Modifies the car of `pair`.
*   `set-cdr! pair obj`: (built-in) Modifies the cdr of `pair`.
*   `pair? obj`: (built-in, predicate from `sl_predicates.c`) Tests if `obj` is a pair.
*   `null? obj`: (built-in, predicate from `sl_predicates.c`) Tests if `obj` is the empty list.
*   `list? obj`: (built-in, predicate from `sl_predicates.c`) Tests if `obj` is a proper list.
*   `length list`: (built-in) Returns the length of `list`.
*   `append list ...`: (built-in) Concatenates lists. The last argument becomes the tail of the new list.
*   `reverse list`: (built-in) Returns a new list with elements in reverse order.
*   `assoc key alist`: (standard library, `lists.scm`) Finds the first pair in `alist` whose car is `equal?` to `key`.
*   `memq obj list`: (standard library, `lists.scm`) Searches `list` for `obj` using `eq?`.
*   `member obj list`: (standard library, `lists.scm`) Searches `list` for `obj` using `equal?`.
*   `exists pred list`: (standard library, `lists.scm`) Returns the first true value returned by `(pred element)` for elements in `list`.
*   `find-if pred list`: (standard library, `lists.scm`) Returns the first element in `list` that satisfies `pred`.
*   `remove item list`: (standard library, `lists.scm`) Returns a new list with the first `equal?` occurrence of `item` removed.
*   `count pred list`: (standard library, `lists.scm`) Counts elements in `list` satisfying `pred`.
*   `remove-duplicates list`: (standard library, `lists.scm`) Removes duplicate elements from `list` (uses `equal?`).
*   `list-take list k`: (standard library, `lists.scm`) Returns the first `k` elements of `list`.
*   `list-drop list k`: (standard library, `lists.scm`) Returns `list` after dropping the first `k` elements.
*   `merge-sorted-lists list1 list2 pred`: (standard library, `lists.scm`) Merges two sorted lists.
*   `list-sort pred list`: (standard library, `lists.scm`) Sorts `list` using `pred`.
*   `iota count [start step]`: (standard library, `lists.scm`) Generates a list of numbers.

### Higher-Order Functions
*   `map proc list1 [list2 ...]`: (built-in, from `sl_higher_order.c`) Applies `proc` to corresponding elements of lists.
*   `filter pred list`: (built-in, from `sl_higher_order.c`) Returns a list of elements from `list` satisfying `pred`.
*   `for-each proc list1 [list2 ...]`: (built-in, from `sl_higher_order.c`) Applies `proc` to elements for side effects.
*   `fold-left proc initial list1 [list2 ...]`: (built-in, from `sl_higher_order.c`) Left-associative fold.
*   `reduce proc initial list1 [list2 ...]`: (built-in, alias for `fold-left`).
*   `fold-right proc initial list1 [list2 ...]`: (built-in, from `sl_higher_order.c`) Right-associative fold.
*   `filter-map proc list`: (standard library, `lists.scm`) Applies `proc`, collects non-`#f` results.

### Numbers & Arithmetic
*   `+ [num ...]`: (built-in) Addition. `(+)` is 0.
*   `- num1 [num ...]`: (built-in) Subtraction. `(- x)` is negation.
*   `* [num ...]`: (built-in) Multiplication. `(*)` is 1.
*   `/ num1 [num ...]`: (built-in) Division. `(/ x)` is 1/x.
*   `modulo int1 int2`: (built-in) Modulo operation (result has sign of `int2`, R7RS floor).
*   `remainder int1 int2`: (built-in) Remainder operation (result has sign of `int1`, R7RS truncate).
*   `denominator num`: (built-in) Returns the denominator of `num`.
*   `numerator num`: (built-in) Returns the numerator of `num`.
*   `quotient int1 int2`: (built-in) Integer division (truncates towards zero).
*   `gcd int ...`: (built-in) Greatest Common Divisor. `(gcd)` is 0.
*   `lcm int ...`: (built-in) Least Common Multiple. `(lcm)` is 1.
*   `expt base exponent`: (built-in) `base` raised to the power of `exponent` (integer exponents).
*   `square num`: (built-in) `num * num`.
*   `exact-integer-sqrt n`: (built-in) Returns `(list s r)` where `s*s + r = n`.
*   `abs num`: (built-in) Absolute value.
*   `max num ...`: (built-in) Maximum of numbers.
*   `min num ...`: (built-in) Minimum of numbers.
*   `= num1 num2`: (built-in) Numeric equality.
*   `> num1 num2`: (built-in) Numeric greater than.
*   `< num1 num2`: (built-in) Numeric less than.
*   `>= num1 num2`: (built-in) Numeric greater than or equal.
*   `<= num1 num2`: (built-in) Numeric less than or equal.
*   `number? obj`: (built-in, predicate from `sl_predicates.c`) Tests if `obj` is a number.
*   `integer? obj`: (built-in, predicate from `sl_predicates.c`) Tests if `obj` is an integer.
*   `odd? integer`: (built-in, predicate from `sl_predicates.c`) Tests if `integer` is odd.
*   `even? integer`: (built-in, predicate from `sl_predicates.c`) Tests if `integer` is even.
*   `prime? integer`: (built-in, predicate from `sl_predicates.c`) Probabilistic primality test.
*   `zero? num`: (standard library, `lists.scm` but numeric) Tests if `num` is zero.
*   `number->string num [radix]`: (built-in, from `sl_strings.c`) Converts `num` to string. Radix other than 10 for integers only.
*   `string->number str [radix]`: (built-in, from `sl_strings.c`) Converts `str` to number or `#f`. Supports integers (radix 2-62), fractions `N/D` (radix 10), decimals (radix 10).
*   `float num [precision]`: (built-in) Returns string representation of `num` to `precision` decimal places.
*   `prime-factors n`: (built-in) Returns a list of prime factors of integer `n`.
*   `next-prime n`: (built-in) Returns the smallest prime strictly greater than integer `n`.

### Strings
*   `string char ...`: (built-in, from `sl_strings.c`) Creates a string from characters.
*   `string-length str`: (built-in, from `sl_strings.c`) Number of Unicode code points in `str`.
*   `string-ref str k`: (built-in, from `sl_strings.c`) Character at 0-based index `k`.
*   `string-append str ...`: (built-in, from `sl_strings.c`) Concatenates strings.
*   `substring str start [end]`: (built-in, from `sl_strings.c`) Extracts substring (indices are code point based).
*   `string-join list-of-strings delimiter-string`: (built-in, from `sl_strings.c`) Joins strings with a delimiter.
*   `string-split str delimiter-char`: (built-in, from `sl_strings.c`) Splits `str` by a single `delimiter-char`. Consecutive delimiters yield empty strings.
*   `string-tokenize str delimiter-set-string`: (built-in, from `sl_strings.c`) Splits `str` by any char in `delimiter-set-string`. Consecutive delimiters are treated as one.
*   `string-upcase str`: (built-in, from `sl_strings.c`) Converts `str` to uppercase (simple Unicode mapping).
*   `string-downcase str`: (built-in, from `sl_strings.c`) Converts `str` to lowercase (simple Unicode mapping).
*   `string=? str1 str2`: (built-in, from `sl_strings.c`) Case-sensitive string equality.
*   `string<? str1 str2`: (built-in, from `sl_strings.c`) Case-sensitive string less than.
*   `string>? str1 str2`: (built-in, from `sl_strings.c`) Case-sensitive string greater than.
*   `string<=? str1 str2`: (built-in, from `sl_strings.c`) Case-sensitive string less than or equal.
*   `string>=? str1 str2`: (built-in, from `sl_strings.c`) Case-sensitive string greater than or equal.
*   `string-ci=? str1 str2`: (built-in, from `sl_strings.c`) Case-insensitive string equality.
*   `string-ci<? str1 str2`: (built-in, from `sl_strings.c`) Case-insensitive string less than.
*   `string-ci>? str1 str2`: (built-in, from `sl_strings.c`) Case-insensitive string greater than.
*   `string-ci<=? str1 str2`: (built-in, from `sl_strings.c`) Case-insensitive string less than or equal.
*   `string-ci>=? str1 str2`: (built-in, from `sl_strings.c`) Case-insensitive string greater than or equal.
*   `string? obj`: (built-in, predicate from `sl_predicates.c`) Tests if `obj` is a string.
*   `string->list str`: (built-in, from `sl_strings.c`) Converts `str` to a list of characters.
*   `list->string list-of-chars`: (built-in, from `sl_strings.c`) Converts a list of characters to `str`.
*   `symbol->string sym`: (built-in, from `sl_strings.c`) Converts `sym` to its string name.
*   `string->symbol str`: (built-in, from `sl_strings.c`) Converts `str` to a symbol (interned).
*   `expr->string expr`: (built-in, from `sl_strings.c`) Converts `expr` to its external string representation.
*   `string->expr str`: (built-in, from `sl_strings.c`) Parses `str` into a single SymLisp expression.
*   `string->infix-tokens str`: (built-in, from `sl_strings.c`) Tokenizes an infix string (primarily for `string->prefix-expr`).

### Characters
*   `char? obj`: (built-in, predicate from `sl_predicates.c`) Tests if `obj` is a character.
*   `char-alphabetic? char`: (built-in, predicate from `sl_predicates.c`)
*   `char-numeric? char`: (built-in, predicate from `sl_predicates.c`)
*   `char-whitespace? char`: (built-in, predicate from `sl_predicates.c`)
*   `char-upper-case? char`: (built-in, predicate from `sl_predicates.c`)
*   `char-lower-case? char`: (built-in, predicate from `sl_predicates.c`)
*   `char=? char1 char2`: (built-in, from `sl_strings.c`) Character equality.
*   `char<? char1 char2`: (built-in, from `sl_strings.c`) Character less than.
*   `char>? char1 char2`: (built-in, from `sl_strings.c`) Character greater than.
*   `char<=? char1 char2`: (built-in, from `sl_strings.c`) Character less than or equal.
*   `char>=? char1 char2`: (built-in, from `sl_strings.c`) Character greater than or equal.
*   `char-ci=? char1 char2`: (built-in, from `sl_strings.c`) Case-insensitive char equality.
*   `char-ci<? char1 char2`: (built-in, from `sl_strings.c`) Case-insensitive char less than.
*   `char-ci>? char1 char2`: (built-in, from `sl_strings.c`) Case-insensitive char greater than.
*   `char-ci<=? char1 char2`: (built-in, from `sl_strings.c`) Case-insensitive char less than or equal.
*   `char-ci>=? char1 char2`: (built-in, from `sl_strings.c`) Case-insensitive char greater than or equal.
*   `char-upcase char`: (built-in, from `sl_strings.c`) Converts `char` to uppercase.
*   `char-downcase char`: (built-in, from `sl_strings.c`) Converts `char` to lowercase.

### Booleans & General Predicates
*   `#t`, `#f`: Boolean true and false.
*   `not obj`: (built-in) Logical negation. `(not #f)` is `#t`, otherwise `#f`.
*   `boolean? obj`: (built-in, predicate from `sl_predicates.c`) Tests if `obj` is `#t` or `#f`.
*   `symbol? obj`: (built-in, predicate from `sl_predicates.c`) Tests if `obj` is a symbol.
*   `procedure? obj`: (built-in, predicate from `sl_predicates.c`) Tests if `obj` is a procedure (built-in or closure).
*   `eq? obj1 obj2`: (built-in) Identity comparison (pointer equality).
*   `equal? obj1 obj2`: (built-in) Structural equality.

### Input/Output & System
*   `display obj`: (built-in, from `sl_output.c`) Prints `obj` to standard output (strings without quotes).
*   `newline`: (built-in, from `sl_output.c`) Prints a newline to standard output.
*   `write obj`: (built-in, from `sl_output.c`) Prints `obj`'s external representation to standard output.
*   `display-markdown obj`: (built-in, from `sl_output.c`) Displays `obj` as Markdown (primarily for Jupyter).
*   `display-html obj`: (built-in, from `sl_output.c`) Displays `obj` as HTML (primarily for Jupyter).
*   `load filename-string`: (built-in) Loads and evaluates expressions from a file.
*   `read`: (built-in) Reads one S-expression from standard input.
*   `current-time`: (built-in) Returns a high-resolution time object/list.
*   `gettimeofday`: (built-in) Returns `(list seconds microseconds)`.
*   `time`: (built-in) Returns seconds since epoch as a number.
*   `random-integer min max`: (built-in) Returns a random integer between `min` and `max` inclusive.
*   `error? obj`: (built-in, predicate from `sl_predicates.c`) Tests if `obj` is an error object.
*   `environment? obj`: (built-in, predicate from `sl_predicates.c`) Tests if `obj` is an environment.

### Symbolic Computation (Standard Library - `stdsymlisp/symbolics.scm`)
*   `simplify expr`: Simplifies `expr` according to algebraic and trigonometric rules.
*   `expand expr`: Expands `expr` (e.g., distributive property, binomial expansion).
*   **Symbolic Constants**: `e`, `pi`, `i` are recognized.
*   **Predicates**:
    *   `constant? expr`, `variable? expr`, `atomic-expr? expr`, `compound-expr? expr`
    *   `sum? expr`, `product? expr`, `power? expr`, `negation? expr`, `difference? expr`, `quotient? expr`
    *   `abs? expr`, `ln? expr`, `exp? expr`, `sin? expr`, `cos? expr`, `tan? expr`, `sqrt? expr`
*   **Accessors**:
    *   `operator expr`, `operands expr`
    *   `terms expr`, `factors expr`
    *   `base expr`, `exponent expr`
    *   `negated-expr expr`, `minuend expr`, `subtrahend expr`
    *   `quotient-numerator expr`, `quotient-denominator expr`
    *   `abs-arg expr`, `ln-arg expr`, `exp-arg expr`, `sin-arg expr`, `cos-arg expr`, `tan-arg expr`, `sqrt-arg expr`

### Infix/Prefix Conversion (Standard Library - `stdsymlisp/infix.scm`)
*   `string->prefix-expr infix-string`: Converts an `infix-string` to a SymLisp prefix S-expression. Handles basic arithmetic, `^`, and function calls like `f(a,b)`.
*   `prefix-expr->infix-string prefix-expr`: Converts a `prefix-expr` back to an infix string.
*   `prefix-expr->markdown-latex prefix-expr`: Converts a `prefix-expr` to a Markdown string containing LaTeX math (e.g., `$ \sin(x) + 1 $`).

### Utilities (Standard Library - `stdsymlisp/lists.scm`)
*   `values v1 v2 ...`: Returns a list of its arguments (used for `call-with-values`).
*   `call-with-values producer-thunk consumer-proc`: Calls `producer-thunk` (which should call `values`), then calls `consumer-proc` with the produced values as arguments.

## Todos

- [x] Environment structure and basic operations (sl_env_create, sl_env_define, sl_env_lookup).
- [x] Basic eval handling self-evaluating types and symbol lookup.
- [x] Implement quote special form in eval.
- [x] Implement a few simple builtins (car, cdr, cons) and the apply logic for builtins.
- [x] Populate the global environment with these builtins. Test simple calls like (car (quote (1 2))).
- [x] Add math for +, -, *, /
- [x] Implement lambda special form (creating closures) and apply logic for closures. (Basic TCO included)
- [x] Implement define special form. Test defining and calling simple functions.
- [x] Implement if special form.
- [x] Implement set! special form.
- [x] Add API functions for environment get/set/define by C string name.
- [x] Remove reliance on fixed allocations for buffers
- [x] Implement `begin` special form (for multi-expression bodies in lambda/define).
- [x] Implement basic I/O: `display`, `newline`.
- [x] Implement file I/O: `load` function.
- [x] Implement library loading mechanism (e.g., `load-library` or auto-load from dir).
- [x] Implement more builtins: list, eq?, equal?
- [x] Implement more builtins: remainder, modulo // <<< ADDED
- [x] let and named let
- [x] Test framwork
- [x] CI with valgrind
- [x] cond
- [x] Boolean ops: or, and, not
- [x] List functions: length, append, reverse
- [x] Math: denominator, numerator, quotient, gcd, lcm, 
- [x] Math functions: abs, max, min
- [x] expt, square, exact-integer-sqrt
- [x] float approximation
- [x] Predicate tester functions
- [x] String functions
- [x] eval, write, read
- [x] Conversion between types
- [x] Implement symbol interning in sl_make_symbol so that only one symbol object exists per unique name.
- [x] Nested defines, let, let*, letrec*
- [x] Refine lambda/define syntax validation (variadics?), variable arity support (lambda)
- [x] higher-order functions, map, reduce, foldl, foldr, filter.
- [x] the `do` loop.
- [x] Conversion: rational<->string // <<< MARK AS DONE (implicitly by string->number)
- [x] Conversion: symbol<->string // <<< MARK AS DONE
- [x] Conversion: expr<->string
- [x] String case conversion functions (string-upcase, string-downcase).
- [x] Character case conversion functions (char-upcase, char-downcase).
- [x] Case-insensitive string comparison functions (string-ci=?, string-ci<?, etc.).
- [x] Character predicates (char-alphabetic?, char-numeric?, char-whitespace?, etc.).
- [x] integer?, prime?
- [x] math factorize
- [x] Refine error handling and GC integration for environments. (Initial error object mechanism added)
- [x] Eval gc crash on string->prefix-expr tests. Fixed.
- [x] Time and random numbers
- [x] Macros: define-syntax (limited scape, non-hygienic)
- [x] CMake cleanup subprojects
- [x] Jupyter Kernel
  - [x] Jupyter kernel sub-project
  - [x] output for stdio and `(display)` stderr (error-messages), extra output-modes `(display-markdown)` and `(display-html)`
- [x] String and infix<->prefix (vars, functions)
- [x] Symbolics phases 1.1, 1.2, 1.3 done, Phase 1 is complete.
- [x] Symbolix phase 2, expand, complete.
- [x] char compare
- [x] Latex output (jupyter kernel), via (display-markdown (prefix-expr->markdown-latex '(+ 1 x))), text math output, better symbolics term collection
- [x] Phase 3.A: Special functions accessors, treatment of `i`.
- [x] Phase 3.B: Fractional Exponents, sqrt, and abs Function
- [x] Phase 3.C: Exponential and Logarithmic Functions (Natural)
- [x] D. Trigonometric Functions

Larger steps:

- [ ] Symbolics libs (s.b.)
- [ ] review against standard(s)
- [ ] Better error handling: `assert-error` support, fix of many unbalanced gc_lock/removes in error paths
- [ ] single-page doc suitable as instruction for LLMs (language description and limitations) 
- [ ] MCP server (stdio based, jsonrpc 2.0)



## Symbolics steps

### Phase 3: Special functions: abs, ln, exp, sin, cos, tan, sqrt (involves constants e, pi, i)

A. [DONE] Core Constants and Imaginary Unit i

- Define Symbolic Constants: Introduce e (base of natural logarithm), pi (π), and i (imaginary unit) as distinct, recognized symbolic constants.
- Basic Properties of i: (^ i 2) -> -1: Implement this in simplify-power. This is the fundamental definition.
- Higher integer powers of i: simplify-power should also handle (^ i 3) -> (- i), (^ i 4) -> 1, (^ i n) cyclically.
- (* ... i ... i ...): simplify-product should recognize (* i i) within its factors and replace it with -1, effectively simplifying products of multiple i's.
- predicate and accessor functions for abs, ln, exp, sin, cos, tan, sqrt

B. [DONE] Fractional Exponents, sqrt, and abs Function

- Rational Exponents for ^:
  - Extend simplify-power to robustly handle rational exponents m/n.
  - Ensure (^ base 0) -> 1 and (^ base 1) -> base continue to work correctly.
- sqrt Function and Representation:
  - Introduce (sqrt base) as a recognized function.
  - Internally, simplify should convert (sqrt base) to (^ base (1/2)) for consistent rule application.
  - expr->string and expr->latex-recursive should be updated to render (^ base (1/2)) as sqrt(base) (or \sqrt{base}) for user-friendly output.
- Simplifying Roots of Integers (e.g., sqrt(12)):
  - When simplifying (^ N (1/M)) where N is an integer: Use `prime-factors` C function to get the prime factorization of N. For each prime factor p^k in N, the term becomes (^ (p^k) (1/M)) = p^(k/M). If k is a multiple of M, p^(k/M) simplifies to an integer.
- Negative Bases with Fractional Exponents (Introducing i from roots):
  - In simplify-power, for (^ base exponent) where base is a negative constant and exponent is 1/(2k) (e.g., 1/2, 1/4): Rewrite as (^ (* -1 abs-base) exponent) -> (* (^ -1 exponent) (^ abs-base exponent)). (^ -1 (1/2)) must simplify to i.
- abs Function and sqrt(x^2)-like Simplifications:
  - Introduce a symbolic function (abs x).
  - Add simplification rules for abs in simplify:
  - abs(constant) -> |constant| (e.g., abs(-5) -> 5), abs((- expr)) -> (abs expr).
  - If expr can be determined to be non-negative (e.g., (^ y 2)), then abs(expr) -> expr.
- In simplify-power for (^ (base^M) (1/N)):
  - This simplifies to (^ base (/ M N)).
  - generally, (^ (base^N) (1/N)) should be base if N is odd, and (abs base) if N is even and positive. (This assumes base can be complex or negative; if base is known real and positive, abs is not needed).
- General Power Rules:
  - Ensure existing rules like (^ (^ base m) k) -> (^ base (* m k)) and (^ (* f1 f2) k) -> (* (^ f1 k) (^ f2 k)) are robust and correctly interact with new fractional exponent logic.
  - Rendering for sqrt in infix output and latex

C. [Done] Exponential and Logarithmic Functions (Natural)

- exp Function (Exponential):
  - Represent as (exp x). This is generally clearer than always using (^ e x) for rules specific to the exponential function, e (from section A) is the base.
- Add (ln x) as a recognized function.
  - Simplification rules in simplify: (ln 1) -> 0, (ln e) -> 1
  - (ln (exp x)) -> x (or (ln (^ e x)) -> x if using power form for exp).
  - ((exp (ln x)) -> x (or (^ e (ln x)) -> x) (Domain: x > 0. Consider how to handle this. For now, you might assume valid inputs or simplify symbolically).
  - (ln (^ base exponent)) -> (* exponent (ln base)) (Domain considerations for base > 0).
  - (ln (* f1 f2)) -> (+ (ln f1) (ln f2)) (Domain: f1, f2 > 0).

D. [DONE] Trigonometric Functions

- Functions: Introduce (sin x), (cos x), (tan x).
- Simplification for Specific Values (Numerical Evaluation):
  - In simplify (when the argument to sin, cos, tan is (multiple of) a known constant):
    - sin(0) -> 0, cos(0) -> 1, tan(0) -> 0.
    - sin(pi) -> 0, cos(pi) -> -1, tan(pi) -> 0.
    - sin((/ pi 2)) -> 1, cos((/ pi 2)) -> 0.
    - tan() 0->0, pi/6 -> 1/3sqrt(3), pi/4 -> 1, etc.
    - Extend for other simple multiples/fractions of pi (e.g., pi/4, pi/3, pi/6).
- Trigonometric Identities:
  - Pythagorean Identity: In simplify-sum, add a rule to recognize (+ (^ (sin x) 2) (^ (cos x) 2)) and simplify it to 1.
  - Tan definition: (/ (sin x) (cos x)) should be recognized as (tan x)

E. (partialy done, rest deferred) Trigonometric Functions, part 2

- sin(A+B) -> sin(A)cos(B)+cos(A)sin(B)
- sin(A-B) -> sin(A)cos(B)-cos(A)sin(B)
- cos(A+B) -> cos(A)cos(B)-sin(A)sin(B)
- cos(A-B) -> cos(A)cos(B)+sin(A)sin(B)
- sin(2A) <-> 2sin(A)cos(A)
- cos(2A) -> cos^2(A)-sin^2(A) (or 2cos^2(A)-1 or 1-2sin^2(A)

- Euler's Formula: (deferred)
  - The identity (exp (* i x)) <-> (+ (cos x) (* i (sin x))) (or (^ e (* i x))) 

### Phase 4: Rule engine

### Phase 5: Differentiation (differentiate or diff)

1. Implement differentiate Function:
- Signature: (differentiate expr var)
- Basic Rules:
  - d/dx c -> 0 (if c is a constant)
  - d/dx x -> 1 (if var is x)
  - d/dx y -> 0 (if var is x and y is a different variable)
- Sum/Difference Rule: d/dx (u +/- v) -> (+/- (d/dx u) (d/dx v))
- Product Rule: d/dx (u * v) -> (+ (* u (d/dx v)) (* v (d/dx u)))
- Quotient Rule: d/dx (u / v) -> (/ (- (* v (d/dx u)) (* u (d/dx v))) (^ v 2))
- Power Rule: d/dx (^ u n) -> (* n (^ u (- n 1)) (d/dx u)) (where n can be a number or expression not containing var)
- Chain Rule (for known functions):
  - Maintain a table of derivatives for elementary functions (e.g., sin, cos, log, exp).
  - d/dx (f u) -> (* (derivative-of-f-wrt-u u) (d/dx u))
  - Example: d/dx (sin (^ x 2)) -> (* (cos (^ x 2)) (* 2 x))
- Crucially, call simplify on the result of each differentiation step.

### Phase 6: Factorization (factorize)

This is generally the most complex part.

1. Implement factorize Function:
- Extract Common Factors (for sums):
  - (+ (* a b) (* a c)) -> (* a (+ b c))
  - This requires inspecting terms of a sum and finding common parts.
- Recognize Patterns:
   - Difference of Squares: (- (^ a 2) (^ b 2)) -> (* (+ a b) (- a b))
   - Perfect Square Trinomials: (+ (^ a 2) (* 2 a b) (^ b 2)) -> (^ (+ a b) 2)
- Polynomial Factorization (start simple):
   - Factoring integers (trivial for your system).
   - Factoring quadratics ax^2 + bx + c.
   - General polynomial factorization over integers or rationals is a very advanced topic (e.g., Rational Root Theorem, Kronecker's method, Berlekamp algorithm for finite fields, Cantor-Zassenhaus). You might want to limit the scope initially.
- factorize often involves trial and error or heuristic approaches.
- May also benefit from calling simplify.

### Phase 7

- Taylor series
- limits
- integrals
- solver

## DONE TASKS

### Phase 1: [DONE] Core Expression Representation and Basic Simplification Infrastructure

1. [DONE] Define Expression Structure & Predicates:

- Solidify how expressions are represented (your S-expressions are a good start: (+ x 1), (* a b), (^ x 2)).
- Create helper predicates:
  - constant? expr (checks if it's a number)
  - variable? expr (checks if it's a symbol, excluding operator names)
  - sum? expr (e.g., (and (pair? expr) (eq? (car expr) '+)))
  - product? expr
  - power? expr
  - And so on for other operations you intend to support.
- Create accessors:
  - operator expr -> (car expr)
  - operands expr -> (cdr expr)
  - addend1 expr, addend2 expr (for binary +) or terms expr (for n-ary +)
  - multiplicand1 expr, multiplicand2 expr or factors expr
  - base expr, exponent expr (for ^)

2. [DONE] Implement a Basic simplify Function:

- This function will be the workhorse and will be expanded iteratively.
- Start with a recursive structure that dispatches based on the expression type.
- Initial Rules:
  - Constant Folding: If an expression is composed entirely of constants, evaluate it. E.g., (+ 1 2) -> 3, (* 2 3 4) -> 24.
  - Arithmetic Identities:
    - (+ x 0) -> x (and (+ 0 x) -> x)
    - (* x 1) -> x (and (* 1 x) -> x)
    - (* x 0) -> 0 (and (* 0 x) -> 0)
    - (^ x 1) -> x
    - (^ x 0) -> 1 (for x != 0)
    - (- 0 x) -> (- x) (if you have a distinct unary minus representation) or (* -1 x)
  - Recursively simplify operands: (simplify '(+ (* 2 3) x)) should first simplify (* 2 3) to 6.

3. [DONE] Canonical Forms (Early Considerations):

- Flatten Associative Operations: For n-ary operators like + and *, ensure expressions like (+ x (+ y z)) become (+ x y z).
- Order Terms: For commutative operations (+, *), decide on a canonical order for terms (e.g., constants first, then variables alphabetically). This helps in recognizing equivalent expressions like (+ x 1) and (+ 1 x).
- Represent subtraction as addition: (- a b) -> (+ a (* -1 b)). This simplifies rule writing.

### Phase 2: Expansion (expand) [DONE]

1. [DONE] Implement expand Function:
- Recursively applies expansion rules.
- Distributive Property:
  - (* a (+ b c)) -> (+ (* a b) (* a c))
  - Handle n-ary sums: (* a (+ b c d)) -> (+ (* a b) (* a c) (* a d))
- Powers of Sums (Binomial Expansion):
  - (^ (+ a b) 2) -> (+ (^ a 2) (* 2 a b) (^ b 2))
  - Start with small integer powers. General binomial theorem can be complex.
- Powers of Products/Quotients:
  - (^ (* a b) n) -> (* (^ a n) (^ b n))
  - (^ (/ a b) n) -> (/ (^ a n) (^ b n))
- Call simplify after expansion steps to clean up.

