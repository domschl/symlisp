[![SymLisp CI](https://github.com/domschl/symlisp/actions/workflows/ci.yml/badge.svg)](https://github.com/domschl/symlisp/actions/workflows/ci.yml)

T.B.D.

Again another Scheme, this time by Gemini 2.5 Pro (preview)

## todos

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

Additional Ideas:

- [ ] String and infix<->prefix
- [ ] Macros: define-syntax, let-syntax
- [ ] Check against r7rs
- [ ] Standard library
- [ ] Symbolics libs
- [ ] Kernel
- [ ] MCP/agnt
