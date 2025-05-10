[![SymLisp CI](https://github.com/domschl/symlisp/actions/workflows/ci.yml/badge.svg)](https://github.com/domschl/symlisp/actions/workflows/ci.yml)

T.B.D.

Again another Scheme, this time:

- Main interpreter by Gemini 2.5 Pro (preview)
- Jupyter kernel by Claude 3.7 thinking

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

B. Fractional Exponents, sqrt, and abs Function

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


-   **Negative Bases with Fractional Exponents**: Implement the rule in `simplify-power` for `(^ neg-const rational-exp)` to introduce `i` (e.g., `(^ -4 (1/2)) -> (* 2 i)`). This will involve checking if the exponent is of the form `num/(2k*den)` where `num/den` is odd/odd.
-   **`abs` Function**: Introduce `(abs x)` and its simplification rules.
-   **`sqrt(x^2)`-like rules**: Implement `(^ (base^N) (1/N))` simplifying to `base` or `(abs base)`.


C. Exponential and Logarithmic Functions (Natural)

- exp Function (Exponential):
  - Represent as (exp x). This is generally clearer than always using (^ e x) for rules specific to the exponential function, e (from section A) is the base.
- Add (ln x) as a recognized function.
  - Simplification rules in simplify: (ln 1) -> 0, (ln e) -> 1
  - (ln (exp x)) -> x (or (ln (^ e x)) -> x if using power form for exp).
  - ((exp (ln x)) -> x (or (^ e (ln x)) -> x) (Domain: x > 0. Consider how to handle this. For now, you might assume valid inputs or simplify symbolically).
  - (ln (^ base exponent)) -> (* exponent (ln base)) (Domain considerations for base > 0).
  - (ln (* f1 f2)) -> (+ (ln f1) (ln f2)) (Domain: f1, f2 > 0).

D. Trigonometric Functions

- Functions: Introduce (sin x), (cos x), (tan x).
- pi (Constant): Use pi (from section A).
- Simplification for Specific Values (Numerical Evaluation):
  - In simplify (when the argument to sin, cos, tan is a known constant):
    - sin(0) -> 0, cos(0) -> 1, tan(0) -> 0.
    - sin(pi) -> 0, cos(pi) -> -1, tan(pi) -> 0.
    - sin((/ pi 2)) -> 1, cos((/ pi 2)) -> 0. (Tan undefined here).
    - Extend for other simple multiples/fractions of pi (e.g., pi/4, pi/3, pi/6).
- Trigonometric Identities:
  - Pythagorean Identity: In simplify-sum, add a rule to recognize (+ (^ (sin x) 2) (^ (cos x) 2)) and simplify it to 1.
  - Tan definition: Consider if (tan x) should simplify to (/ (sin x) (cos x)) as a canonical form, or if simplify should recognize this pattern.
- Euler's Formula:
  - The identity (exp (* i x)) <-> (+ (cos x) (* i (sin x))) (or (^ e (* i x))) is powerful for deriving many trig identities but adds complexity to simplify as it can convert between exponential and trigonometric forms. 


### Phase 4: Differentiation (differentiate or diff)

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

### Phase 5: Factorization (factorize)

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

### Phase 6

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

