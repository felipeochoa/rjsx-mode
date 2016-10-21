# Notes on the js2-mode parser


* Initialized/called by `js2-parse`
* Only one active scanner/token stream per buffer

* The lexer (aka scanner in the docs) runs JIT before the parser
  itself
* State is managed using buffer-local variables. The main variables
  that the scanner/parsers use are:

  - `js2-ti-tokens`: Vector of recently lexed (not necessarily parsed)
  - `js2-ti-tokens-cursor`: Pointer into `js2-ti-tokens` with the
    "current" token
  - `js2-ti-lookahead`: Counter recording how many tokens have been
    lexed but not parsed

`js2-get-token` is used to advance the current token cursor, which
automatically pulls in more tokens if `js2-ti-tokens` is out

## Lexer/Scanner:

* `js2-get-token` is the function called by most of the token-dealing
  code. It looks in the buffer, and dispatches to
  `js2-get-token-internal` if it needs to read a new token
* `js2-get-token-internal` calls the scanner (using
  `js2-get-token-internal-1`), processes comments, and fontifies
  errors and keywords


## `js2-parse`

1. initializes (buffer-local) state variables
3. calls `js2-do-parse`
4. does error reporting
5. runs post-parse hooks
6. returns AST from `js2-do-parse`


## `js2-do-parse`

This function creates the ast instance and runs a while loop that
parses all of the top-level blocks. Functions are parsed using
`js2-parse-function-stmt` (called after consuming the "function"
keyword) and other statements are called using `js2-parse-statement`.

There's also the "use strict" handling here and some comment-handling
stuff. At the end, post-parse-callbacks are run and undeclared-vars
are highlighted.

Local variables:

* **root**: `(make-js2-ast-root :buffer (buffer-name) :pos pos)`
* **tt**: `(js2-get-token)`
* **n**: One of `(js2-parse-function-expr)` or `(js2-parse-function-stmt)`


## `js2-parse-function-stmt`

* Checks for a generator function `star-p`
* Ensure function is not unnamed  `(js2-must-match-name "msg.unnamed.function.stmt")`
* Parses out the Rhino-specific "member expression", if any
* Parses out the left paren
* Calls `(js2-parse-function 'FUNCTION_STATEMENT pos star-p async-p name)` and returns its
  value.


## `js2-parse-function`

Calls `js2-parse-function-internal` and handles re-parsing of the function if strict
directives are found

## `js2-parse-function-internal`

1. Binds its name (if it's a statement) in the parent scope
2. Creates new scope (incl. handling dynamic scope)
3. Parses function params using `js2-parse-function-params`
4. Checks for arrow syntax
5. Dispatch body parsing based on whether there are curly braces (deprecated SpiderMonkey
   feature) --> mostly use `(js2-parse-function-body fn-node)`
6. Check for inconsistent returns
7. Binds its name (maybe) in its own scope


## `js2-parse-function-body`

1. Creates a new block node
2. Pushes statement nodes `(js2-parse-statement)` onto the block node
3. Check for use strict directive and throw 'reparse t if found

## `js2-parse-statement`

* Checks for user input to interrupt
* Checks for no-side-effects code
* Parses statements using `js2-statement-helper`
* Dynamic dispatch based on token type using `(aref js2-parsers tt)`

## `js2-parse-const-var`

* Parses variable declarations and wraps it ina statement node using
  `make-js2-expr-stmt-node`
* Calls `js2-parse-variables` for the actual parsing

## `js2-parse-variables`

* Handles destructuring, name assignment in scope, declaration checks
* Uses `js2-parse-destruct-primary-expr` for destructuring assignments
* Uses `js2-parse-assign-expr` for the assignment expressions (i.e., the RHS)

## `js2-parse-assign-expr`

* For yield expressions calls `js2-parse-return-or-yield`
* For assignment (incl. augmented) calls itself recursively and ensures proper identifiers
* For arrow functions, calls `js2-parse-function` (detecting whether it's async or not)
* Everything else is handled through `js2-parse-cond-expr`

## `js2-parse-cond-expr`

Uses `js2-parse-or-expr` to parse out one expression, then checks for the "? __ : __"
pattern to create a cond-node

## `js2-parse-or-expr`, `js2-parse-and-expr`, `js2-parse-bit-or-expr`, ...

Classic recursive descent parser. In order of priority:

1. OR
2. AND
3. BIT OR
4. BIT XOR
5. BIT AND
6. EQ [EQ, NE, SHEQ, SHNE]
7. REL [IN, INSTANCEOF, LE, LT, GE, GT]
8. SHIFT [LSH, URSH, RSH]
9. ADD/SUB
10. MUL/DIV/MOD
11. EXPON [incl. fix for right-associativity]
12. UNARY [VOID, NOT, BITNOT, TYPEOF, POS, NEG, INC, DEC, DELPROP]

Unary is a more complicated parser, which checks for valid increment/decrement sequences,
that delprop ("delete a.b") is called on a valid property, etc. It also dispatches parsing
to `(js2-parse-member-expr-tail t (js2-parse-xml-initializer))` if it finds an XML
snippet, and finally dispatches to `(js2-parse-member-expr t)`

## `js2-parse-member-expr`

* Handles calls using "new" directly, or dispatches to `js2-parse-primary-expr`

## `js2-parse-primary-expr`

Parse a literal (leaf) expression of some sort:

* class
* function/async function
* Array literals or comprehensions
* Object literal
* Let expression
* Parenthetisized or generator comp
* XMLATTR [E4X attribute expression using @ sign, e.g., `@attr`]
* names
* numbers
* strings
* template literals
* regexp literal
* null, this, super, false, true
* tripledot (must be arrow function in rest param)
* reserved keyword
* EOF
* scanner error

## `js2-parse-xml-initializer`

Parses a literal XML fragment. The tokenizer changes here to `js2-get-next-xml-token`,
which emits the following token types:

* js2-ERROR
* js2-XML: Generic XML fragment, including CDATA, comments, doctype. Stops at any bare `{`
* js2-XMLEND: Found the last close-tag

The tokenizer tracks significantly more state than it emits (in-tag, in-attr,
num-open-tags, etc.)

After every `js2-XML` token, `js2-get-next-xml-token` makes it into a string (not
fontified as such), and uses `js2-parse-expr` (and the normal tokenizer) to parse the
expression in curly braces. It then wraps the expression in a `js2-xml-js-expr-node`.


**Hypothesis**: Extend JS2-JSX mode and advice `js2-parse-xml-initializer` to our own XML
parser when in said mode. Re-use as much of the js2 parser (e.g., js2-parse-member-expr)
and use the grammar at https://facebook.github.io/jsx/ to coordinate between them
