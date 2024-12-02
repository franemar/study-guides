`<span id="hyperpolyglot">`{=html}`</span>`{=html}

# [Hyperpolyglot]( "wikilink")

`<span id="ml-dialects-and-haskell-sml-ocaml-f-haskell">`{=html}`</span>`{=html}

## ML Dialects and Haskell: SML, OCaml, F#, Haskell {#ml_dialects_and_haskell_sml_ocaml_f_haskell}

*a side-by-side reference sheet*

[grammar and invocation](#grammar-invocation "wikilink") \| [variables
and expressions](#var-expr "wikilink") \| [arithmetic and
logic](#arithmetic-logic "wikilink") \| [strings](#strings "wikilink")
\| [dates and time](#dates-time "wikilink") \|
[arrays](#arrays "wikilink") \| [lists](#lists "wikilink") \|
[tuples](#tuples "wikilink") \| [dictionaries](#dictionaries "wikilink")
\| [functions](#functions "wikilink") \| [execution
control](#execution-control "wikilink") \|
[exceptions](#exceptions "wikilink") \|
[concurrency](#concurrency "wikilink") \| [file
handles](#file-handles "wikilink") \| [files](#files "wikilink") \|
[directories](#directories "wikilink") \| [processes and
environment](#processes-environment "wikilink") \| [libraries and
namespaces](#libraries-namespaces "wikilink") \| [user-defined
types](#user-defined-types "wikilink") \| [objects](#objects "wikilink")
\| [inheritance and polymorphism](#inheritance-polymorphism "wikilink")
\| [net and web](#net-web "wikilink") \| [unit
tests](#unit-tests "wikilink") \| [debugging and
profiling](#debugging-profiling "wikilink") \| [repl](#repl "wikilink")

\<iframe src="./ML-Dialects&Haskell_SML&OCaml&F#&Haskell-table.html"
title="View table" #loading="lazy"\>`</iframe>`{=html}

`<span id="version-used">`{=html}`</span>`{=html}

## [version used](#version-used "wikilink") {#version_used}

Versions used to test the code samples in this sheet.

`<span id="show-version">`{=html}`</span>`{=html}

## [show version](#version "wikilink") {#show_version}

How to get the version.

`<span id="grammar-and-invocation">`{=html}`</span>`{=html}

## [Grammar and Invocation](#grammar-invocation "wikilink") {#grammar_and_invocation}

## [interpreter](#interpreter "wikilink")

How to run the interpreter on a file of source code.

## [shebang](#shebang "wikilink")

How to use the interpreter in a shebang.

`<span id="bytecode-compiler-and-interpreter">`{=html}`</span>`{=html}

## [bytecode compiler and interpreter](#bytecode-compiler-interpreter "wikilink") {#bytecode_compiler_and_interpreter}

How to compile source to bytecode and run it.

**ocaml:**

It is not necessary to invoke *ocamlrun* on the bytecode; the bytecode
can be executed directly because the bytecode compiler puts a shebang
invocation at the top of the file.

`<span id="native-compiler">`{=html}`</span>`{=html}

## [native compiler](#native-compiler "wikilink") {#native_compiler}

How to compile source to native code and run it.

`<span id="library-which-is-always-imported">`{=html}`</span>`{=html}

## [library which is always imported](#library-always-imported "wikilink") {#library_which_is_always_imported}

The name of the library containing the types and functions which are
always available.

`<span id="statement-terminator">`{=html}`</span>`{=html}

## [statement terminator](#statement-terminator "wikilink") {#statement_terminator}

**ocaml:**

; is the ocaml statement separator. It is not necessary at the end of the line if the following line starts with an *open* or *let* keyword or at the end of the file.

## [blocks](#blocks "wikilink")

How to define a block of statements.

`<span id="end-of-line-comment">`{=html}`</span>`{=html}

## [end-of-line comment](#end-of-line-comment "wikilink") {#end_of_line_comment}

A comment terminated by the end of the line.

`<span id="multiple-line-comment">`{=html}`</span>`{=html}

## [multiple line comment](#multiple-line-comment "wikilink") {#multiple_line_comment}

A comment with a start and end delimiter which can span multiple lines.

**ocaml:**

(\\\* \*) style comments can be nested.

`<span id="variables-and-expressions">`{=html}`</span>`{=html}

# [Variables and Expressions](#var-expr "wikilink") {#variables_and_expressions}

`<span id="write-once-variable">`{=html}`</span>`{=html}

## [write-once variable](#value "wikilink") {#write_once_variable}

How to define a variable which can be set at run-time but cannot be
modified after it is set.

`<span id="modifiable-variable">`{=html}`</span>`{=html}

## [modifiable variable](#variable "wikilink") {#modifiable_variable}

How to define a modifiable variable.

`<span id="unit-type-and-value">`{=html}`</span>`{=html}

## [unit type and value](#unit "wikilink") {#unit_type_and_value}

The notation for the unit type and the unit value. In all languages the
notation for the unit value is the same as the notation for an empty
tuple.

The unit value is a common return value of functions which perform side
effects.

`<span id="conditional-expression">`{=html}`</span>`{=html}

## [conditional expression](#conditional-expression "wikilink") {#conditional_expression}

The syntax for a conditional expression.

`<span id="branch-type-mismatch">`{=html}`</span>`{=html}

## [branch type mismatch](#branch-type-mismatch "wikilink") {#branch_type_mismatch}

What happens if the two branches of a conditional expression don't have
the same type.

## [null](#null "wikilink")

A value used somewhat paradoxically to indicate the absence of a value.

Types which can contain a null value are called *option types*.

`<span id="nullable-type">`{=html}`</span>`{=html}

## [nullable type](#nullable-type "wikilink") {#nullable_type}

`<span id="null-test">`{=html}`</span>`{=html}

## [null test](#null-test "wikilink") {#null_test}

## [coalesce](#coalesce "wikilink")

`<span id="expression-type-declaration">`{=html}`</span>`{=html}

## [expression type declaration](#expr-type-decl "wikilink") {#expression_type_declaration}

How to explicitly declare the type of an expression.

`<span id="let-in">`{=html}`</span>`{=html}

## [let ... in ...](#let-in "wikilink") {#let_in}

How to define local variables.

**ocaml:**

OCaml uses *let* to define a value and *let* with *in* to define values
in a local scope. OCaml follows the usage of the original dialect of ML
in this respect.

OCaml can define multiple values with a single *let* and *in* by
conjoining the definitions with *and*. The definitions are performed in
parallel, so later definitions cannot use the earlier definitions:

    let z =
    let x = 3
    and y = 4 in
    x * y;;

**haskell:**

Haskell uses *let* with *in* to define local scope. In addition, *ghci*
uses *let* without *in* to define values.

## [where](#where "wikilink")

How to define local variables with definitions after the expression that
uses them.

`<span id="arithmetic-and-logic">`{=html}`</span>`{=html}

# [Arithmetic and Logic](#arithmetic-logic "wikilink") {#arithmetic_and_logic}

`<span id="boolean-type">`{=html}`</span>`{=html}

## [boolean type](#boolean-type "wikilink") {#boolean_type}

The type for boolean values.

`<span id="true-and-false">`{=html}`</span>`{=html}

## [true and false](#true-false "wikilink") {#true_and_false}

The literals for true and false.

`<span id="logical-operators">`{=html}`</span>`{=html}

## [logical operators](#logical-op "wikilink") {#logical_operators}

The logical operators: *and*, *or*, and *not*.

`<span id="relational-operators">`{=html}`</span>`{=html}

## [relational operators](#relational-op "wikilink") {#relational_operators}

Operators for performing comparisons.

`<span id="min-and-max">`{=html}`</span>`{=html}

## [min and max](#min-max "wikilink") {#min_and_max}

The binary functions *min* and *max*.

`<span id="integer-types">`{=html}`</span>`{=html}

## [integer types](#int-type "wikilink") {#integer_types}

The most commonly used numeric types.

`<span id="integer-literal">`{=html}`</span>`{=html}

## [integer literal](#int-literal "wikilink") {#integer_literal}

Integer literals.

**haskell:**

Haskell does not have negative integer literal syntax. The negative sign
parses as a unary prefix operator. It may be necessary to put parens
around a negative integer constant:

    -- syntax error:
    1 + -3

    -- ok:
    1 + (-3)

`<span id="float-type">`{=html}`</span>`{=html}

## [float type](#float-type "wikilink") {#float_type}

Floating point types.

`<span id="integer-operators">`{=html}`</span>`{=html}

## [integer operators](#int-op "wikilink") {#integer_operators}

The integer operators.

`<span id="float-operators">`{=html}`</span>`{=html}

## [float operators](#float-op "wikilink") {#float_operators}

The floating point operators. Note that in the OCaml the floating point
operators are different from the integer operators.

`<span id="add-integer-and-float">`{=html}`</span>`{=html}

## [add integer and float](#add-int-float "wikilink") {#add_integer_and_float}

How to add an integer and a float.

**ocaml:**

OCaml also can convert a integer to float with *float_of_int*.

`<span id="integer-division">`{=html}`</span>`{=html}

## [integer division](#int-div "wikilink") {#integer_division}

How to find the quotient of two integers; how to find the remainder of
two integers.

`<span id="integer-division-by-zero">`{=html}`</span>`{=html}

## [integer division by zero](#int-div-zero "wikilink") {#integer_division_by_zero}

The result of dividing an integer by zero.

`<span id="float-division">`{=html}`</span>`{=html}

## [float division](#float-div "wikilink") {#float_division}

`<span id="float-division-by-zero">`{=html}`</span>`{=html}

## [float division by zero](#float-div-zero "wikilink") {#float_division_by_zero}

The result of division by zero.

`<span id="float-exponentiation">`{=html}`</span>`{=html}

## [float exponentiation](#float-exponentiation "wikilink") {#float_exponentiation}

How to exponentiate a float.

`<span id="float-functions">`{=html}`</span>`{=html}

## [float functions](#float-func "wikilink") {#float_functions}

The square root function; the natural exponential and natural logarithm
functions; the trigonometric functions.

`<span id="arithmetic-truncation">`{=html}`</span>`{=html}

## [arithmetic truncation](#arith-truncation "wikilink") {#arithmetic_truncation}

Ways to convert a float to a nearby integer.

**ocaml:**

This definition of *round* handles negative numbers correctly:

    let round x = int_of_float (floor (x +. 0.5))

## [power](#power "wikilink")

How to perform exponentiation.

**ocaml:**

How to define a function which computes the power of an integer:

    let integer_exponent b e =
      let rec aux x i =
        if i = e then x else aux (x * b) (i + 1)
      in
      aux 1 0;;

`<span id="sqrt--1">`{=html}`</span>`{=html}

## [sqrt -1](#sqrt-negative-one "wikilink") {#sqrt__1}

The result of taking the square root of a negative number.

`<span id="transcendental-functions">`{=html}`</span>`{=html}

## [transcendental functions](#transcendental-func "wikilink") {#transcendental_functions}

`<span id="transcendental-constants">`{=html}`</span>`{=html}

## [transcendental constants](#transcendental-func "wikilink") {#transcendental_constants}

`<span id="integer-overflow">`{=html}`</span>`{=html}

## [integer overflow](#int-overflow "wikilink") {#integer_overflow}

What happens when expression evaluates to an integer that is larger than
what can be stored.

`<span id="float-overflow">`{=html}`</span>`{=html}

## [float overflow](#float-overflow "wikilink") {#float_overflow}

The result of float overflow.

Ocaml has literals for infinity and negative infinity, but Scala and
Haskell do not.

`<span id="rational-type">`{=html}`</span>`{=html}

## [rational type](#rational-type "wikilink") {#rational_type}

`<span id="rational-construction">`{=html}`</span>`{=html}

## [rational construction](#rational-construction "wikilink") {#rational_construction}

`<span id="rational-decomposition">`{=html}`</span>`{=html}

## [rational decomposition](#rational-decomposition "wikilink") {#rational_decomposition}

`<span id="complex-type">`{=html}`</span>`{=html}

## [complex type](#complex-type "wikilink") {#complex_type}

`<span id="complex-construction">`{=html}`</span>`{=html}

## [complex construction](#complex-construction "wikilink") {#complex_construction}

`<span id="complex-decomposition">`{=html}`</span>`{=html}

## [complex decomposition](#complex-decomposition "wikilink") {#complex_decomposition}

`<span id="random-number">`{=html}`</span>`{=html}

## [random number](#random-num "wikilink") {#random_number}

How to generate a uniformly distributed random integer; how to generate
a uniformly distributed float; how to generate a normally distributed
float.

`<span id="random-seed">`{=html}`</span>`{=html}

## [random seed](#random-seed "wikilink") {#random_seed}

How to set a random seed. How to get and restore the state of a random
number generator.

`<span id="bit-operators">`{=html}`</span>`{=html}

## [bit operators](#bit-op "wikilink") {#bit_operators}

The bit operators.

**ocaml:**

Also has operators which perform arithmetic shift: *asl* and *asr*. When
performing an arithmetic shift, the sign of the integer is preserved.

**haskell:**

Haskell does not assign a default size or type to numeric literals.
Hence numeric literals must have their type declared for bit operations
to be performed on them.

`<span id="binary-octal-and-hex-literals">`{=html}`</span>`{=html}

## [binary, octal, and hex literals](#binary-octal-hex-literals "wikilink") {#binary_octal_and_hex_literals}

## [radix](#radix "wikilink")

`<span id="strings">`{=html}`</span>`{=html}

# [Strings](#strings "wikilink")

`<span id="string-type">`{=html}`</span>`{=html}

## [string type](#str-type "wikilink") {#string_type}

The types for strings and characters.

`<span id="string-literal">`{=html}`</span>`{=html}

## [string literal](#str-literal "wikilink") {#string_literal}

The syntax for a string literal.

`<span id="newline-in-literal">`{=html}`</span>`{=html}

## [newline in literal](#newline-in-str-literal "wikilink") {#newline_in_literal}

`<span id="literal-escapes">`{=html}`</span>`{=html}

## [literal escapes](#str-esc "wikilink") {#literal_escapes}

`<span id="format-string">`{=html}`</span>`{=html}

## [format string](#format-str "wikilink") {#format_string}

## [concatenate](#str-concat "wikilink")

How to concatenate strings.

**f#:**

F# supports (with a warning) the \^ operator for compatibility with
OCaml.

## [replicate](#str-replicate "wikilink")

`<span id="translate-case">`{=html}`</span>`{=html}

## [translate case](#translate-case "wikilink") {#translate_case}

How to convert a string to uppercase; how to convert a string to
lowercase; how to capitalize the first character.

## [capitalize](#capitalize "wikilink")

## [trim](#trim "wikilink")

## [pad](#pad "wikilink")

`<span id="number-to-string">`{=html}`</span>`{=html}

## [number to string](#num-to-str "wikilink") {#number_to_string}

`<span id="string-to-number">`{=html}`</span>`{=html}

## [string to number](#str-to-num "wikilink") {#string_to_number}

How to parse numeric types from string; how to convert numeric types to
strings.

**ocaml:**

To convert a string to a float:

    float_of_string &quot;3.14&quot;

## [join](#join "wikilink")

## [split](#split "wikilink")

`<span id="character-type">`{=html}`</span>`{=html}

## [character type](#char-type "wikilink") {#character_type}

`<span id="character-literal">`{=html}`</span>`{=html}

## [character literal](#char-literal "wikilink") {#character_literal}

## [length](#str-len "wikilink")

How to get the length of a string.

`<span id="index-of-substring">`{=html}`</span>`{=html}

## [index of substring](#index-substr "wikilink") {#index_of_substring}

How to get the index of a substring.

`<span id="extract-substring">`{=html}`</span>`{=html}

## [extract substring](#substr "wikilink") {#extract_substring}

How to extract a substring.

`<span id="extract-character">`{=html}`</span>`{=html}

## [extract character](#extract-char "wikilink") {#extract_character}

How to get the character at a specified index of a string.

The syntax for a character literal.

`<span id="chr-and-ord">`{=html}`</span>`{=html}

## [chr and ord](#chr-ord "wikilink") {#chr_and_ord}

How to convert a character to its ASCII code or Unicode point; how to
convert an ASCII code or Unicode point to a character.

`<span id="dates-and-time">`{=html}`</span>`{=html}

# [Dates and Time](#dates-time "wikilink") {#dates_and_time}

`<span id="arrays">`{=html}`</span>`{=html}

# [Arrays](#arrays "wikilink")

`<span id="lists">`{=html}`</span>`{=html}

# [Lists](#lists "wikilink")

`<span id="list-literal">`{=html}`</span>`{=html}

## list literal {#list_literal}

`<span id="list-element-element">`{=html}`</span>`{=html}

## list element element {#list_element_element}

`<span id="list-head">`{=html}`</span>`{=html}

## list head {#list_head}

**f#:**

Supports *List.hd* (with a warning) to be compatible with OCaml.

## list-tail {#list_tail}

Supports *List.tl* (with a warning) to be compatible with OCaml.

`<span id="tuples">`{=html}`</span>`{=html}

# [Tuples](#tuples "wikilink")

## tuple

`<span id="tuple-element">`{=html}`</span>`{=html}

## tuple element {#tuple_element}

`<span id="functions">`{=html}`</span>`{=html}

# [Functions](#functions "wikilink")

## function

How to define a function.

## lambda

How to define an anonymous function.

`<span id="piecewise-defined-function">`{=html}`</span>`{=html}

## piecewise defined function {#piecewise_defined_function}

How to define a function with multiple equations and matching on the
arguments.

`<span id="recursive-function">`{=html}`</span>`{=html}

## recursive function {#recursive_function}

How to define a recursive function.

`<span id="mutually-recursive-functions">`{=html}`</span>`{=html}

## mutually recursive functions {#mutually_recursive_functions}

How to define two functions which call each other. Mutual recursion can
be eliminated by inlining the second function inside the first function.
The first function is then recursive and can be defined independently of
the second function.

`<span id="named-parameter">`{=html}`</span>`{=html}

## named parameter {#named_parameter}

How to define and invoke a function with named parameters.

**ocaml:**

Multiple parameters can share a name. In the function definition colons
are used to rename the parameters for use in the function body.

    let add_xs ~x:x1 ~x:x2 = x1 + x2;;
    add_xs ~x:3 ~x:7;;

`<span id="named-parameter-default-value">`{=html}`</span>`{=html}

## named parameter default value {#named_parameter_default_value}

How to make named parameters optional by providing a default value in
the definition.

**ocaml:**

For a named parameter to be optional, it must be following by an unnamed
parameter in the definition. This permits the parser to unambiguously
determine if the optional parameter has been provided or not. If the
optional parameter is not followed by an unnamed parameter in the
definition, then named parameter is not optional. If the function is
invoked without the parameter, it returns a curried version of the
function which expects the missing named parameter as an argument.

`<span id="infix-operator-in-prefix-position">`{=html}`</span>`{=html}

## infix operator in prefix position {#infix_operator_in_prefix_position}

How to invoke an infix operator in prefix position.

`<span id="function-in-infix-position">`{=html}`</span>`{=html}

## function in infix position {#function_in_infix_position}

How to invoke a function in infix position.

## currying

How to create a curried function by providing values for some of the
arguments of a function.

`<span id="function-composition-operator">`{=html}`</span>`{=html}

## function composition operator {#function_composition_operator}

An operator which takes two functions as arguments and returns a
function constructed from them by composition.

`<span id="lazy-evaluation">`{=html}`</span>`{=html}

## lazy evaluation {#lazy_evaluation}

How to evaluate the arguments to a function in a lazy manner.

Lazy evaluation is also called *call-by-name*.

**ocaml:**

OCaml provides the `lazy` function. It is up to the caller to specify
that the argument is to evaluated lazily.

**haskell:**

Haskell evaluates arguments lazily by default.

`<span id="strict-evaluation">`{=html}`</span>`{=html}

## strict evaluation {#strict_evaluation}

How to evaluate arguments before they are passed to a function.

Strict evaluation is also called *call by-value*.

**haskell:**

The `seq` function evaluates its first argument and then returns the
second argument.

`<span id="execution-control">`{=html}`</span>`{=html}

# [Execution Control](#execution-control "wikilink") {#execution_control}

## if

`<span id="if-else-if-else">`{=html}`</span>`{=html}

## if else-if else {#if_else_if_else}

## sequencing

## while

**ocaml:**

There is no break or continue statement. In addition to using
references, it is possible to use exceptions to break out of a while
loop.

## for

How to loop over a range of integers.

**sml:**

How to define a `for` loop in SML:

    datatype for = to of int * int
                 | downto of int * int

    infix to downto

    val for =
        fn lo to up =&gt;
           (fn f =&gt; let fun loop lo = if lo &gt; up then ()
                                      else (f lo; loop (lo+1))
                    in loop lo end)
         | up downto lo =&gt;
           (fn f =&gt; let fun loop up = if up &lt; lo then ()
                                      else (f up; loop (up-1))
                    in loop up end)

How to use the for loop:

    for (1 to 9)
        (fn i =&gt; print (Int.toString i))

    for (9 downto 1)
        (fn i =&gt; print (Int.toString i))

`<span id="for-in-reverse">`{=html}`</span>`{=html}

## for in reverse {#for_in_reverse}

How to iterate over a reversed range of integers.

`<span id="list-iteration">`{=html}`</span>`{=html}

## list iteration {#list_iteration}

How to iterate over the members of a list.

## loop

An infinite loop.

`<span id="exceptions">`{=html}`</span>`{=html}

# [Exceptions](#exceptions "wikilink")

`<span id="raise-error">`{=html}`</span>`{=html}

## raise error {#raise_error}

How to raise an error.

`<span id="handle-error">`{=html}`</span>`{=html}

## handle error {#handle_error}

How to handle an error.

`<span id="concurrency">`{=html}`</span>`{=html}

# [Concurrency](#concurrency "wikilink")

`<span id="filehandles">`{=html}`</span>`{=html}

# [Filehandles](#file-handles "wikilink")

`<span id="files">`{=html}`</span>`{=html}

# [Files](#files "wikilink")

`<span id="directories">`{=html}`</span>`{=html}

# [Directories](#directories "wikilink")

`<span id="processes-and-environment">`{=html}`</span>`{=html}

# [Processes and Environment](#processes-environment "wikilink") {#processes_and_environment}

`<span id="libraries-and-namespaces">`{=html}`</span>`{=html}

# [Libraries and Namespaces](#libraries-namespaces "wikilink") {#libraries_and_namespaces}

`<span id="namespace-example">`{=html}`</span>`{=html}

## namespace example {#namespace_example}

## namespaces

`<span id="file-name-restrictions">`{=html}`</span>`{=html}

## file name restrictions {#file_name_restrictions}

## import

`<span id="namespace-creation">`{=html}`</span>`{=html}

## namespace creation {#namespace_creation}

`<span id="namespace-alias">`{=html}`</span>`{=html}

## namespace alias {#namespace_alias}

`<span id="namespace-separator">`{=html}`</span>`{=html}

## namespace separator {#namespace_separator}

## subnamespace

`<span id="inspect-namespace">`{=html}`</span>`{=html}

## inspect namespace {#inspect_namespace}

`<span id="user-defined-types">`{=html}`</span>`{=html}

# [User-Defined Types](#user-defined-types "wikilink") {#user_defined_types}

keywords used to define types by language

pascal · c · c++ · ocaml · scala · haskell

type synonym · type · typedef · typedef · type · type · type

sum type · type · enum *or* uniontype · abstract class · data

tuple product typetypedata

record product type · record · struct · struct *or* class · type *...*
of · class · data

Examples of algebraic sum types are the enumerated type of Pascal and
the enum of C. The definition of the type lists a set of values which
variables which have the type can contain. The values are called
variants.

The enumerated type of Pascal and the enum of C are implemented as
integers, and one can recover the underlying integer value associated
with each variant. In Pascal one uses the `ord` function to do this. One
can use the equality test operator to determine whether two variables
hold the same variant. One can also use the less than (\<) operator to
determine if a variable holds a variant which occurs earlier in the type
definition list than another.

An enumerated type is thus similar to defining a sequence of integer
constants like this:

      typedef int month;

      const month JANUARY = 1;
      const month FEBRUARY = 2;
      .
      .
      .
      const month DECEMBER = 12;

An enumerated type gives the compiler the ability to ensure that only
variants listed in the type definition list are actually stored in
variables with the enumerated type however.

BETTER EXPLANATION AND MOTIVATION OF UNARY TYPES. OTHER THAN VARIANTS:
UNIT. ARE

UNARY TYPES USEFUL?

Algebraic sum types are more general than enumerated types, because the
variants are not restricted to being unary types. By a unary type we
mean a type whose variables can only contain a single value. EXAMPLE OF
SUCH AND ALGEBRAIC SUM TYPE. Because of this generality, one cannot
assume that a general algebraic sum type variant has an integer
representation. Some languages nevertheless define an order on the
variants.

SUM TYPE: NUMBER OF VALUES IS THE SUM OF THE VALUES OF EACH OF THE
VARIANTS

C UNION TYPE AS ALGEBRAIC SUM TYPE

Examples of algebraic product types are the record of Pascal and the
struct of C. An algebraic product type wraps several values into a
single "super" value. The components of an algebraic product type are
called fields, and each has a type which constrains the values which can
be stored in it. The type of each field is normally a pre-existing type,
but see the note on recursive types below.

To extract a field from a product value, each field must be identified.
In the case of the Pascal and the C struct the fields are given names.
Product types can also be defined in which the fields are identified by
position like a tuple. OCaml and Haskell support both types of product
type.

Since OCaml and Haskell have both tuples and tuple product types, it is
worthwhile to consider the differences. One could represent represent
coordinates on plane with a simple pair tuple with this type:

      (float, float)

However, all 2-tuples in which the components are both floats are the
same type. With tuple product types, we could define two distinct types:

      type cartesian = Cartestion of float * float;
      type polar = Polar of float * float;

The compiler will now prevent us from using cartesian coordinates in a
place where polar coordinates are expected.

It is also instructive to consider the difference between a type synonym
and a product type with a single field. In the former case the two types
are interchangeable. Type synonyms are useful as a shorthand for a long
type, such as a 10-tuple or a function type. Functions which operate on
variables of the original type will also operate on variables with the
type synonym. In fact, it should be noted that type synonyms don't
create a constructor, so the constructor for the original type must be
used.

A product type with a single field creates a new type and provides a
constructor for it which accepts the original type as an argument.
Functions which take the original type as an argument cannot be used on
the new type.

COMBINED ALGEBRAIC TYPES.

Algebraic product types first appeared in 1966 in Algol W. Algol W
extended Algol 60 by adding a record type. The idea was due to Niklaus
Wirth and C. A. R. Hoare. Pascal, which appeared in 1970, had both a
record type and an enumerated type as already noted, and the Pascal
enumerated type seems to be the first example of a type that could be
called an algebraic sum type.

Algebraic types first appeared in their full generality in the
programming language called Hope, circa 1980. Algebraic types were soon
borrowed into ML. Hope introduced the terms algebraic data type, product
type, and sum type. It also introduced pattern matching.

PATTERN MATCHING.

`<span id="type-synonym">`{=html}`</span>`{=html}

## [type synonym](#type-synonym "wikilink") {#type_synonym}

`<span id="sum-type">`{=html}`</span>`{=html}

## [sum type](#sum-type "wikilink") {#sum_type}

`<span id="generic-type">`{=html}`</span>`{=html}

## [generic type](#generic-type "wikilink") {#generic_type}

`<span id="recursive-type">`{=html}`</span>`{=html}

## [recursive type](#recursive-type "wikilink") {#recursive_type}

`<span id="objects">`{=html}`</span>`{=html}

# [Objects](#objects "wikilink")

`<span id="inheritance-and-polymorphism">`{=html}`</span>`{=html}

# [Inheritance and Polymorphism](#inheritance-polymorphism "wikilink") {#inheritance_and_polymorphism}

`<span id="repl">`{=html}`</span>`{=html}

# [REPL](#repl "wikilink")

`<span id="repl-1">`{=html}`</span>`{=html}

## repl {#repl_1}

`<span id="repl-limitations">`{=html}`</span>`{=html}

## repl limitations {#repl_limitations}

`<span id="repl-last-value">`{=html}`</span>`{=html}

## repl last value {#repl_last_value}

## help

**ocaml**

The OCaml top level provides [these
directives](http://caml.inria.fr/pub/docs/manual-ocaml/manual023.html#toc90):

    #cd &quot;DIRNAME&quot;;;
    #directory &quot;DIRNAME&quot;;;
    #install_printer PRINTER_NAME;;
    #label BOOL;;
    #load &quot;FILENAME&quot;;;
    #print_depth N;;
    #print_length N;;
    #quit;;
    #remove_printer PRINTER_NAME;;
    #trace FUNCTION_NAME;;
    #untrace FUNCTION_NAME;;
    #untrace_all;;
    #use &quot;FILENAME&quot;;;
    #warnings &quot;WARNINGS_LIST&quot;;;

`<span id="inspect-type">`{=html}`</span>`{=html}

## inspect type {#inspect_type}

`<span id="load-source-file">`{=html}`</span>`{=html}

## load source file {#load_source_file}

`<span id="search-path">`{=html}`</span>`{=html}

## search path {#search_path}

`<span id="set-search-path-on-command-line">`{=html}`</span>`{=html}

## set search path on command line {#set_search_path_on_command_line}

`<span id="sml">`{=html}`</span>`{=html}

# SML

[Programming in Standard ML
'97](http://homepages.inf.ed.ac.uk/stg/NOTES/node2.html)

[The Standard ML Basis Library](http://sml-family.org/Basis/index.html)

`<span id="ocaml">`{=html}`</span>`{=html}

# OCaml

[The Objective-Caml
system](http://caml.inria.fr/pub/docs/manual-ocaml/index.html)

`<span id="f">`{=html}`</span>`{=html}

# F#

[F# Language
Reference](http://msdn.microsoft.com/en-us/library/dd233181.aspx)

[F# Core Library
Reference](http://msdn.microsoft.com/en-us/library/ee353567.aspx)

`<span id="haskell">`{=html}`</span>`{=html}

# Haskell

[Haskell 2010 Language
Report](http://www.haskell.org/onlinereport/haskell2010/)

[Haskell Hierarchical
Libraries](http://www.haskell.org/ghc/docs/latest/html/libraries/index.html)

[issue tracker](https://github.com/clarkgrubb/hyperpolyglot/issues) \|
content of this page licensed under [creative commons
attribution-sharealike
3.0](http://creativecommons.org/licenses/by-sa/3.0/)
