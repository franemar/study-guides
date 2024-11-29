# [Hyperpolyglot](/)

## ML Dialects and Haskell: SML, OCaml, F#, Haskell


_a side-by-side reference sheet_

[grammar and invocation](#grammar-invocation) \| [variables and expressions](#var-expr) \| [arithmetic and logic](#arithmetic-logic) \| [strings](#strings) \| [dates and time](#dates-time) \| [arrays](#arrays) \| [lists](#lists) \| [tuples](#tuples) \| [dictionaries](#dictionaries) \| [functions](#functions) \| [execution control](#execution-control) \| [exceptions](#exceptions) \| [concurrency](#concurrency) \| [file handles](#file-handles) \| [files](#files) \| [directories](#directories) \| [processes and environment](#processes-environment) \| [libraries and namespaces](#libraries-namespaces) \| [user-defined types](#user-defined-types) \| [objects](#objects) \| [inheritance and polymorphism](#inheritance-polymorphism) \| [net and web](#net-web) \| [unit tests](#unit-tests) \| [debugging and profiling](#debugging-profiling) \| [repl](#repl)


<iframe src="ML-Dialects&Haskell_SML&OCaml&F#&Haskell-table.html" title="View table" loading="lazy"></iframe>


## [version used](\#version-used)

Versions used to test the code samples in this sheet.

## [show version](\#version)

How to get the version.

## [Grammar and Invocation](\#grammar-invocation)

## [interpreter](\#interpreter)

How to run the interpreter on a file of source code.

## [shebang](\#shebang)

How to use the interpreter in a shebang.

## [bytecode compiler and interpreter](\#bytecode-compiler-interpreter)

How to compile source to bytecode and run it.

**ocaml:**

It is not necessary to invoke _ocamlrun_ on the bytecode; the bytecode can be executed directly because the bytecode compiler puts a shebang invocation at the top of the file.

## [native compiler](\#native-compiler)

How to compile source to native code and run it.

## [library which is always imported](\#library-always-imported)

The name of the library containing the types and functions which are always available.

## [statement terminator](\#statement-terminator)

**ocaml:**

;; is the ocaml statement separator. It is not necessary at the end of the line if the following line starts with an _open_ or _let_ keyword or at the end of the file.

## [blocks](\#blocks)

How to define a block of statements.

## [end-of-line comment](\#end-of-line-comment)

A comment terminated by the end of the line.

## [multiple line comment](\#multiple-line-comment)

A comment with a start and end delimiter which can span multiple lines.

**ocaml:**

(\\* \*) style comments can be nested.

# [Variables and Expressions](\#var-expr)

## [write-once variable](\#value)

How to define a variable which can be set at run-time but cannot be modified after it is set.

## [modifiable variable](\#variable)

How to define a modifiable variable.

## [unit type and value](\#unit)

The notation for the unit type and the unit value. In all languages the notation for the unit value is the same as the notation for an empty tuple.

The unit value is a common return value of functions which perform side effects.

## [conditional expression](\#conditional-expression)

The syntax for a conditional expression.

## [branch type mismatch](\#branch-type-mismatch)

What happens if the two branches of a conditional expression don't have the same type.

## [null](\#null)

A value used somewhat paradoxically to indicate the absence of a value.

Types which can contain a null value are called _option types_.

## [nullable type](\#nullable-type)

## [null test](\#null-test)

## [coalesce](\#coalesce)

## [expression type declaration](\#expr-type-decl)

How to explicitly declare the type of an expression.

## [let ... in ...](\#let-in)

How to define local variables.

**ocaml:**

OCaml uses _let_ to define a value and _let_ with _in_ to define values in a local scope. OCaml follows the usage of the original dialect of ML in this respect.

OCaml can define multiple values with a single _let_ and _in_ by conjoining the definitions with _and_. The definitions are performed in parallel, so later definitions cannot use the earlier definitions:

```
let z =
let x = 3
and y = 4 in
x * y;;

```

**haskell:**

Haskell uses _let_ with _in_ to define local scope. In addition, _ghci_ uses _let_ without _in_ to define values.

## [where](\#where)

How to define local variables with definitions after the expression that uses them.

# [Arithmetic and Logic](\#arithmetic-logic)

## [boolean type](\#boolean-type)

The type for boolean values.

## [true and false](\#true-false)

The literals for true and false.

## [logical operators](\#logical-op)

The logical operators: _and_, _or_, and _not_.

## [relational operators](\#relational-op)

Operators for performing comparisons.

## [min and max](\#min-max)

The binary functions _min_ and _max_.

## [integer types](\#int-type)

The most commonly used numeric types.

## [integer literal](\#int-literal)

Integer literals.

**haskell:**

Haskell does not have negative integer literal syntax. The negative sign parses as a unary prefix operator. It may be necessary to put parens around a negative integer constant:

```
-- syntax error:
1 + -3

-- ok:
1 + (-3)

```

## [float type](\#float-type)

Floating point types.

## [integer operators](\#int-op)

The integer operators.

## [float operators](\#float-op)

The floating point operators. Note that in the OCaml the floating point operators are different from the integer operators.

## [add integer and float](\#add-int-float)

How to add an integer and a float.

**ocaml:**

OCaml also can convert a integer to float with _float\_of\_int_.

## [integer division](\#int-div)

How to find the quotient of two integers; how to find the remainder of two integers.

## [integer division by zero](\#int-div-zero)

The result of dividing an integer by zero.

## [float division](\#float-div)

## [float division by zero](\#float-div-zero)

The result of division by zero.

## [float exponentiation](\#float-exponentiation)

How to exponentiate a float.

## [float functions](\#float-func)

The square root function; the natural exponential and natural logarithm functions; the trigonometric functions.

## [arithmetic truncation](\#arith-truncation)

Ways to convert a float to a nearby integer.

**ocaml:**

This definition of _round_ handles negative numbers correctly:

```
let round x = int_of_float (floor (x +. 0.5))

```

## [power](\#power)

How to perform exponentiation.

**ocaml:**

How to define a function which computes the power of an integer:

```
let integer_exponent b e =
  let rec aux x i =
    if i = e then x else aux (x * b) (i + 1)
  in
  aux 1 0;;

```

## [sqrt -1](\#sqrt-negative-one)

The result of taking the square root of a negative number.

## [transcendental functions](\#transcendental-func)

## [transcendental constants](\#transcendental-func)

## [integer overflow](\#int-overflow)

What happens when expression evaluates to an integer that is larger than what can be stored.

## [float overflow](\#float-overflow)

The result of float overflow.

Ocaml has literals for infinity and negative infinity, but Scala and Haskell do not.

## [rational type](\#rational-type)

## [rational construction](\#rational-construction)

## [rational decomposition](\#rational-decomposition)

## [complex type](\#complex-type)

## [complex construction](\#complex-construction)

## [complex decomposition](\#complex-decomposition)

## [random number](\#random-num)

How to generate a uniformly distributed random integer; how to generate a uniformly distributed float; how to generate a normally distributed float.

## [random seed](\#random-seed)

How to set a random seed. How to get and restore the state of a random number generator.

## [bit operators](\#bit-op)

The bit operators.

**ocaml:**

Also has operators which perform arithmetic shift: _asl_ and _asr_. When performing an arithmetic shift, the sign of the integer is preserved.

**haskell:**

Haskell does not assign a default size or type to numeric literals. Hence numeric literals must have their type declared for bit operations to be performed on them.

## [binary, octal, and hex literals](\#binary-octal-hex-literals)

## [radix](\#radix)

# [Strings](\#strings)

## [string type](\#str-type)

The types for strings and characters.

## [string literal](\#str-literal)

The syntax for a string literal.

## [newline in literal](\#newline-in-str-literal)

## [literal escapes](\#str-esc)

## [format string](\#format-str)

## [concatenate](\#str-concat)

How to concatenate strings.

**f#:**

F# supports (with a warning) the ^ operator for compatibility with OCaml.

## [replicate](\#str-replicate)

## [translate case](\#translate-case)

How to convert a string to uppercase; how to convert a string to lowercase; how to capitalize the first character.

## [capitalize](\#capitalize)

## [trim](\#trim)

## [pad](\#pad)

## [number to string](\#num-to-str)

## [string to number](\#str-to-num)

How to parse numeric types from string; how to convert numeric types to strings.

**ocaml:**

To convert a string to a float:

```
float_of_string "3.14"

```

## [join](\#join)

## [split](\#split)

## [character type](\#char-type)

## [character literal](\#char-literal)

## [length](\#str-len)

How to get the length of a string.

## [index of substring](\#index-substr)

How to get the index of a substring.

## [extract substring](\#substr)

How to extract a substring.

## [extract character](\#extract-char)

How to get the character at a specified index of a string.

The syntax for a character literal.

## [chr and ord](\#chr-ord)

How to convert a character to its ASCII code or Unicode point; how to convert an ASCII code or Unicode point to a character.

# [Dates and Time](\#dates-time)

# [Arrays](\#arrays)

# [Lists](\#lists)

## list literal

## list element element

## list head

**f#:**

Supports _List.hd_ (with a warning) to be compatible with OCaml.

## list-tail

Supports _List.tl_ (with a warning) to be compatible with OCaml.

# [Tuples](\#tuples)

## tuple

## tuple element

# [Functions](\#functions)

## function

How to define a function.

## lambda

How to define an anonymous function.

## piecewise defined function

How to define a function with multiple equations and matching on the arguments.

## recursive function

How to define a recursive function.

## mutually recursive functions

How to define two functions which call each other. Mutual recursion can be eliminated by inlining the second function inside the first function. The first function is then recursive and can be defined independently of the second function.

## named parameter

How to define and invoke a function with named parameters.

**ocaml:**

Multiple parameters can share a name. In the function definition colons are used to rename the parameters for use in the function body.

```
let add_xs ~x:x1 ~x:x2 = x1 + x2;;
add_xs ~x:3 ~x:7;;

```

## named parameter default value

How to make named parameters optional by providing a default value in the definition.

**ocaml:**

For a named parameter to be optional, it must be following by an unnamed parameter in the definition. This permits the parser to unambiguously determine if the optional parameter has been provided or not. If the optional parameter is not followed by an unnamed parameter in the definition, then named parameter is not optional. If the function is invoked without the parameter, it returns a curried version of the function which expects the missing named parameter as an argument.

## infix operator in prefix position

How to invoke an infix operator in prefix position.

## function in infix position

How to invoke a function in infix position.

## currying

How to create a curried function by providing values for some of the arguments of a function.

## function composition operator

An operator which takes two functions as arguments and returns a function constructed from them by composition.

## lazy evaluation

How to evaluate the arguments to a function in a lazy manner.

Lazy evaluation is also called _call-by-name_.

**ocaml:**

OCaml provides the `lazy` function. It is up to the caller to specify that the argument is to evaluated lazily.

**haskell:**

Haskell evaluates arguments lazily by default.

## strict evaluation

How to evaluate arguments before they are passed to a function.

Strict evaluation is also called _call by-value_.

**haskell:**

The `seq` function evaluates its first argument and then returns the second argument.

# [Execution Control](\#execution-control)

## if

## if else-if else

## sequencing

## while

**ocaml:**

There is no break or continue statement. In addition to using references, it is possible to use exceptions to break out of a while loop.

## for

How to loop over a range of integers.

**sml:**

How to define a `for` loop in SML:

```
datatype for = to of int * int
             | downto of int * int

infix to downto

val for =
    fn lo to up =>
       (fn f => let fun loop lo = if lo > up then ()
                                  else (f lo; loop (lo+1))
                in loop lo end)
     | up downto lo =>
       (fn f => let fun loop up = if up < lo then ()
                                  else (f up; loop (up-1))
                in loop up end)

```

How to use the for loop:

```
for (1 to 9)
    (fn i => print (Int.toString i))

for (9 downto 1)
    (fn i => print (Int.toString i))

```

## for in reverse

How to iterate over a reversed range of integers.

## list iteration

How to iterate over the members of a list.

## loop

An infinite loop.

# [Exceptions](\#exceptions)

## raise error

How to raise an error.

## handle error

How to handle an error.

# [Concurrency](\#concurrency)

# [Filehandles](\#file-handles)

# [Files](\#files)

# [Directories](\#directories)

# [Processes and Environment](\#processes-environment)

# [Libraries and Namespaces](\#libraries-namespaces)

## namespace example

## namespaces

## file name restrictions

## import

## namespace creation

## namespace alias

## namespace separator

## subnamespace

## inspect namespace

# [User-Defined Types](\#user-defined-types)

keywords used to define types by language

pascal · c · c++ · ocaml · scala · haskell

type synonym · type · typedef · typedef · type · type · type

sum type · type · enum _or_ uniontype · abstract class · data

tuple product typetypedata

record product type · record · struct · struct _or_ class · type _…_ of · class · data

Examples of algebraic sum types are the enumerated type of Pascal and the enum of C. The definition of the type lists a set of values which variables which have the type can contain. The values are called variants.

The enumerated type of Pascal and the enum of C are implemented as integers, and one can recover the underlying integer value associated with each variant. In Pascal one uses the `ord` function to do this. One can use the equality test operator to determine whether two variables hold the same variant. One can also use the less than (<) operator to determine if a variable holds a variant which occurs earlier in the type definition list than another.

An enumerated type is thus similar to defining a sequence of integer constants like this:

```
  typedef int month;

  const month JANUARY = 1;
  const month FEBRUARY = 2;
  .
  .
  .
  const month DECEMBER = 12;

```

An enumerated type gives the compiler the ability to ensure that only variants listed in the type definition list are actually stored in variables with the enumerated type however.

BETTER EXPLANATION AND MOTIVATION OF UNARY TYPES. OTHER THAN VARIANTS: UNIT. ARE

UNARY TYPES USEFUL?

Algebraic sum types are more general than enumerated types, because the variants are not restricted to being unary types. By a unary type we mean a type whose variables can only contain a single value. EXAMPLE OF SUCH AND ALGEBRAIC SUM TYPE. Because of this generality, one cannot assume that a general algebraic sum type variant has an integer representation. Some languages nevertheless define an order on the variants.

SUM TYPE: NUMBER OF VALUES IS THE SUM OF THE VALUES OF EACH OF THE VARIANTS

C UNION TYPE AS ALGEBRAIC SUM TYPE

Examples of algebraic product types are the record of Pascal and the struct of C. An algebraic product type wraps several values into a single "super" value. The components of an algebraic product type are called fields, and each has a type which constrains the values which can be stored in it. The type of each field is normally a pre-existing type, but see the note on recursive types below.

To extract a field from a product value, each field must be identified. In the case of the Pascal and the C struct the fields are given names. Product types can also be defined in which the fields are identified by position like a tuple. OCaml and Haskell support both types of product type.

Since OCaml and Haskell have both tuples and tuple product types, it is worthwhile to consider the differences. One could represent represent coordinates on plane with a simple pair tuple with this type:

```
  (float, float)

```

However, all 2-tuples in which the components are both floats are the same type. With tuple product types, we could define two distinct types:

```
  type cartesian = Cartestion of float * float;
  type polar = Polar of float * float;

```

The compiler will now prevent us from using cartesian coordinates in a place where polar coordinates are expected.

It is also instructive to consider the difference between a type synonym and a product type with a single field. In the former case the two types are interchangeable. Type synonyms are useful as a shorthand for a long type, such as a 10-tuple or a function type. Functions which operate on variables of the original type will also operate on variables with the type synonym. In fact, it should be noted that type synonyms don't create a constructor, so the constructor for the original type must be used.

A product type with a single field creates a new type and provides a constructor for it which accepts the original type as an argument. Functions which take the original type as an argument cannot be used on the new type.

COMBINED ALGEBRAIC TYPES.

Algebraic product types first appeared in 1966 in Algol W. Algol W extended Algol 60 by adding a record type. The idea was due to Niklaus Wirth and C. A. R. Hoare. Pascal, which appeared in 1970, had both a record type and an enumerated type as already noted, and the Pascal enumerated type seems to be the first example of a type that could be called an algebraic sum type.

Algebraic types first appeared in their full generality in the programming language called Hope, circa 1980. Algebraic types were soon borrowed into ML. Hope introduced the terms algebraic data type, product type, and sum type. It also introduced pattern matching.

PATTERN MATCHING.

## [type synonym](\#type-synonym)

## [sum type](\#sum-type)

## [generic type](\#generic-type)

## [recursive type](\#recursive-type)

# [Objects](\#objects)

# [Inheritance and Polymorphism](\#inheritance-polymorphism)

# [REPL](\#repl)

## repl

## repl limitations

## repl last value

## help

**ocaml**

The OCaml top level provides [these directives](http://caml.inria.fr/pub/docs/manual-ocaml/manual023.html#toc90):

```
#cd "DIRNAME";;
#directory "DIRNAME";;
#install_printer PRINTER_NAME;;
#label BOOL;;
#load "FILENAME";;
#print_depth N;;
#print_length N;;
#quit;;
#remove_printer PRINTER_NAME;;
#trace FUNCTION_NAME;;
#untrace FUNCTION_NAME;;
#untrace_all;;
#use "FILENAME";;
#warnings "WARNINGS_LIST";;

```

## inspect type

## load source file

## search path

## set search path on command line

# SML

[Programming in Standard ML '97](http://homepages.inf.ed.ac.uk/stg/NOTES/node2.html)

[The Standard ML Basis Library](http://sml-family.org/Basis/index.html)

# OCaml

[The Objective-Caml system](http://caml.inria.fr/pub/docs/manual-ocaml/index.html)

# F\#

[F# Language Reference](http://msdn.microsoft.com/en-us/library/dd233181.aspx)

[F# Core Library Reference](http://msdn.microsoft.com/en-us/library/ee353567.aspx)

# Haskell

[Haskell 2010 Language Report](http://www.haskell.org/onlinereport/haskell2010/)

[Haskell Hierarchical Libraries](http://www.haskell.org/ghc/docs/latest/html/libraries/index.html)

[issue tracker](https://github.com/clarkgrubb/hyperpolyglot/issues) \|
content of this page licensed under
[creative commons attribution-sharealike 3.0](http://creativecommons.org/licenses/by-sa/3.0/)

