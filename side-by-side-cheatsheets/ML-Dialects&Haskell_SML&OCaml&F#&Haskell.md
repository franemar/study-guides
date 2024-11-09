# [Hyperpolyglot](/)

## ML Dialects and Haskell: SML, OCaml, F#, Haskell


_a side-by-side reference sheet_

[grammar and invocation](#grammar-invocation) \| [variables and expressions](#var-expr) \| [arithmetic and logic](#arithmetic-logic) \| [strings](#strings) \| [dates and time](#dates-time) \| [arrays](#arrays) \| [lists](#lists) \| [tuples](#tuples) \| [dictionaries](#dictionaries) \| [functions](#functions) \| [execution control](#execution-control) \| [exceptions](#exceptions) \| [concurrency](#concurrency) \| [file handles](#file-handles) \| [files](#files) \| [directories](#directories) \| [processes and environment](#processes-environment) \| [libraries and namespaces](#libraries-namespaces) \| [user-defined types](#user-defined-types) \| [objects](#objects) \| [inheritance and polymorphism](#inheritance-polymorphism) \| [net and web](#net-web) \| [unit tests](#unit-tests) \| [debugging and profiling](#debugging-profiling) \| [repl](#repl)


|            | [sml](#sml) | [ocaml](#ocaml) | [f#](#fsharp) | [haskell](#haskell) |
| :-------------: | :-------------: | :-----: | :---: | :---: |
| [version used](#version-used-note) | _SML NJ 110_ | _4.0_ | _F# 3.0. Mono 3.2_ | _7.4_  
| [show version](#version-note)      | _displayed at startup_ | $ `ocaml -version` | $ `fsharpi --help` | $ `ghc --version` |

### [grammar and invocation](#grammar-invocation-note)

|            | sml | ocaml | f# | haskell |
| :-------------: | :-------------: | :-----: | :---: | :---: |

[interpreter](#interpreter-note)$ echo 'print\_endline "hello"' > hello.ml

$ ocaml hello.ml · $ cat <<EOF > hello.fs

module hello

let main = printfn "hello"

EOF

$ fsharpi --quiet --exec hello.fs · $ echo 'main = putStrLn "hello"' > hello.hs

$ runghc hello.hs

[shebang](#shebang-note)$ cat <<EOF > hello.ml

#!/usr/bin/env ocaml

print\_endline "hello";;

EOF

$ chmod +x hello.ml

$ ./hello.ml · $ cat <<EOF > hello.fs

#light (\*

exec fsharpi --exec $0 --quiet

\*)

module hello

printfn "hello"

EOF

$ chmod +x hello.fs

$ ./hello.fs · $ cat <<EOF > hello.hs

#!/usr/bin/env runghc

main = putStrLn "hello"

EOF

$ chmod +x hello.hs

$ ./hello.hs

[bytecode compiler and interpreter](#bytecode-compiler-interpreter-note)$ echo 'print\_endline "hello";;' > hello.ml

$ ocamlc -o hello hello.ml

$ ocamlrun hello · $ echo 'printfn "hello"' > hello.fs

$ fsharpc hello.fs

$ mono hello.exe · _none_

[native compiler](#native-compiler-note)$ echo 'print\_endline "hello";;' > hello.ml

$ ocamlopt hello.ml -o hello

$ ./hello · _none_ · $ echo 'main = putStrLn "hello"' > hello.hs

$ ghc -o hello hello.hs

$ ./hello

[library which is always imported](#library-always-imported-note)Pervasives · Core · Prelude

[statement terminator](#statement-terminator-note) · ; · ;; · ;; · _next line has equal or less indentation, or_ ;

[blocks](#blocks-note) · ( _expr_ ; _…_ ) · ( _expr_ ; _…_ )

begin _expr_ ; _…_ end · ( _expr_ ; _…_ )

begin _expr_ ; _…_ end · _offside rule or_ { }

[end-of-line comment](#end-of-line-comment-note) · _none_ · _none_ · //_comment_ · --_comment_

[multiple line comment](#multiple-line-comment-note) · (\\* _comment_

_another comment_ \*) · (\\* _comment_

_another comment_ \*) · (\\* _comment_

_another comment_ \*) · {\- _comment_

_another comment_ -}

[variables and expressions](#var-expr-note)

sml · ocaml · f# · haskell

[write-once variable](#value-note) · val a = 3; · let n = 1 + 2;; · let n = 1 + 2 · n = 3

[modifiable variable](#variable-note) · val a = ref 3;

a := 4;

!a + 7; · let n = ref 3;;

n := 4;;

!n + 7;; · let n = ref 3

n := 4

!n + 7 · n <- return 3

[unit type and value](#unit-note) · unit

() · unit

() · unit

() · ()

()

[conditional expression](#conditional-expression-note) · val x = 3;

if x < 0 then ~x else x; · let n = -3;;

let absn = if n < 0 then -n else n;; · let n = -3

let absn = if n < 0 then -n else n · n = -3

let absn = if n < 0 then -n else n

[branch type mismatch](#branch-type-mismatch-note) · (\\* compilation error: \*)

if true then "hello" else 3; · (\\* compilation error: \*)

if true then "hello" else 3;; · (\\* compilation error: \*)

if true then "hello" else 3 · -- compilation error:

if True then "hello" else 3

[null](#null-note) · NONE · None · None

_Also this value returned by .NET library functions. It has a type distinct from_ None:

null · Nothing

[nullable type](#nullable-type-note) · type list\_option\_int = int option list;

val list = \[SOME 3,NONE, SOME ~4\]; · type list\_option\_int = int option list;;

let list = \[Some 3; None; Some (-4)\];;list = \[Just(3), Nothing, Just(-4)\]

[null test](#null-test-note)match foo with

\| None -> true

\| \_ -> false;;

[coalesce](#coalesce-note) · val foo = SOME 3;

(\\* raises exception if NONE: \*)

valOf foo;

(\\* evaluates to 0 if NONE: \*)

getOpt (foo, 0); · match foo with

\| None -> 0

\| Some n -> n;;import Data.Maybe

let foo = Just(3)

_raises exception if Nothing:_

fromJust foo

let intId x = x

_evaluates to 0 if Nothing:_

maybe 0 intId foo

[nullif](#nullif-note)match foo with

\| -999 -> None

\| n -> Some n;;

[expression type declaration](#expr-type-decl-note)float 1 · float 1 · 1 :: Double

[let ... in ...](#let-in-note) · val z =

let

val x = 3.0

val y = 2.0 \* x

in

x \* y

end; · let z =

let x = 3.0 in

let y = 2.0 \*. x in

x \*. y;; · let z =

let x = 3.0 in

let y = 2.0 \* x in

x \* y · z = let x = 3.0

y = 2.0 \* x

in x \* y

[where](#where-note) · _none_ · _none_ · _none_ · z = x \* y

where x = 3.0

y = 2.0 \* x

[arithmetic and logic](#arithmetic-logic-note)

sml · ocaml · f# · haskell

[boolean type](#boolean-type-note) · bool · bool · bool · Bool

[true and false](#true-false-note) · true false · true false · true false · True False

[logical operators](#logical-op-note) · andalso orelse not · && \|\| not · && \|\| not · && \|\| not

[relational operators](#relational-op-note) · = <> < \> <= >= · = <> < \> <= >= · = <> < \> <= >= · == /= < \> <= >=

[min and max](#min-max-note)min 1 2

max 1 2 · min 1 2

max 1 2 · min 1 2

max 1 2

[integer type](#int-type-note) · int · int

_other integer types:_

int32 int64 nativeint · int

_other integer types:_

int32 int64 nativeint · Integer

[integer literal](#int-literal-note) · negative integer: _~4_ · int, int64, and nativeint literals:

12 12L 12n

literals can contain underscores:

1\_000\_000

this parses as an expression:

-4 · -4 · an expression, not a literal:

-4

[float type](#float-type-note) · real · float · float · Double

[integer operators](#int-op-note) · \+ \- \\* div mod · \+ \- \\* / mod

mod //is an infix operator · \+ \- \\* / % · \+ \- \\* div rem

div _and_ rem _are functions, not infix operators_

[float operators](#float-op-note) · \+ \- \\* / · +. -. \*. /. · + \- \\* / · \+ \- \\* /

[add integer and float](#add-int-float-note) · real 3 + 7.0; · float 3 +. 7.0 · float 3 + 7.0 · 3 + 7.0

[integer division](#int-div-note)

_and remainder_ · 7 div 3

7 mod 3

real 7 / real 3 · 7 / 3

7 mod 3 · 7 / 3

7 % 3 · div 7 3

rem 7 3

[integer division by zero](#int-div-zero-note)_raises_ Division\_by\_zero · System.DivideByZeroException · _Exception: divide by zero_

[float division](#float-div-note)float 7 /. float 3 · float 7 / float 3 · 7 / 3

[float division by zero](#float-div-zero-note)infinity nan _or_ neg\_infinity · infinity nan _or_ neg\_infinity · _evaluates to_ Infinity, NaN, _or_ -Infinity, _values which do not have literals_

[power](#power-note) · Math.pow (2.0, 32.0); · 2.0 \*\* 32.0 · 2.0 \*\* 32.0 · 2 \*\* 32

-- syntax error if exponent not an integer:

2 ^ 32

[sqrt](#sqrt-note) · Math.sqrt 2.0 · sqrt 2.0 · sqrt 2.0 · sqrt 2

[sqrt -1](#sqrt-negative-one-note) · _Math.sqrt ~1.0 evaluates to_ nan · sqrt (-1.0):

nan · nan · sqrt (-1) _evaluates to_ NaN, _a value which has no literal_

[transcendental functions](#transcendental-func-note) · Math.exp Math.ln

Math.sin Math.cos Math.tan

Math.asin Math.acos Math.atan

Math.atan2 · exp log

sin cos tan

asin acos atan

atan2 · exp log

sin cos tan

asin acos atan

atan2 · exp log

sin cos tan

asin acos atan

atan2

[transcendental constants](#transcendental-const-note) · Math.pi

Math.e · 4.0 \*. atan 1.0

exp 1.0 · System.Math.PI

System.Math.E · pi

exp 1

[float truncation](#float-truncation-note) · round 3.14

trunc 3.14

floor 3.14

ceil 3.14 · truncate 3.14

_none_

floor 3.14 _returns float_

ceil 3.14 _returns float_ · truncate 3.14

round 3.14

floor 3.14 _returns float_

ceil 3.14 _returns float_ · truncate 3.14

round 3.14

floor 3.14

ceiling 3.14

[absolute value](#abs-val-note)

_and signum_abs (-7)

abs\_float (-7.0)

_no signum_ · abs -7

abs -7.0

sign -7

sign -7.0 · abs (-7)

signum (-7)

[integer overflow](#int-overflow-note) · _Overflow exception_ · _modular arithmetic_ · _modular arithmetic_ · _has arbitrary length integers_

[float overflow](#float-overflow-note)infinity · infinity · _evaluates to_ Infinity, _a value which has no literal_

[arbitrary length integer](#arbitrary-len-int-note)open Big\_int;;

let n = big\_int\_of\_int 7;;

let m = big\_int\_of\_int 12;; · // System.Numerics.BigInteger:

let n = 7I

let m = 12I · -- Integer is arbitrary length type:

let n = 7

let m = 12

[arbitrary length integer operators](#arbitrary-len-int-op-note)add\_big\_int n m

sub\_big\_int n m

mult\_big\_int n m

div\_big\_int n m (\\* quotient \*)

mod\_big\_int n m

eq\_big\_int n m

lt\_big\_int n m

gt\_big\_int n m

le\_big\_int n m

ge\_big\_int n m · n + m

n - m

n \* m

n / m

n % m

n = m

n < m

n < m

n <= m

n >= m · n + m

n - m

n \* m

div n m

mod n m

n == m

n < m

n < m

n <= m

n >= m

[rational type](#rational-type-note)Ratio Integer

[rational construction](#rational-construction-note)import Data.Ratio

1 % 7

[rational decomposition](#rational-decomposition-note)import Data.Ratio

numerator (1 % 7)

denominator (1 % 7)

[complex type](#complex-type-note)Complex.tComplex Double

[complex constants](#complex-const-note)Complex.zero

Complex.one

Complex.i

[complex operators](#complex-op-note)Complex.add z w;;

Complex.sub z w;;

Complex.mul z w;;

Complex.div z w;;

[complex construction](#complex-construction-note){Complex.re=1.0; Complex.im=2.0} · System.Numerics.Complex(1.0, 2.0) · import Data.Complex

1 :+ 2.0

[complex decomposition](#complex-decomposition-note)let z = {Complex.re=1.0; Complex.im=2.0};;

z.Complex.re;;

z.Complex.im;;

Complex.arg z;;

Complex.norm z;;

Complex.conj z;;import Data.Complex

realPart (1 :+ 2)

imagPart (1 :+ 2)

phase (1 :+ 2)

magnitude (1 :+ 2)

conjugate (1 :+ 2)

[random number](#random-num-note)

_uniform int, uniform float, normal float_Random.int 100

Random.float 1.0

_none_ · let rnd = System.Random()

rnd.Next(0, 100)

rnd.NextDouble()

_none_ · -- $ cabal install random

import System.Random

getStdRandom (randomR (0, 99))

getStdRandom (randomR (0.0, 1.0))

_none_

[random seed](#random-seed-note)

_set, get, restore_Random.init 17;;

let seed = Random.get\_state();;

Random.set\_state seed;; · let rnd = System.Random(17)

_none_

_none_ · -- $ cabal install random

import System.Random

setStdGen $ mkStdGen 17

seed <- getStdGen

setStdGen seed

[bit operators](#bit-op-note)1 lsl 4

1 lsr 4

1 land 3

1 lor 3

1 lxor 3

lnot 1 · 1 <<< 4

1 >>> 4

1 &&& 3

1 \|\|\| 3

1 ^^^ 3

~~~ 1 · import Data.Bits

x = 1 :: Integer

y = 3 :: Integer

shiftL x 4

shiftR x 4

x .&. y

x .\|. y

xor x y

complement x

[binary, octal, and hex literals](#binary-octal-hex-literals-note)0b101010

0o52

0x2a · 0b101010

0o52

0x2a · _none_

052

0x2a

[radix](#radix-note)

[strings](#strings-note)

sml · ocaml · f# · haskell

[string type](#str-type-note) · string · string · string · String

[string literal](#str-literal-note) · "Hello, World!" · "Hello, World!" · "Hello, World!" · "Hello, World!"

[newline in literal](#newline-in-str-literal-note)_no_ · _yes_ · _no_

[literal escapes](#str-esc-note) · \\000 \\a \\b \\f \\n \\r \\t \\v \\040 · \\b \\n \\r \\t \\" \\' \\\

\_ooo_ \\x_hh_ · \\b \\n \\r\ t \\" \\' \\\

\\u_hhhh_ \\U_hhhhhhhh_ · \\a \\b \\f \\n \\r \\t \\v \\" \\& \\' \\\

\\o_o..._ \_d..._ \\x_h..._

_Octal, decimal, and hex escapes denote Unicode characters and can contain anywhere from 1 to 7 digits. The max values are \\o4177777, \\1114111, and \\x10ffff. The \\& escape does not represent a character, but can separate a numeric backslash escape sequence from a following digit._

[format string](#format-str-note)sprintf "foo %s %d %.2f" "bar" 7 3.1415 · import Text.Printf

printf "foo %s %d %.2f" "bar" 7 3.1415

[concatenate](#str-concat-note) · "Hello" ^ ", " ^ "World!" · "Hello" ^ ", " ^ "World!" · "Hello" + ", " + "World!" · "Hello" ++ ", " ++ "World!"

[replicate](#str-replicate-note)String.make 80 '-' · String.replicate 80 "-" · concat ( replicate 80 "-" )

[translate case](#translate-case-note)

_to upper, to lower_String.uppercase "hello"

String.lowercase "HELLO" · "hello".ToUpper()

"HELLO".ToLower() · import Data.Char

map toUpper "hello"

map toLower "HELLO"

[capitalize](#capitalize-note)String.capitalize "hello"

[trim](#trim-note)

_both sides, left, right_String.trim " hello " · " hello ".Trim()

" hello".TrimStart()

"hello ".TrimEnd()

[pad](#pad-note)

_on left, on right_"hello".PadLeft(10, ' ')

"hello".PadRight(10, ' ')

[number to string](#num-to-str-note)"two: " ^ string\_of\_int 2

"pi: " ^ float\_of\_string 3.14 · "two: " + string 2

"pi: " + string 3.14 · "two: " ++ (show 2)

"pi: " ++ (show 3.14)

[string to number](#str-to-num-note) · Int.toString 3

Real.toString 3.14 · 7 + int\_of\_string "12"

73.9 +. float\_of\_string ".037" · 7 + int "12"

73.9 + float ".037 · 7 + (read "12")::Integer

73.9 + (read "0.037")::Double

_raises exception if string doesn't completely parse_

[join](#join-note)System.String.Join(" ", \["do"; "re"; "mi"\])

[split](#split-note)"do re mi".Split(' ')

[character type](#char-type-note) · char · char · char · Char

[character literal](#char-literal-note) · #"h" · 'h' · 'h' · 'h'

[length](#str-len-note) · size "hello" · String.length "hello" · "hello".Length · length "hello"

[index of substring](#index-substr-note)"hello".IndexOf("hell")

[extract substring](#substr-note) · substring ("hello",0,4) · String.sub "hello" 0 4 · "hello".Substring(0, 4) · drop 0 (take 4 "hello")

[extract character](#extract-char-note) · String.sub ("hello", 0) · "hello".\[0\] · "hello".\[0\] · "hello" !! 0

[chr and ord](#chr-ord-note) · ord #"a"

chr 97 · Char.code 'a'

Char.chr 97 · int 'a'

char 97 · Char.ord 'a'

Char.chr 97

[dates and time](#dates-time-note)

sml · ocaml · f# · haskell

[date and time types](#dates-time-types-note)ClockTime CalendarTime TimeDiff

[current date and time](#current-date-time-note)import Time

t <- getClockTime

[current unix epoch](#current-unix-epoch-note)open Unix;;

(\\* float: \*)

time();;import System.Time

getClockTime >>= (\\(TOD sec \_) -> return sec)

[arrays](#arrays-note)

sml · ocaml · f# · haskell

literal

size

lookup

update

out-of-bounds

[lists](#lists-note)

sml · ocaml · f# · haskell

[literal](#list-literal-note) · \[1, 2, 3\] · \[1; 2; 3\] · \[1; 2; 3\] · \[1, 2, 3\]

[empty list](#empty-list-note)\[\]\[\]

[empty list test](#empty-list-test-note)let list = \[1; 2; 3\];;

list == \[\]let list = \[1, 2, 3\]

list == \[\]

null list

[cons](#cons-note) · 1 :: \[2, 3\] · 1 :: \[2; 3\] · 1 :: \[2; 3\] · 1 : \[2, 3\]

[head](#head-note) · List.hd \[1, 2, 3\] · List.hd \[1; 2; 3\] · List.head \[1; 2; 3\] · head \[1, 2, 3\]

[tail](#tail-note) · List.tl \[1, 2, 3\] · List.tl \[1; 2; 3\] · List.tail \[1; 2; 3\] · tail \[1, 2, 3\]

[head and tail of empty list](#head-tail-empty-list-note)_exceptions__exceptions_

[length](#list-length-note) · List.length \[1, 2, 3\] · List.length \[1; 2; 3\] · List.length \[1; 2; 3\] · length \[1, 2, 3\]

[nth element](#nth-elem-of-list-note) · List.nth (\[1, 2, 3\], 0) · List.nth \[1; 2; 3\] 0 · List.nth \[1; 2; 3\] 0 · \[1, 2, 3\] !! 0

[element index](#list-elem-index-note)import Data.list

-- Just 1:

elemIndex 8 \[7, 8, 9\]

-- Nothing:

elemIndex 10 \[7, 8, 9\]

[update](#update-list-note)

[concatenate](#concat-list-note)

_two lists, list of lists_ · \[1, 2\] @ \[3, 4\]

List.concat \[\[1, 2\], \[3, 4\]\] · \[1; 2\] @ \[3; 4\]

List.append \[1; 2\] \[3; 4\]

List.concat \[\[1; 2\]; \[3; 4\]\] · \[1; 2\] @ \[3; 4\]

List.append \[1; 2\] \[3; 4\]

List.concat \[\[1; 2\]; \[3; 4\]\] · \[1, 2\] ++ \[3, 4\]

concat \[\[1, 2\], \[3, 4\]\]

[last](#list-last-note)

_and butlast_last \[1, 2, 3\]

init \[1, 2, 3\]

[take](#list-take-note)take 2 \[1, 2, 3\]

[drop](#list-drop-note)drop 2 \[1, 2, 3\]

[iterate](#iterate-over-list-note) · fun f i = print ((Int.toString i) ^ "\\n");

List.app f \[1, 2, 3\]; · let f i =

print\_endline (string\_of\_int i);;

List.iter f \[1; 2; 3\];; · let f i =

System.Console.WriteLine(string i)

List.iter f \[1; 2; 3\] · mapM\_ print \[1, 2, 3\]

[reverse](#reverse-list-note) · List.rev \[1, 2, 3\] · List.rev \[1; 2; 3\] · List.rev \[1; 2; 3\] · reverse \[1, 2, 3\]

[sort](#sort-list-note)List.sort min \[1; 3; 2; 4\]

List.sort max \[1; 3; 2; 4\] · List.sort \[1; 3; 2; 4\] · import Data.List

sort \[1, 3, 2, 4\]

[map](#map-list-note) · List.map (fn (x) => x + 2) \[1, 2, 3\]; · List.map (( \* ) 2) \[1; 2; 3\] · List.map (( \* ) 2) \[1; 2; 3\] · map (\\x -> x \* x) \[1, 2, 3\]

[filter](#filter-list-note) · List.filter (fn (x) => x > 2) \[1, 2, 3\]; · List.filter ((<) 2) \[1; 2; 3\] · List.filter ((<) 2) \[1; 2; 3\] · filter (\\x -> x > 2) \[1, 2, 3\]

[fold from left](#fold-list-left-note) · List.foldl (op +) 0 \[1, 2, 3\]; · List.fold\_left (+) 0 \[1; 2; 3\] · List.fold (-) 0 \[1; 2; 3\] · foldl (+) 0 \[1, 2, 3\]

[fold from right](#fold-list-right-note)List.fold\_right (-) \[1; 2; 3\] 0 · List.foldr (op -) 0 \[1, 2, 3\]; · foldr (-) 0 \[1, 2, 3\]

[membership](#list-member-note)List.mem 3 \[1; 2; 3\]elem 3 \[1, 2, 3\]

[universal test](#universal-test-list-note)List.for\_all (fun x -> x > 2) \[1; 2; 3\];; · List.forall (fun x -> x > 2) \[1; 2; 3\] · all (\\x -> x > 2) \[1, 2, 3\]

[existential test](#existential-test-list-note)List.exists (fun x -> x > 2) \[1; 2; 3\];; · List.exists (fun x -> x > 2) \[1; 2; 3\] · any (\\x -> x > 2) \[1, 2, 3\]

[zip lists](#zip-list-note)(\* list of tuples \*)

List.combine \[1; 2; 3\] \['a'; 'b'; 'c'\]-- list of tuples:

zip \[1, 2, 3\] \['a', 'b', 'c'\]

[tuples](#tuples-note)

sml · ocaml · f# · haskell

[literal](#tuple-literal-note) · (1, "hello", true) · (1, "hello", true) · (1, "hello", true) · (1, "hello", True)

[lookup](#tuple-lookup-note) · #1 (1, "hello", true) · match (1, "hello", true) with \_, x, \_ -> x · match (1, "hello", true) with \_, x, \_ -> x · (\\(a, \_, \_) -> a) (1, "hello", True)

[pair lookup](#pair-lookup-note) · #1 (12,"December")

#2 (12,"December") · fst (12, "December")

snd (12, "December") · fst (12, "December")

snd (12, "December") · fst (12, "December")

snd (12, "December")

[dictionaries](#dictionaries-note)

sml · ocaml · f# · haskell

[functions](#functions-note)

sml · ocaml · f# · haskell

[define function](#def-func-note) · fun average a b = ( a + b ) / 2.0; · let average a b = ( a +. b ) /. 2.0;; · let average a b = ( a + b ) / 2.0 · average a b = (a + b) / 2.0

[invoke function](#invoke-func-note)(\* 4.5: \*)

average 1.0 2.0 +. 3.0;;

(\* 3.0: \*)

average 1.0 (2.0 +. 3.0);; · // 4.5:

average 1.0 2.0 + 3.0

// 3.0:

average 1.0 (2.0 + 3.0) · -- 4.5:

average 1 2 + 3

-- 3.0:

average 1 (2 + 3)

average 1 $ 2 + 3

[named parameter](#named-parameter)let subtract ~m ~s = m - s;;

subtract ~s: 3 ~m: 7;;_none_

[named parameter default value](#default-value)let logarithm ?(base = (exp 1.0)) x = log x /. (log base);;

logarithm 2.718;;

logarithm ~base: 2.0 10.0;;_none_

[piecewise defined function](#piecewise-defined-function) · val to\_s = fn Red => "red"

\| Green => "green"

\| Blue => "blue"; · let to\_s = function Red -> "red"

\| Green -> "green"

\| Blue -> "blue";;to\_s Red = "red"

to\_s Green = "green"

to\_s Blue = "blue"

[recursive function](#recursive-function) · fun range a b =

if a > b then \[\]

else a :: range (a + 1) b; · let rec range a b =

if a > b then \[\]

else a :: range (a+1) b;;range a b = if a > b then \[\] else a : range (a+1) b

[mutually-recursive-functions](#mutually-recursive-functions)let rec even n = if n = 0 then true else odd (n-1)

and odd n = if n = 0 then false else even (n-1);;

[anonymous function](#anonymous-func) · fn x => fn y => (x + y) / 2.0 · fun x -> fun y -> (x +. y) /. 2.0 · fun x -> fun y -> (x + y) / 2.0 · \\x y -> (x+y) / 2.0

[infix operator in prefix position](#infix-prefix) · (op \* ) (3, 4) · ( \\* ) 3 4;;( \\* ) 3 4

[function in infix position](#function-infix)_none_add x y = x + y

3 ‘add\` 4

[currying](#currying) · un plus x y = x + y;

val plus2 = plus 2;

plus2 7; · let plus2 = (+) 2;;plus2 = (+) 2

[composition](#composition)f x = x + 2

g x = x \* 3

(f . g ) 4

[function composition operator](#function-composition) · fun double x = 2 \* x;

val quadruple = double o double; · _none_double x = 2 \* x

quadruple x = double . double

[lazy evaluation](#lazy-evaluation)let arg1 x y = x;;

arg1 7 (lazy (1/0) );;_lazy evaluation is default:_

arg1 x y = x

arg1 7 (error "bam!")

[strict evaluation](#strict-evaluation)_default behavior_ · _default behavior_ · arg1 x y = seq y x

arg1 7 (error "bam!")

[execution control](#execution-control-note)

sml · ocaml · f# · haskell

[if](#if) · f x > 0 then

print "pos\\n"

else

(); · if x > 0 then

print\_endline "pos";; · if x > 0 then

printfn "pos" · if x > 0

then putStrLn "pos"

else return ()

[if else-if else](#if-else-if-else) · if x > 0 then print "pos" else if x < 0 then print "neg" else print "zero"; · if x > 0 then

print\_endline "pos"

else

if x < 0 then

print\_endline "neg"

else

print\_endline "zero";; · if x > 0 then

printfn "pos"

else

if x < 0 then

printfn "neg"

else

printfn "zero" · if x > 0

then putStrLn "pos"

else if x < 0

then putStrLn "neg"

else putStrLn "zero"

[sequencing](#sequencing)print\_endline "one";

print\_endline "two";

print\_endline "three";; · printfn "one"

printfn "two"

printfn "three" · do

putStrLn "one"

putStrLn "two"

putStrLn "three"

[while](#while)let i = ref 0;;

while !i < 10 do

print\_endline (string\_of\_int !i);

i := !i + 1

done;; · let i = ref 0

while !i < 10 do

printfn "%d" !i

i := !i + 1

[for](#for)for i = 1 to 10 do

let s = string\_of\_int i in

print\_endline s

done;;

[for in reverse](#reverse-for)for i = 10 downto 1 do

let s = string\_of\_int i in

print\_endline s

done;;

[list iteration](#list-iteration)_none_

[loop](#loop)let rec loop i =

if i <= 10 then begin

print\_endline (string\_of\_int i);

loop (i+1)

end in

loop 0;;

[exceptions](#exceptions-note)

sml · ocaml · f# · haskell

[raise error](#raise-error)raise (Failure "bam!");;

_or_

failwith "bam!";;error "bam!"

[handle error](#handle-error)let x = try 1 / 0 with Division\_by\_zero -> 0;;

[type of exceptions](#exception-type)exn

[user defined exception](#user-exception)exception Foo of string;;

raise (Foo "invalid input");;

[standard exceptions](#standard-exceptions)Division\_by\_zero

Failure _string_

Not\_found

Invalid\_argument _string_

Match\_failure (_string_, _int_, _int_)

Assert\_failure (_string_, _int_, _int_)

Out\_of\_memory

Stack\_overflow

[assert](#assert)assert(1 = 0);;

[concurrency](#concurrency-note)

sml · ocaml · f# · haskell

[file handles](#file-handles-note)

sml · ocaml · f# · haskell

standard file handlesstdin stdout stderr · stdin stdout stderr · import System.Posix.IO

stdInput stdOutput stdError

read line from stdinlet line = read\_line();;line <- getLine

end-of-file behavior_raises_ End\_of\_file_when last data is returned,_ hIsEOF _will return True. Reading after end-of-file throws an exception._

chomp

[write line to stdout](#write-line-stdout)print\_endline "lorem ipsum";; · printfn "lorem ipsum" · putStrLn "lorem ipsum"

write formatted string to stdout

open file for readinglet f = open\_in "/etc/passwd";;import System.IO

f <- openFile "/etc/hosts" ReadMode

open file for writinglet f = open\_out "/tmp/ocaml.out";;import System.IO

f <- openFile "/tmp/test" WriteMode

open file for appendingimport System.IO

f <- openFile "/tmp/err.log" AppendMode

close fileimport System.IO

hClose f

i/o errors

[read line](#read-line) · fun displayFile(file: string) =

let

val f = TextIO.openIn file

fun iter(s: string option) =

case s of

NONE =>

(TextIO.closeIn f)

\| SOME(line) =>

(print line;

iter(TextIO.inputLine f))

in

iter(TextIO.inputLine f)

end

displayFile("/etc/passwd"); · let ic = open\_in "/etc/passwd" in

let line = input\_line ic in

print\_endline line;;import IO

readAndPrintLines h = do

eof <- hIsEOF h

if eof

then return ()

else do

line <- hGetLine h

putStrLn line

readAndPrintLines h

main = do

h <- openFile "/etc/passwd" ReadMode

readAndPrintLines h

iterate over file by line

read file into array of strings

read file into string

write string

[write line](#write-file) · val file = "/tmp/test-sml";

val f = TextIO.openOut file;

TextIO.output(f, "hello out\\n");

TextIO.closeOut f; · open Printf

let oc = open\_out "/tmp/test-ocaml" in

fprintf oc "hello out\\n";

close\_out oc;;s = "hello out\\n"

f = "/tmp/test-haskell"

main = writeFile f s

flush file handle

end-of-file test

get and set filehandle position

[files](#files-note)

sml · ocaml · f# · haskell

file test, regular file testopen Unix

try Some (stat "/etc/hosts") with

Unix\_error (ENOENT, \_, \_) -> None

(stat "/etc/hosts").st\_kind = S\_REGimport System

Directory.doesFileExist "/etc/hosts"

import Control.Monad

import System.Posix.Files

liftM isRegularFile (getFileStatus "/etc/hosts")

file size(stat "/etc/hosts").st\_sizeimport Control.Monad

import System.Posix.Files

liftM fileSize (getFileStatus "/etc/hosts")

is file readable, writable, executableopen Unix

try access "/tmp/bar" \[R\_OK\]; true with

Unix.Unix\_error (EACCES, \_, \_) -> false;;

try access "/tmp/bar" \[W\_OK\]; true with

Unix.Unix\_error (EACCES, \_, \_) -> false;;

try access "/tmp/bar" \[X\_OK\]; true with

Unix.Unix\_error (EACCES, \_, \_) -> false;;import Control.Monad

liftM readable

(getPermissions "/etc/hosts")

liftM writable

(getPermissions "/etc/hosts")

liftM executable

(getPermissions "/etc/hosts")

set file permissionsopen Unix

chmod "/tmp/foo" 0o755import System.Posix.Files

setFileMode "/tmp/foo" ownerModes

setFileMode "/tmp/foo" groupReadMode

setFileMode "/tmp/foo" groupExecuteMode

setFileMode "/tmp/foo" otherReadMode

setFileMode "/tmp/foo" otherExecuteMode

copy file, remove file, rename fileopen Unix

_??_

unlink "/tmp/foo"

rename "/tmp/bar" "/tmp/foo"import System.Directory

copyFile "/tmp/foo" "/tmp/bar"

removeFile "/tmp/foo"

renameFile "/tmp/bar" "/tmp/foo"

create symlink, symlink test, readlinkopen Unix

symlink "/etc/hosts" "/tmp/hosts"

(lstat "/tmp/hosts").st\_kind = S\_LNK

readlink "/tmp/hosts"import System.Posix.Files

createSymbolicLink "/etc/hosts" "/tmp/hosts"

_??_

readSymbolicLink "/tmp/hosts"

generate unused file nameopen Filename

(\\* prefix and suffix: \*)

temp\_file "foo" ".txt"

[directories](#directories-note)

sml · ocaml · f# · haskell

build pathnameopen Filename

concat "/etc" "hosts"import System.FilePath ((</>))

let path = "/etc" </> "hosts"

dirname and basenameopen Filename

dirname "/etc/hosts"

basename "/etc/hosts"import System.FilePath

takeFileName "/etc/hosts"

takeDirectory "/etc/hosts"

iterate over directory by fileimport System

-- returns IO \[FilePath\]

Directory.getDirectoryContents "/etc"

make directory(\\* opam install fileutils \*)

open FileUtil

mkdir ~parent:true "/tmp/foo/bar"import System.Directory

createDirectoryIfMissing True

"/tmp/foo/bar"

remove empty directoryopen Unix

rmdir "/tmp/foodir"import System.Directory

removeDirectory "/tmp/foodir"

remove directory and contentsimport System.Directory

removeDirectoryRecursive "/tmp/foodir"

directory testimport System

Directory.doesDirectoryExist "/tmp"

temporary directory

[processes and environment](#processes-environment-note)

sml · ocaml · f# · haskell

[command line arguments](#command-line-arg)for i = 0 to Array.length Sys.argv - 1 do

print\_endline i Sys.argv.(i)

doneimport System

printArgs args = do

if length args == 0

then return ()

else do

putStrLn (head args)

printArgs (tail args)

main = do

a <- getArgs

printArgs a

[program name](#program-name-note)import System

s <- getProgName

[getopt](#getopt-note)

[get and set environment variable](#env-var-note)open Unix

s = getenv "HOME"

putenv "PATH" "/bin"import System.Posix.Env

s <- getEnv "HOME"

putEnv "PATH=/bin"

[get pid, parent pid](#pid-note)open Unix

let pid = getpid()

let ppid = getppid()import System.Posix.Process

pid <- getProcessID

ppid <- getParentProcessID

[get user id and name](#user-id-name-note)let uid = getuid()

let username =

(getpwuid (getuid())).pw\_nameimport System.Posix.User

uid <- getRealUserID

username <- getLoginName

[exit](#exit-note)exit 0

exit 1import System.Exit

exitWith ExitSuccess

_to return nonzero status:_

exitWith (ExitFailure 1)

[set signal handler](#signal-handler-note)

[external command](#external-cmd-note)import System.Cmd

rawSystem "ls" \["-l", "/tmp"\]

[escaped external command](#escaped-external-cmd-note)

[backticks](#backticks-note)

[libraries and namespaces](#libraries-namespaces-note)

sml · ocaml · f# · haskell

[namespace example](#namespace-example)_Foo/Bar.hs_

module Foo.Bar where

data Baz = Baz

say Baz = putStrLn "hello"

_Main.hs_

module Main where

import Foo.Bar

baz = Baz

main = say baz

_to compile and run_

$ ghc -c Foo/Bar.hs

$ ghc Main.hs

$ ./Main

hello

[namespaces](#namespaces)values, constructors, type variables, type constructors, type classes, modules

[file name restrictions](#file-name)_module_ Foo.Bar _must be in_ Foo.ml_module_ Foo.Bar _must be in_ Foo/Bar.hs

[namespace](#import)open Graphics;;import Data.Bytestring

[namespace creation](#namespace-creation)_put code in file_ MODULE\_NAME _.ml_

[namespace alias](#namespace-alias)module Gr = Graphics;;import qualified Data.Bytestring as B

[namespace separator](#namespace-separator)..

[subnamespace](#subnamespace)_in A.ml:_

module B =

sig

val display\_instruction : unit -> unit

end =

struct

let msg = "attack"

let display\_instruction () = print\_endline msg

end

_in client source:_

A.B.display\_instruction;;

[package manager setup](#pkg-manager-setup-note)_do this once:_

$ opam init

_for each shell session:_

$ eval $(opam config env)

[package manager](#pkg-manager-note)

_search; install; list installed_$ opam search utop

$ opam install utop

$ opam list --installed$ cabal list parsec

$ cabal install parsec

$ cabal list --installed

[compile app using package](#pkg-compile-note)

[user-defined types](#user-defined-types-note)

sml · ocaml · f# · haskell

[type synonym](#type-synonym-note) · type name = string; · type name = string;; · type name = string · type Name = String

[sum type](#sum-type-note) · datatype color = Red \| Green \| Blue; · type color = Red \| Green \| Blue;;

let col = Red;;

(\\* evaluates to true: \*)

col < Green;; · type color = Red \| Green \| Blue

let col = Red

// evaluates to true:

col < Green · data Color = Red \| Green \| Blue

col = Red

-- this won’t compile:

col < Green

tuple product type with one field · datatype special\_int = SpecialInt of int;

val x = SpecialInt 7; · type special\_int = SpecialInt of int;;

let n = SpecialInt 7;; · type special\_int = SpecialInt of int

let n = SpecialInt 7 · data SpecialIntType = SpecialInt Integer

n = SpecialInt 7

tuple product type with two fields · datatype int\_pair = IntPair of int \* int;

val y = IntPair (7, 11); · type int\_pair = IntPair of int \* int;;

let p = IntPair (7, 11);; · type int\_pair = IntPair of int \* int

let p = IntPair (7, 11) · data IntPairType = IntPair Integer Integer

p = IntPair 7 11

record product type · type customer = {id:int, name:string, address:string} · type customer = {

id: int;

name: string;

address: string

};; · type customer = {

id: int;

name: string;

address: string

} · data CustomerType = Customer {

customerId :: Integer,

name :: String,

address :: String

}

record product type literal · {id=7, name="John", address="Topeka, KS"} · let cust = {

id=7;

name="John";

address="Topeka, KS"

};; · {id=7; name="John"; address="Topeka, KS"} · Customer {

customerId=7,

name="John",

address="Topeka, KS" }

[generic type](#generic-type-note) · datatype ('a, 'b) twosome =

Twosome of 'a \* 'b;

val z = Twosome ("pi", 3.14); · type ('a, 'b) twosome =

Twosome of 'a \* 'b;;

let p = Twosome ("pi", 3.14);; · type ('a, 'b) twosome =

Twosome of 'a \* 'b

let p = Twosome ("pi", 3.14) · data TwosomeType a b = Twosome a b

p = Twosome ("pi", 3.14)

[recursive type](#recursive-type) · datatype binary\_tree =

Leaf of int

\| Tree of binary\_tree \* binary\_tree; · type binary\_tree =

\| Leaf of int

\| Tree of binary\_tree \* binary\_tree;; · type binary\_tree =

\| Leaf of int

\| Tree of binary\_tree \* binary\_tree · data BinaryTree = Leaf Integer \| Tree BinaryTree BinaryTree

pattern match sum type · val c = Red;

case c of Red => "red"

\| Blue => "blue"

\| Green => "green"; · let col = Red;;

let s = match col with

\| Red -> "red"

\| Blue -> "blue"

\| Green -> "green";;c = Red

case c of Red -> "red"

Green -> "green"

Blue -> "blue"

pattern match product type

[pattern match guard](#match-guard) · _none; use_ if · match i with j when i < 0 -> -j \| j -> j;;_none, use if or piecewise function definition_

[pattern match catchall](#match-catchall) · fun to\_s c = case c of Red => "red" \| \_ => "not red"; · let to\_s c = match c with Red -> "red" \| \_ -> "not red";;

to\_s Green;;c = Green

case c of Red -> "red"; \_ -> "not red"

[objects](#objects-note)

sml · ocaml · f# · haskell

[class definition](#class-definition)class counter = object

val mutable n = 0

method incr = n <- n+1

method get = n

end;;

[object creation](#object-creation)let c = new counter;;

[method invocation](#method-invocation)c#incr;;

c#get;;

[field access](#field-access)_none_

[inheritance and polymorphism](#inheritance-polymorphism-note)

sml · ocaml · f# · haskell

overload function

[inheritance](#inheritance)

[net and web](#net-web-note)

sml · ocaml · f# · haskell

[unit test](#unit-tests-note)

sml · ocaml · f# · haskell

[debugging and profiling](#debugging-profiling-note)

sml · ocaml · f# · haskell

[repl](#repl-note)

sml · ocaml · f# · haskell

[invoke repl](#invoke-repl) · $ sml · $ ocaml

_Use this if you want history:_

$ rlwrap ocaml

_The utop toplevel, which can be installed via opam, also provides history._ · _Mono:_

$ fsharpi

_In visual studio, highlight code and press ALT+ENTER._ · $ ghci

[repl limitations](#repl-limitations)_Must use let to define values and functions; when defining functions with multiple equations the equations must be separated by semicolons; the clauses of case/of statements must be separated by semicolons; it is not possible to define data types._

[repl last value](#repl-last-value) · it · _none_ · it · it

[help](#help)_none_:?

[quit](#quit)^D · #quit;;

[inspect type](#inspect-type)_repl displays the type of any expression entered_let a = 3

:type a

[inspect namespace](#inspect-namespace)module Unix = Unix;;

[load source file](#load-source) · use "hello.ml"; · #use "hello";;:edit hello.hs

:load hello

[load package](#load-pkg-note)_consider adding to_ .ocamlinit:

#use "topfind";;

\# thread;;

#require "core";;

open Core.Std;;

[search path](#search-path)#directory "libdir";;

[set search path on command line](#search-path-command-line)ocaml -Ilibdir

\\_\\_\\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ · \\_\\_\\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ · \\_\\_\\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ · \\_\\_\\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_

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

