##### Formatted - no table

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