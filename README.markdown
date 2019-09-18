MiniTypeScript implements parametric polymorphism on top of STLC along with union types, intersection types, distributive conditional types, and mapped types.

This project is built on top of the [PL Zoo](http://plzoo.andrej.com/) mini-language framework.

# Features

## Union types

```ml
let incrementOrZero = fun f(x): (int | bool) -> int is
    match x with
    | int as y -> y + 1
    | bool as y -> 0
;;

let z = incrementOrZero 3;;
# => 4
let z = incrementOrZero true;;
# => 0
```

## Intersection types

Use case: Say we want the return type of a function to be specific even though the input type is a union. `negate true` should return `bool`, not `int | bool`, and `negate 7` should return `int`, not `int | bool`.

```ml
let zeroMinusX = fun f(x): int -> int is (0 - x);;
let toggle = fun f(x): bool -> bool is (if x then false else true);;

let negate = zeroMinusX & toggle;;
# => (int -> int) & (bool -> bool)

negate true;;
# => false

negate 7;;
# => -7

negate (negate true);;
# => true
toggle (negate true);;
# No need to pattern-match since the return type is bool.
# => true
```

## Higher-Order Types

```ml
type Id = tfun f(A): * -> * is A;;
type Const = tfun f(A): * -> * -> * is tfun g(B): * -> * is A;;

type IdInt = Id int;;
# type: int
type ConstIntBool = Const int bool;;
# type: int

let id = \A:* . fun f(x): A -> A is x;;

id [IdInt] 3;;
id [ConstIntBool] 3;;
```

## If Type-operator (using Church encoding)

```ml
type True = tfun f(T): * -> * -> * is
     tfun g(F): * -> * is T;;
type False = tfun f(T): * -> * -> * is
     tfun g(F): * -> * is F;;
type If = tfun f(B): (* -> * -> *) -> * -> * -> * is
     tfun g(T): * -> * -> * is
     	  tfun h(F): * -> * is
	       B T F;;
type Not = tfun f(B): (* -> * -> *) -> * -> * -> * is
     tfun g(T): * -> * -> * is
     	  tfun h(F): * -> * is
	       B F T;;

type Lion = {tail_length: int, meow_loudness: int};;
type Tiger = {tail_length: int, meow_loudness: int};;
type Zebra = {tail_length: int, num_stripes: int};;
type Shark = {num_fins: int, num_gills: int};;

type Foo = If False Lion Zebra;;
# type: Zebra

type TryNot = If (Not False) Lion Zebra;;
# type: Lion

type Foo = If (Lion extends {tail_length: int}) Lion Tiger;;
# type: Lion

type Foo = If (Not (Lion extends {tail_length: int})) Lion Never;;
# type: Never


# Never.
let tryNever = fun f(x): Never -> int is 3;;
# Will give an error.
# tryNever 7;;
```

## Distributive Conditional types

```ml
type ExtractCat = tfun f(A): * -> * is
     If (A extends { meow_loudness: int }) A Never;;
type ExtractNonCat = tfun f(A): * -> * is
     If (Not (A extends { meow_loudness: int })) A Never;;

type Animal = Lion | Zebra | Tiger | Shark;;

type Cat = ExtractCat Over Animal;;
# => Lion | Tiger

type NonCat = ExtractNonCat Over Animal;;
# => Zebra | Shark
```

## Mapped types

```ml
type ExcludeKey = tfun f(K): * -> * -> * is
     tfun g(A): * -> * is
     	  If (A extends K) Never A;;

type ExcludeUserTypeKey = ExcludeKey "user_type";;

type Test = ExcludeUserTypeKey Over ("emailAddress" | "user_type" | "foo");;
# => "emailAddress" | "foo"

# Remove the user_type field from the record.
type ExcludeUserTypeField = tfun f(A): * -> * is
     (MapUnionToRecord {(tfun k(K): * -> * is K): (tfun v(K): * -> * is A[K])}
     		       (ExcludeUserTypeKey Over (Keyof A)));;

type Test = ExcludeUserTypeField { user_type: "LOGGED_IN", area_code: int, is_premium: bool };;
# => { area_code: int, is_premium: bool }
```

# Code

The code for System F is in `src/systemf`.

The code for MiniTypeScript is in `src/uniontypes`.
