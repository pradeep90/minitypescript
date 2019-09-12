
# TypeScript Feature Wish list

## Usability

+ Have a flag for toggling the type of type system.

## Organize the typechecker like a theorem-prover

Make the logical inference explicit.

`subtype` is probably implies.

Remove the other features from the ast. Implement bool using ADTs. `plus`, `and`, etc. should be built-in functions with their built-in implications. `if` should be pattern-matching on Bool.

Constructors are used to introduce a type or proposition. Pattern matches are used to eliminate disjunctions and conjunctions (via deconstruction).

## Conditional types

+ `eval` example for GADTs.

```haskell
{-# LANGUAGE GADTs #-}

data Expr t where  -- phantom 't'
  -- built-in smart constructors
  I :: Int  -> Expr Int
  B :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int

eval :: Expr t -> t
eval (I v) = v
eval (B v) = v
eval (Add x y) = (eval x) + (eval y)

--  eval (Add (I 10) (I 12))
```

(Contrast it to the non-GADT version.)

Source: Haskell Design Patterns by Ryan Lemmer

GADTs let you treat the result of `eval (Add ...)`  as the integer it is instead of treating it like some abstract type `t`.

Wow. To infer that, my typechecker would have to store the type of the function as `(Expr Int -> Expr Int -> Int) | (Bool -> Bool)`. Once the argument type is known to be `Expr Int`, it can infer that the return type must be `Expr Int -> Int`.

Hmm... So, the type `Int -> Bool` is really the logical implication `input type is Int => return type will be Bool`. I guess simple types lead to simple logical implications whereas things like GADTs lead to more complicated, multi-branch implications, like `input type is Expr Int => return type will be Int, but if input type is Bool => return type will be Bool`. The types are there to help the typechecker logically infer the types and thus check them against the provided types. You want to infer as much as possible and that's why you need really powerful type systems. (Curry-Howard correspondence, anybody?)

When you want three functions with the same name and overall input type but different return types, make their type signature `A1 -> B1 | A2 -> B2 | A3 -> B3` so that it's easier to typecheck. (I guess the problem for Haskell would have been in inferring, not in typechecking.)

I guess `typematch x with | A1 -> ... | A2 -> ... | A3 -> ...` indexes into the above type union. Well, it tells you the type of the first argument, which allows you to narrow down the type.

Workflow would be: declare a type for the function (`f :: Int -> String | Bool -> String`); match on the argument (`typematch x with | Int -> ... | Bool -> ...`). An outsider can't tell which type the function is. All he can do is pass in either an Int or a Bool.

However, can you also call it from a function that takes in as argument `String | Bool`, so that you get a result of type `String`?

What are the differences between `(Int | Bool) -> (Int | Bool)` and `(Int -> Int) | (Bool -> Bool)`? The former can return a value of either type; the latter can't. The latter allows you to infer that an input of `Int` will give an output of `Int`; the former doesn't. This is what TypeScript allows with `function process<T extends string | null>(text: T): T extends string ? string : null {}`. This is probably what GADTs give you. Finally, both can accept an input of type `string | null`, such as `process(maybeFoo)`.

Another example from the Advanced Types page:

```typescript
declare function f<T>(x: T): T extends Foo ? string : number;

function foo<U>(x: U) {
    // Has type 'U extends Foo ? string : number'
    let a = f(x);

    // This assignment is allowed though!
    let b: string | number = a;
}
```

I would have thought that the type of `a` would be `string | number`. They say it's `U extends Foo ? string : number`. But by the time you apply `foo` to an argument `x`, you would have got some more information about `U`, so that if you know x's type extends `Foo`, then `a` would automatically get the type `string`, and if x's type doesn't extend `Foo`, then `a` would get `number`. I don't think you have to carry around the extra part about `U extends Foo ?`. You can just say `string | number`.

+ Another use case for GADTs:

```haskell
Fun :: (Lam a -> Lam b) -> Lam (a -> b)
App :: Lam (a -> b) -> Lam a -> Lam b

data Lam T
 = forall b c . Fun (Lam b -> Lam c) with T = b -> c
| forall a . App (Lam (a -> T )) (Lam a)

reduce :: Lam b -> Lam b
reduce (App (Fun f ) t) = f t
```

Source: First-class Phantom Types, Cheney et al, page 4.

The aim is that you say that the input type of `reduce` is `Lam b` and the typechecker knows that it must have been constructed by `App` (since `Fun` returns `Lam (a -> b)`, not `Lam b`).

I guess you could still have a dead case-branch for `Fun`, for fun. The idea is that once you know you're in an `App` branch, you know that the two arguments must have been of types `Lam (a -> b)` and `Lam a`. Once you can type-match against the full type, I don't think there's any ambiguity left.

My only question is: what to do when there are two branches of the same type, like `Int -> Int | Int -> Int`? Wait, that's pointless! There's nothing new you're inferring about the two branches. When it comes to `Int -> Int | Int -> Bool`, you can tell that the result of the first branch is an `Int` and that of the second is `Bool`. Useful information. But `Int -> Int | Int -> Int` in logic is basically `Int => Int || Int => Int`, which is just `Int => Int`.

+ conditional types: (I'm guessing this is like the smart pattern-matching of GADTs)

```typescript
function process<T extends string | null>(
  text: T
): T extends string ? string : null {
  ...
}

typeof process("foo") // => string
typeof process(null) // => null
typeof process(maybeFoo) // => string | null
```

Source: https://artsy.github.io/blog/2018/11/21/conditional-types-in-typescript/

+ refining unions with distributive conditional types

```typescript
type Animal = Lion | Zebra | Tiger | Shark
type ExtractCat<A> = A extends { meow(): void } ? A : never
type Cat = ExtractCat<Animal>
// => Lion | Tiger
```

This looks like the predicate "A is in `Animal` and A has field `meow` => A is in `ExtractCat<Animal>`". So, if someone tries to expect `fins` when given `ExtractCat<Animal>`, you can throw a type error because none of the animals that satisfy the predicate of `ExtractCat<Animal>` have the field `fins`.

Other type predicates you could have are `isArgument Int` or `isReturnType Bool` or `isFunction` or `not`.

Wow, you'll probably need some way of composing predicates. Wait. A predicate is nothing but a higher-kinded type, basically a type operator. `List` takes `Int` and returns `List Int`. `ExtractCat` takes `Animal` and returns another type. That would mean that even `|` is a type operator, albeit of a higher kind. It takes two type operators and returns a new type operator.

Question: Can the typechecker learn that `A | B` means that `A or B` must be true? Will find out.

+ mapped types

```typescript
type ExcludeTypeKey<K> = K extends "type" ? never : K

type Test = ExcludeTypeKey<"emailAddress" | "type" | "foo">
// => "emailAddress" | "foo"

// here's the mapped type
type ExcludeTypeField<A> = { [K in ExcludeTypeKey<keyof A>]: A[K] }

type Test = ExcludeTypeField<{ type: "LOG_IN"; emailAddress: string }>
// => { emailAddress: string }
```

I guess his example makes the type of the second argument depend on the type of the first argument.

Continuing with the System F-omega theme, I'm guessing `keyof` is another type operator. There seems to be some syntactic sugar to represent mapping over the set of keys, but I think the basic idea is the same.

+ index type query and index access operators: `function pluck<T, K extends keyof T>(o: T, propertyNames: K[]): T[K][] {return propertyNames.map(n => o[n]);}`

My language would probably have the type signature as: `fun pluck(o: T, propertyNames: K[]): K extends keyof T => T[K][]`. The corresponding implication would probably be `T and K[] and K extends keyof T => T[K][]`.

This probably follows from the lower-level implication about `o: T and n: K and K extends keyof T => o[n]: T[K]`. Follow that up with the property of map (or its type, if you will), i.e., `f: a -> b and xs: a[] => f xs: b[]`, and you will get the return type `T[K][]`.

How do you tell it that an array `propertyNames` is of the appropriate type `K` such that `K extends keyof T`? You would probably have to either construct it using constant symbols that can be checked to be keys of T or you would have to have got the property names from some method that extracted them from T. Maybe `filterProperties(o: T, f: String -> Bool): [String]`. Would it realize that the strings came from the keys of `T` (because you used `o.keys`) or will you have to specify that? The Car example on the TypeScript webpage used literal strings.

At least in this example:

```typescript
function getProperty<T, K extends keyof T>(o: T, propertyName: K): T[K] {
    return o[propertyName]; // o[propertyName] is of type T[K]
}
```

they specified the fact that `K extends keyof T`. So, that fact was somehow known before this function was reached; probably by looking at the string literals. I guess they can get away with it because they don't have to infer anything. Otherwise, when you said `o[n]`, they would have to infer the condition `K extends keyof T`, just like Haskell infers the condition `Show a` when you ask it to type `f x = show x`, so that you get the overall type `f :: Show a => a -> String`.

## String Literals in TypeScript

Changed the `pluck` example from

```typescript
let makeAndModel: string[] = pluck(taxi, ['manufacturer', 'model']);
```

to

```typescript
let s = 'man';
let s2 = 'ufacturer';
let makeAndModel: string[] = pluck(taxi, [s + s2, 'model']);
```

Got:

> Type 'string' is not assignable to type '"model" | "manufacturer" | "year"'.

The same for

```typescript
let makeAndModel: string[] = pluck(taxi, ['manuf' + 'acturer', 'model']);
```

So, TypeScript can't just handle any string as a type, even though the output is literally the same as the literal type "manufacturer". I'm guessing it doesn't have full-on dependent typing in the sense of types being determined by values. It's more about types being indexed by types. If so, System F-omega should be enough to handle it.

## System F

+ System F requires this: "if a function has a polymorphic type then type applications must be explicitly indicated." (Theorems for Free paper)

I guess I'm currently *inferring* the types. That's why I had to write a `unify` function to solve the constraints.

Polymorphic types should be represented as `forall` so that you can instantiate them. For a normal parameter, the argument type has to imply the parameter type. But for a type parameter, it just instantiates the parameter. For multiple matches in a function type, look for subtype relations. A failure would be A -> a being a super type of x,y -> x. Int <= X <= int, means x is int.

## Church encoding of Discriminated Unions and Union Types

ADTs can be the same way too - you dispatch on the deconstructor - it's either `a -> b` for `Just` or `b` for `Nothing`. Can we avoid the runtime type information? Well, you either have to carry it as a deconstructor or as an explicit type.

How to deal with the names? Would that just be syntactic sugar for defining the name of the constructor? Let's say we can handle that.

How would you handle arbitrary, nameless union types, like `int | bool`? The user won't wrap it up in the appropriate deconstructor, which would be `(int -> b) -> (bool -> b) -> b`. Even if you inserted a wrapper so that you got `x: (int | bool) = IntOrBool (y: int)`, how would you make `IntOrBool` return the correct deconstructor? I guess you would need to insert it with the knowledge of the type. Either the type is known or it is unknown, in which case its type would already be `int | bool` and it would already be wrapped.

What about an extended type like `Foo = A | B` and then `Bar = Foo | C`? Typechecking would remain the same. Dispatch would be the same once you have the right deconstructor. The matter is now of inserting the right constructor when going from `B` to `(A | B) | C`. Now, the inserted wrapper would need to be of the type `((A|B) -> b) -> (C -> b) -> b`. The inner type `(A|B) -> b` would expand to the expected `(A -> b) -> (B -> b) -> b`. Finally, the type-match syntax would be `match x with (A|B) -> (match x with | A -> ... | B -> ...) | C -> ...`.

## Church encoding of Intersection Types

Question: What would be the drawback of carrying around the type at runtime? I guess a type match within a long loop would kill you. No. I'm carrying around the type too, it's just hidden as a function with continuations as arguments.

What about `foo: {x: int, y: int} | {x: int, z: int}` followed by `foo.x`? No need to type match or deconstruct; you can typecheck that the projection is valid. But the value is wrapped, so you have to unwrap it. Ah. You just send the same continuation to both types!

What if you wanted a function with conditional types, one that treated `(int | bool) -> (int | bool)` as `(int -> int) | (bool -> bool)`? You can't define it using the ordinary function definition. You would just expect a parameter with the type as the union of two functions. And, as usual, you would insert a wrapper whenever going from `int -> int` to `(int -> int) | (bool -> bool)`. Could you then typecheck `f 3` to be `int`?

Wait a minute. `function process<T extends string | null>(text: T): T extends string ? string : null {}` is actually `(string -> string) & (null -> null)`. It's an intersection type, not a union type.

Let's figure out intersection types, now that they seem key to GADTs. Say you have `int & bool`. How to represent that? It's probably just `(int -> bool -> b) -> b`. To project out the `int`, i.e., to get `(int -> bool -> int) -> int`, you would just pass in `const`.

If `x` in `f x` is an intersection type, then there's no real problem because `A & B` is a subtype of `A`.

What is the type of `((int -> Foo) & (bool -> Bar)) (int | bool)`? It's `Foo | Bar`. But how will you get that? Our `int | bool` is actually `(int -> b) -> (bool -> b) -> b`. We need to replace `b` with `Foo | Bar` (or rather `(Foo -> c) -> (Bar -> c) -> c`). We already have `int -> Foo`, so we wrap the result with `FooToFooOrBar` to get `int -> Foo | Bar` and likewise with `bool -> Bar`.

If `f` in `f x` is an intersection type `(A -> C) & (B -> D)` and `x` is of type `A`, then we should wrap `x` to make it be of type `A | B`. This is for the evaluation. The typechecker should infer that the output will be of type `C`. If `f` is of the type `(int -> int -> int) & (int -> bool -> bool)`, then you should probably infer that the output will be of type `(int -> int) & (bool -> bool)`.

Basically, when you have a function with an intersection type, you should merge the types to get a union input type like `A | B`. Then, you should wrap the input type if needed to get it to the type `A | B`.

What if you have a function of type `(A -> C) & B`? Then, merging the types should give you just `A -> C`. And, as usual, `A & B` is not a function type.

What if you have a function of type `(A -> C) & (B -> D) & (A -> E)`? I guess you get `(A -> (C & E)) & (B -> D)`.

How would you create a value of type `A & B`? Probably using `product x y`, where `product = \A:*. \B:*. \x:A. \y:B. (\f: forall C: *. A -> B -> C. f x y)`.

# System F-omega

Don't know if I need this.

Maybe have a separate pure implementation of System F-omega so that you can look at the differences between that and your ad hoc mini-TypeScript implementation.

Check in the online interpreter (https://crypto.stanford.edu/~blynn/lambda/typo.html) if you can encode something like a GADT in System F-omega.

## Implementing an ADT using Lambdas

I want something where I can pattern-match constructors with different output types:

```haskell
I :: Int  -> Expr Int
B :: Bool -> Expr Bool

f :: Expr a -> a
-- Or, rather:
-- f :: (Expr Int -> Int) | (Expr Bool -> Bool)
f e = match e (\i -> i) (\b -> b)
```

Hmm... A pattern-match gives you a function from the data type to the arguments of the constructor.

Let's start with the easier data type: `Bool`.

```haskell
true :: ()  -> Bool
false :: () -> Bool
type Bool = a -> a -> a

f :: Bool -> b -> b -> b
f x tb fb = x tb fb
```

Aside: Here's the System F-omega version:

```haskell
typo Bool = forall X . X -> X -> X
true = \X t: X . \f: X . t
false = \X t: X . \f: X . f
true [Nat] 0 1
if = \X b: Bool . \tb: X . \fb: X . (b [X] tb fb)
if [Nat] true 1 0
if [Nat] false 1 0
```

Not clear yet. Let's try `Maybe`:

```haskell
just :: a  -> Maybe a
nothing :: () -> Maybe a
type Maybe a = (a -> b) -> b -> b

f :: Maybe a -> (a -> b) -> b -> b
f x jb nb = x jb nb

-- Now we can see what `just` and `nothing` should be.
just y jb nb = jb y
nothing y jb nb = nb
```

Awesome. Got it. Now I know how to implement ADTs using basic lambdas.

**Hypothesis**: A constructor simply returns a function that gives its arguments to the right continuation. `just x` gives a function that gives x to the continuation for just (`jb`) whereas `nothing` gives a function that gives nothing to the continuation for nothing (`nb`). An ADT is just a function with a continuation for each variant.

Will this work for recursively defined ADTs like List? Yes. (Example from https://crypto.stanford.edu/~blynn/lambda/typo.html)

```haskell
typo Nat = forall X.(X->X)->X->X
typo List = \X.forall R::*.(X->R->R)->R->R
0=\X s:X->X . \z:X.z
succ=\n:Nat.\X s:X->X.\z:X.s(n[X] s z)
cons = \X.\h:X.\t:List X.(\R.\c:X->R->R.\n:R.c h(t [R] c n))
nil  = \X.(\R.\c:X->R->R.\n:R.n)
1 = succ 0
let c = cons[Nat] in c 0(c (succ 1)(c 1 (nil[Nat])))
```

Basically, an ADT can be represented as an F-algebra (such as `(1 + Int x a) -> a` for `List Int`). A function that pattern-matches over an ADT is another F-algebra (such as `(1 + Int x Int) -> Int` for `sum`).

The trouble with GADTs is that they don't fit the pattern of an F-algebra like `F A -> A`. It's more like one output type for each variant, such as `(Int -> Expr Int) | (Bool -> Expr Bool)`. Maybe that falls under the category of `F A -> (G F) A`, where the shape of the return type depends on the shape of the input type. (Not sure what is the formal term for this.)

## How to make GADT constructors have the same return type: The Yoneda Lemma

These are my notes on Gabriel Gonzales's post on [implementing GADTs](http://www.haskellforall.com/2012/06/gadts.html).

> Fortunately, the Yoneda lemma from category theory provides the necessary trick to convert a GADT to an ordinary data type. The Yoneda lemma translated into Haskell is actually more understandable than the equivalent category theory explanation (at least, to me). It simply says that if f is a functor, then the following two types are isomorphic:
>
> `(forall b. (a -> b) -> f b) ~ f a`
>
> ... which means that we can define two functions `fw` and `bw` that can convert back and forth between those two types:
>
> ```haskell
> fw :: (Functor f) => (forall b . (a -> b) -> f b) -> f a
> fw f = f id
>
> bw :: (Functor f) => f a -> (forall b . (a -> b) -> f b)
> bw x f = fmap f x
> ```

I'm guessing that `fw` corresponds to a constructor and `bw` corresponds to a deconstructor.

Gabriel applies the Yoneda isomorphism to the `List a n` GADT.

Here's my trial of his code:

```haskell
Prelude> :{
Prelude| fw :: (Functor f) => (forall b . (a -> b) -> f b) -> f a
Prelude| fw f = f id
Prelude| :}
Prelude> :{
Prelude| bw :: (Functor f) => f a -> (forall b . (a -> b) -> f b)
Prelude| bw x f = fmap f x
Prelude| :}
Prelude> :t bw
bw :: Functor f => f a -> (a -> b) -> f b
Prelude> :set -XExistentialQuantification
Prelude> data List a n = Nil (Z -> n) | forall m. Cons a (List a m) (S m -> n)
Prelude> :t fw Nil
fw Nil :: Functor (List a) => List a Z
Prelude> :{
Prelude| instance Functor (List a) where
Prelude|     fmap f (Nil g) = Nil (f . g)
Prelude|     fmap f (Cons x xs g) = Cons x xs (f . g)
Prelude| :}
Prelude> :t fw Nil
fw Nil :: List a Z
Prelude> :t fw (Cons 1 (fw Nil))
fw (Cons 1 (fw Nil)) :: Num a => List a (S Z)
Prelude> :{
Prelude| head' :: List a (S n) -> a
Prelude| head' (Cons x xs f) = x
Prelude| :}
Prelude> head' (Nil id)

<interactive>:117:8: error:
    + Couldn't match type 'Z' with 'S n0'
      Expected type: List a (S n0)
        Actual type: List a Z
    + In the first argument of 'head'', namely '(Nil id)'
      In the expression: head' (Nil id)
      In an equation for 'it': it = head' (Nil id)
Prelude> head' (fw (Cons 1 (fw Nil)))
1
```

It worked! This solves the problem I'd been struggling with for the past few days: how to encode GADTs so that every constructor has the same return type?

## Last Try

I want to implement in System F-omega the following GADT:

```haskell
data Expr t where
	I :: Int -> Expr Int
	B :: Bool -> Expr Bool
	Add :: Expr Int -> Expr Int -> Expr Int
```

Now, `Expr a` is a functor, so the Yoneda lemma says that `Expr a ~ (forall b . (a -> b) -> Expr b)`.

That means that our constructors become:

```haskell
I :: forall b . Int -> (Int -> b) -> Expr b
B :: forall b . Bool -> (Bool -> b) -> Expr b
Add :: forall b . Expr Int -> Expr Int -> (Int -> b) -> Expr b
```

The trick lies in constructing the return type R. I was able to write `Expr`, `I`, and `B` easily enough. I failed when I tried to write `Add` with its recursive dependence on `Expr`. Let's look at how they did it for `List X`.

```haskell
typo List = \X.forall R.(X->R->R)->R->R
0=\X s:X->X . \z:X.z
succ=\n:Nat.\X s:X->X.\z:X.s(n[X] s z)
cons = \X.\h:X.\t:List X.(\R.\c:X->R->R.\n:R.c h(t [R] c n))
nil  = \X.(\R.\c:X->R->R.\n:R.n)
```

Note that the code within parens in `cons` isn't `(\R.\c:X->R->R.\n:R.c h t)`. It's `(\R.\c:X->R->R.\n:R.c h(t [R] c n))`. In other words, `t: List X` is not of type `R`. You have to get to the inner type `R` within the definition of `List` (`\X.forall R.(X->R->R)->R->R`) by adding all the arguments. So, `t [R] c n` sets the type parameter `R` to `R`, the value parameter `(X->R->R)` to `c`, and the value parameter `R` to `n`. Now, `t [R] c n` is of type `R`.

Got stuck when trying to use an `Expr Nat`:

```haskell
typo Expr = \B. forall R::*->*. (Nat -> (Nat -> B) -> R B) -> (Bool -> (Bool -> B) -> R B) -> (R Nat -> R Nat -> (Nat -> B) -> R B) -> R B
EI = \B x: Nat. \f: Nat -> B. (\R::*->*. \fi: (Nat -> (Nat -> B) -> R B). \fb: (Bool -> (Bool -> B) -> R B). \fa: (R Nat -> R Nat -> (Nat -> B) -> R B). fi x f)
EB = \B x: Bool. \f: Bool -> B. (\R::*->*. \fi: (Nat -> (Nat -> B) -> R B). \fb: (Bool -> (Bool -> B) -> R B). \fa: (R Nat -> R Nat -> (Nat -> B) -> R B). fb x f)

EI [Nat] 1 succ

foobar = \B. \x: Expr Nat. (\R:: *->*. \A. \fi: forall A . (Nat -> (Nat -> A) -> R A). \fb: forall A. (Bool -> (Bool -> A) -> R A). \fa: forall A . (R Nat -> R Nat -> (Nat -> A) -> R A). x [R] (fi [Nat]) (fb [Nat]) (fa [Nat]))
=> type error: App: (Nat -> (Nat -> Nat) -> R Nat) -> (Bool -> (Bool -> Nat) -> R Nat) -> (R Nat -> R Nat -> (Nat -> Nat) -> R Nat) -> R Nat to Nat -> (Nat -> Nat) -> R Nat

-- I don't understand why you can't apply that function. The input type of the function and the type of the argument seem to match.

EAdd = \B x: Expr Nat. \y: Expr Nat. \f: Nat -> B. (\R::*->*. \fi: (Nat -> (Nat -> B) -> R B). \fb: (Bool -> (Bool -> B) -> R B). \fa: (R Nat -> R Nat -> (Nat -> B) -> R B). fa (x [R] fi fb fa))
=> type error: App: (Nat -> (Nat -> Nat) -> R Nat) -> (Bool -> (Bool -> Nat) -> R Nat) -> (R Nat -> R Nat -> (Nat -> Nat) -> R Nat) -> R Nat to Nat -> (Nat -> B) -> R B
```

Finally able to reproduce the error with the following example:

```haskell
typo ListR = \X. forall R::*->*. (X -> R X -> R X) -> R X -> R X
consR = \X. \h: X. \t: ListR X. (\R::*->*. \c: X -> R X -> R X. \n: R X. c h (t [R] c n))

=> type error: App: (X -> R X -> R X) -> R X -> R X to X -> R X -> R X

test = \X. \t: (\A. forall RL::*->*. (X -> RL X -> RL X) -> RL X -> RL X). (\R::*->*. \f: X -> R X -> R X. t [R] f)
=> type error: App: (X -> R X -> R X) -> R X -> R X to X -> R X -> R X
```

**Important**: Simpler error:

```haskell
foo = \X. \R::*->*. \x: R X. \f: R X -> Nat. f x
```

* * * * *

I suspect that Lynn's definition of `List` as `typo List = \X.forall R.(X->R->R)->R->R` (on https://crypto.stanford.edu/~blynn/lambda/typo.html) is somehow wrong or at least incomplete, because it does not use any recursive types, whereas the System F paper specifically mentions that you need a fixpoint operator to get such recursive type definitions.

Can I somehow represent a `List` that does not behave like a recursively-defined list? The only difference seems to be that the continuation for `cons` has the type `X -> R -> R` instead of `X -> List X -> R`. You're losing information about the structure of the second argument. How does that restrict you?

There's still a tight structure in `List` because the only source of R is either the first continuation `X -> R -> R` or the second argument `R`.

```haskell
let cnat = cons[Nat] in cnat 0(cnat (succ 1)(cnat 1 (nil[Nat])))
=> \c n.c(\s z.z)(c(\s z.s(s z))(c(\s z.s z) n))

consTwice = \X.\h:X.\t:List X.(\R.\c:X->R->R.\n:R. c h (c h(t [R] c n)))
=> [consTwice:forallX.X -> List X -> (forallR.(X -> R -> R) -> R -> R)]

let cnat = cons[Nat] in let ctwice = consTwice [Nat] in ctwice 0 (cnat (succ 1)(cnat 1 (nil[Nat])))
=> \c n.c(\s z.z)(c(\s z.z)(c(\s z.s(s z))(c(\s z.s z) n)))

let cnat = cons[Nat] in let ctwice = consTwice [Nat] in ctwice 0 (ctwice (succ 1)(ctwice 1 (nil[Nat])))
=> \c n.c(\s z.z)(c(\s z.z)(c(\s z.s(s z))(c(\s z.s(s z))(c(\s z.s z)(c(\s z.s z) n)))))
```

The above experiment doesn't prove anything.

I'm still not sure. What's the difference between `typo List = \X.forall R.(X->R->R)->R->R` and `typo ListR = \X.forall R.(X->ListR X->R)->R->R`? Does the latter give you any extra expressive power? If not, why did the System F introductory paper talk about needing a fixpoint type operator to get recursive types like `List a`? For example, on page 12 the author says "But term and type lambda abstractions are unnamed; the naming mechanism above is meta-notation. Recursion must be achieved indirectly."

Maybe it depends on the definition of the constructors.

* * * * *

Let's continue with my `Expr` attempt.

First, let's try without the Yoneda lemma. A constructor that tries to return `Expr Int` won't need a function of the type `Int -> a` in order to return a polymorphic type `Expr a`.

We represent the constructor `I :: forall b . Int -> (Int -> b) -> Expr b` by the continuation of type `forall b . Int -> (Int -> b) -> R`.

Next, if we find `Expr Int` as a parameter type, we basically want to be able to use the branches of `Expr` that could return an `Expr Int`. So, we represent the recursive constructor `Add :: forall b . Expr Int -> Expr Int -> (Int -> b) -> Expr b` by the continuation of type ... Not sure. Maybe just give it `R`, `Add :: forall b . R -> R -> (Int -> b) -> R`.

Maybe instead of typing it as:

```haskell
typo Expr = \B. forall R::*->*. (Nat -> (Nat -> B) -> R B) -> (Bool -> (Bool -> B) -> R B) -> (R Nat -> R Nat -> (Nat -> B) -> R B) -> R B
```

I should type it as:

```haskell
typo Expr = \B. forall R. (Nat -> (Nat -> B) -> R) -> (Bool -> (Bool -> B) -> R) -> (R -> R -> (Nat -> B) -> R) -> R
```

Worked:

```haskell
1 = succ 0
2 = succ 1
3 = succ 2

plus = \x:Nat. \y:Nat. \X. \s: X->X. \z: X. x [X] s (y [X] s z)
plus 1 1

typo Bool = forall X . X -> X -> X
true = \X t: X . \f: X . t
false = \X t: X . \f: X . f
true [Nat] 0 1
if = \X b: Bool . \tb: X . \fb: X . (b [X] tb fb)
if [Nat] true 1 0
if [Nat] false 1 0

id = \B. \x:B. x

typo Expr = \B. forall R. (Nat -> R) -> (Bool -> R) -> (R -> R -> R) -> R
EI = \B x: Nat. (\R. \fi: (Nat -> R). \fb: (Bool -> R). \fa: (R -> R -> R). fi x)
EB = \B x: Bool. (\R. \fi: (Nat -> R). \fb: (Bool -> R). \fa: (R -> R -> R). fb x)
EAdd = \B x: Expr Nat. \y: Expr Nat. (\R. \fi: (Nat -> R). \fb: (Bool -> R). \fa: (R -> R -> R). fa (x [R] fi fb fa) (y [R] fi fb fa))
=> [EAdd:forall B.Expr Nat -> Expr Nat -> (forall R.(Nat -> R) -> (Bool -> R) -> (R -> R -> R) -> R)]

test = EI [Nat] 3
=> [test:forall R.(Nat -> R) -> (Bool -> R) -> (R -> R -> R) -> R]
```

```haskell
EAdd [Nat] (EI [Nat] 1) (EI [Nat] 2) [Nat] (id [Nat]) (\x:Bool. 0) plus
=> \s z.s(s(s z))

EAdd [Nat] (EI [Nat] 1) (EB [Nat] true) [Nat] (id [Nat]) (\x:Bool. 3) plus
=> \s z.s(s(s(s z)))
```

In general, we don't need to worry about the case where we get `EAdd [Nat] (EI [Nat] 1) (EB [Nat] true)` because `EB [Nat] true [Nat]` has the type `(Nat -> Nat) -> (Bool -> Nat) -> (Nat -> Nat -> Nat) -> Nat`, which requires us to come up with `Bool -> Nat` or more generally `a -> Nat`, which people can't provide and thus we don't need to worry about.

If we try to pass in the wrong value for the Bool branch, we of course get a type error:

```haskell
test = EAdd [Nat] (EI [Nat] 1) (EB [Nat] true) [Nat] (id [Nat]) (\x:Bool. true) plus
=> type error: App: (Bool -> Nat) -> (Nat -> Nat -> Nat) -> Nat to Bool -> (forall X.X -> X -> X)

test = EB [Nat] true [Nat]
=> type error: App: (Bool -> Nat) -> (Nat -> Nat -> Nat) -> Nat to Bool -> (forall X.X -> X -> X)
```

**TODO**: Still not able to write `eval :: Expr t -> t`.

```haskell
eval = \R. \B. \x: Expr B. x [R] (\x: Nat. x) (\y: Bool. y) (\x: Nat. \y: Nat. plus x y)
=> type error: App: (Nat -> R) -> (Bool -> R) -> (R -> R -> R) -> R to Nat -> Nat
```

It would be enough if I could do something like `eval = \R. \B. \x: Expr B. typecase R of | Nat -> x [Nat] ...; | Bool -> x [Bool] ...`.

Actually, `eval :: (Expr Nat -> Nat) | (Expr Bool -> Bool)`. I need `eval [Nat] :: Expr Nat -> Nat` and `eval [Bool] :: Expr Bool -> Bool`. Look at my previous experiments with choosing a particular function based on a type and a constructor. I can do `eval [Nat] evalNat :: Expr Nat -> Nat` when I supply the dummy value `evalNat`.

## My Failed Attempts

### Other Open Questions that I was Struggling With

Now for the burning question. Can the two continuations have different return types?

```haskell
I :: Int  -> Expr Int
B :: Bool -> Expr Bool
type Expr a = (Int -> b) -> (Bool -> b) -> b

f :: Expr a -> (Int -> b) -> (Bool -> b) -> b
f x ib bb = x ib bb

-- So, the definitions of `I` and `B` should be:
I i ib bb = ib i
B b ib bb = bb b
```

But here's the catch: both branches `ib :: Int -> b` and `bb :: Bool -> b` return the same type `b`. I want them to return different types: say, `ib' :: Int -> Int` and `bb' :: Bool -> Bool`.

That means I need a deconstructing function like:

```haskell
I :: Int  -> Expr Int
B :: Bool -> Expr Bool
type Expr a = (Int -> b) -> (Bool -> c) -> (b | c)

f :: Expr a -> (Int -> b) -> (Bool -> c) -> (b | c)
f x ib bb = x ib bb

-- So, the definitions of `I` and `B` should be:
I i ib bb = ib i
B b ib bb = bb b
```

The problem is that I don't know if you can represent a type like `b | c`. `Either b c` does exist, but it is checked dynamically. I want something where `f (I 7) (+1) (not)` typechecks to `Int`, not `Either Int Bool`. That is, when you write `(f (x :: Expr Int) (ib :: Int -> Int) (bb :: Bool -> Bool)) :: Int`, the typechecker should accept it.

I notice that the type `a` is not used anywhere in the type of `f`. Can it be used to decide the return type?

### Choose a Type based on Another Type

I need an example of a type choosing between two types. So far, we saw that a value (`just 3`) chose between two types (or rather between two functions).

Wait. Isn't System F-omega just STLC on the type level? We know how to use a term to choose between two other terms (as we saw in the `Bool` and `Maybe` examples). We can lift that same idea to the type level.

The original, term-level code:

```haskell
just :: a  -> Maybe a
nothing :: () -> Maybe a
type Maybe a = (a -> b) -> b -> b

f :: Maybe a -> (a -> b) -> b -> b
f x jb nb = x jb nb

-- Now we can see what `just` and `nothing` should be.
just y jb nb = jb y
nothing y jb nb = nb
```

Now for the type-level code:

```haskell
TJust :: *  -> TMaybe *
TNothing :: TMaybe *
kind of TMaybe = (* -> *) -> * -> *

TF :: TMaybe -> (* -> *) -> * -> *
f x jb nb = x jb nb

TJust A Jb Nb = Jb A
TNothing A Jb Nb = Nb
```

(Maybe the asterisks should actually be some kind k.)

Let's experiment with it. (Link: https://crypto.stanford.edu/~blynn/lambda/typo.html)

It worked!

```haskell
typo Nat = forall X.(X->X)->X->X
=> [forall X.(X -> X) -> X -> X : *]
typo List = \X.forall R.(X->R->R)->R->R
=> [\X.forall R.(X -> R -> R) -> R -> R : *->*]

-- Fail. This should have been a kind, not a type operator ("typo").
-- typo TMaybe = \A::(* -> *) -> * -> * . A -- fail
-- => [\A::(*->*)->*->*.A : ((*->*)->*->*)->(*->*)->*->*]

typo TJust = \A::*. \J::(* -> *) .\N:: *. J A
=> [\A.\J::*->*.\N.J A : *->(*->*)->*->*]
typo TNothing = \J::(* -> *). \N::*. N
=> [\J::*->*.\N.N : (*->*)->*->*]

typo TF = \J::(* -> *) .\N::*. \X:: (* -> *) -> * -> *. X J N
=> [\J::*->*.\N.\X::(*->*)->*->*.X J N : ((*->*)->*->*)->(*->*)->*->*]

typo Foo = TF List Bool (TJust Nat)
=> [forall R.((forall X.(X -> X) -> X -> X) -> R -> R) -> R -> R : *]
typo Bar = TF List Bool TNothing
=> [forall X.(X -> X) -> X -> X : *]
```

The last part can be rewritten as:

```haskell
typo Foo = TF List Bool (TJust Nat)
=> [forall R.((forall X.(X -> X) -> X -> X) -> R -> R) -> R -> R : *]
=> [forall R.(Nat -> R -> R) -> R -> R : *]
=> [List Nat : *]

typo Bar = TF List Bool TNothing
=> [forall X.X -> X -> X : *]
=> [Bool : *]
```

In other words, when the third argument to `TF` was `TJust Nat`, it applied the first argument `List` to the inner `Nat` and returned `List Nat`. When the third argument to `TF` was `TNothing`, it just returned the second argument `Bool`.

This is completely analogous to the Haskell function `maybe :: b -> (a -> b) -> Maybe a -> b`. When the `Maybe a` is `Just x` it applies the `a -> b` function to `x`. When the `Maybe a` is `Nothing`, it just returns the `b`.

(I need a kind synonym to replace `(*->*)->*->*` with `TMaybe`.)

**Hypothesis**: The union of two type operators A of kind `ka -> *` and B of kind `kb -> *` is simply the type with kind `kunion :: (ka -> *) -> (kb -> *) -> *`. The constructor for A has the kind `ka -> kunion` and the constructor for B has the kind `kb -> kunion`. The pattern-matching operator for the union has the kind `ka -> kb -> kunion -> *`.

**Hypothesis**: The product of two type operators A of kind `ka -> *` and B of kind `kb -> *` is the type with kind `kproduct :: (ka -> kb -> *) -> *`. The constructor is of the kind `ka -> kb -> kproduct` and the pattern-matching operator of the kind `(ka -> kb -> *) -> kproduct -> *`.

```haskell
typo TProduct = \A::*. \B::*. \F::(* -> * -> *). F A B
=> [\A.\B.\F::*->*->*.F A B : *->*->(*->*->*)->*]
typo TDestructProduct = \F:: (* -> * -> *). \P::((* -> * -> *) -> *). P F
=> [\F::*->*->*.\P::(*->*->*)->*.P F : (*->*->*)->((*->*->*)->*)->*]
typo TFst = \A::*. \B::*. A
=> [\A.\B.A : *->*->*]

typo Pair = TProduct Nat Bool

=> [\F::*->*->*.F(forallX.(X -> X) -> X -> X)(forallX.X -> X -> X) : (*->*->*)->*]
typo Fst = TDestructProduct TFst Pair
=> [forallX.(X -> X) -> X -> X : *]
```

### Type-level Attempts

I need a type-lambda that will produce the type `b` if the input is `Int` and `c` if the input is `Bool`. Is that possible? Do you have the necessary equality constraints on types?

Let's translate that type-level problem to a term-level problem in STLC. I want the output to be 3 if the input is 0 and 4 if the input is 1. How do you test whether the input is equal to 3? Well, you can implement an equality test for Church numerals.

Here's how far I got with `eval`:

```haskell
typo ExprNat = \N::*. \B::*. N
typo ExprBool = \N::*. \B::*. B
typo Expr = \X::(* -> * -> *). \FN::*. \FB::*. X FN FB
typo Foo = Expr ExprNat Nat Bool
=> [forall X.(X -> X) -> X -> X : *] (i.e., Nat)

-- Not sure how to pattern-match on the type of x in the body.
eval = \X::(* -> * -> *). \x: Expr X Nat Bool. 0
eval [ExprNat] 1
```

Hmm... Maybe I should **pass in** a function that gives some value of type X.

Stopped at:

```haskell
eval = \X::(* -> * -> *). \x: Expr X Nat Bool. \f: Expr X (Nat -> Nat) (Bool -> Bool). f x
=> type error: App: Expr X(Nat -> Nat)(Bool -> Bool) to Expr X Nat Bool

eval = \X::(* -> * -> *). \x: Expr X Nat Bool. \f: Expr X (Nat -> Nat) (Bool -> Bool). f [X] x
=> type error: TApp X(Nat -> Nat)(Bool -> Bool)

typo test = Expr ExprNat (Nat -> Nat) Bool
=> [(forall X.(X -> X) -> X -> X) -> (forall X.(X -> X) -> X -> X) : *]

evalNat = \x: Expr ExprNat Nat Bool. \f: Expr ExprNat (Nat -> Nat) (Bool -> Bool). f x
=> [evalNat:Expr ExprNat Nat Bool -> Expr ExprNat(Nat -> Nat)(Bool -> Bool) -> Nat]

evalNat 0 succ
=> \s z.s z (i.e., 1)

Current session:

typo Bool = forall X . X -> X -> X
true = \X t: X . \f: X . t
false = \X t: X . \f: X . f
true [Nat] 0 1
if = \X b: Bool . \tb: X . \fb: X . (b [X] tb fb)
if [Nat] true 1 0
if [Nat] false 1 0
typo ExprNat = \N::*. \B::*. N
typo ExprBool = \N::*. \B::*. B
typo Expr = \X::(* -> * -> *). \FN::*. \FB::*. X FN FB
eval = \X::(* -> * -> *). \x: Expr X Nat Bool. \f: Expr X (Nat -> Nat) (Bool -> Bool). (f [X]) x
typo test = Expr ExprNat (Nat -> Nat) Bool
evalNat = \x: Expr ExprNat Nat Bool. \f: Expr ExprNat (Nat -> Nat) (Bool -> Bool). f x
evalNat 0 succ
eval = \X::(* -> * -> *). \x: Expr X Nat Bool. \f: Expr X (Nat -> Nat) (Bool -> Bool). f [X (Nat -> Nat) (Bool -> Bool)] x
```

I don't even know if you can choose the function to be executed based on a type. We want `foo [Int] 3 :: Int` and `foo [Bool] true :: Bool`.

```haskell
foo :: (Int | Bool) -> (Int | Bool)
-- Or maybe
foo :: (Int -> Int) | (Bool -> Bool)
```

How would you instantiate such a type? Don't know.

Here are sum types.

```haskell
typo TSum = \FA::*. \FB::*. \X::(* -> * -> *). X FA FB
typo test = TSum Nat Bool
typo First = \FA::*. \FB::*. FA
typo Second = \FA::*. \FB::*. FB
typo test = TSum Nat Bool First
```

I want `foo :: TSum (Nat -> Nat) (Bool -> Bool)`. But that's not possible, because it has kind `* -> *` whereas a function type has to be of kind `*`. What if you do `foo :: \X::* -> * -> *. TSum (Nat -> Nat) (Bool -> Bool) X`?

I think that on top of the type being right, the constructor has to be such that it picks out the correct branch of the function.

Got an extremely verbose GADT working:

```haskell
sumConstructors = \A. \OA. \B. \OB. \fa: A -> OA. \fb: B -> OB. \X::*->*->*. \x: (A -> OA) -> (B -> OB) -> TSum (A -> OA) (B -> OB) X. x fa fb

addTwoOrNegate = sumConstructors [Nat] [Nat] [Bool] [Bool] addTwo negate
addTwoOrNegateFirst = (\fa:Nat->Nat. \fb:Bool->Bool. fa)
addTwoOrNegateSecond = (\fa:Nat->Nat. \fb:Bool->Bool. fb)

addTwoOrNegate [First] addTwoOrNegateFirst 0
addTwoOrNegate [Second] addTwoOrNegateSecond true
```

Actually, the definition of `addTwoOrNegate` is not all that bad. It's basically what you would have to define in a conditional-type function definition. For example, `function process<T extends string | null>(text: T): T extends string ? string : null {}`.

The two constructors are:

```haskell
[addTwoOrNegateFirst:(Nat -> Nat) -> (Bool -> Bool) -> Nat -> Nat]
[addTwoOrNegateSecond:(Nat -> Nat) -> (Bool -> Bool) -> Bool -> Bool]
```

The type of `addTwoOrNegate` is basically:

```haskell
[addTwoOrNegate:forall X::*->*->*.((Nat -> Nat) -> (Bool -> Bool) -> TSum(Nat -> Nat)(Bool -> Bool) X) -> TSum(Nat -> Nat)(Bool -> Bool) X]

-- In other words:
[addTwoOrNegate: (TAddTwoOrNegate (Nat -> Nat | Bool -> Bool)) -> (Nat -> Nat | Bool -> Bool)]

-- Or (very roughly):
[addTwoOrNegate: Expr t -> t]
```

So, what the caller needs to know is its position type in the GADT (`First`) and its destructor function in the GADT (`\fa:Nat->Nat. \fb:Bool->Bool. fa`). Note that using the polymorphic `first :: forall a . a -> a -> a` didn't work as the destructor function. It had to be specific. That sucks. I guess you have to create such a destructor for every single sum-function you create. I guess you could add some syntactic sugar to define that destructor along with the function itself.

Let's try with a union of base types instead of a union of function types like above.

Question: How will you have multiple variants? You need union to be closed. That is, the type of `A | B` must be the same as the type of `A` or the type of `A | B | C`.

You need two things from a union: the type destructor to get back the original type (so that you can return a specific type like `Int` instead of `Int | Bool`) and the term destructor to get back the original value.

Can we somehow get that destructor using the type operator `First`? I want a type-indexed term. That's what a GADT is.

### Constructors with Different Return Types

We've seen that an ADT is just a function with a continuation for each variant. A constructor just returns a function that takes an ADT and sends in the constructor's arguments to the corresponding continuation.

This formula worked for ADTs, where the return type was the same for all variants. For example, the ADT function for Maybe had the type `(a -> b) -> b -> b`, where the `a -> b` continuation was for `Just a` and `b` "continuation" was for Nothing, but they both had the same return type `b`.

Let's test our understanding by trying to implement `eval` for the GADT `Expr a`:

```haskell
-- Smart constructors.
I :: Int  -> Expr Int
B :: Bool -> Expr Bool

-- GADT.
type Expr a = (Int -> b) -> (Bool -> b) -> b

-- Constructors.
I i fi fb = fi i
B b fi fb = fb b

evalInt i = i
evalBool b = b
-- This is where we hit a snag. (Actually, when I tried it below, I didn't hit a snag.)
eval e = e evalInt evalBool
```

What happened when I actually tried it?

```haskell
makeI i fi fb = fi i
makeB b fi fb = fb b

evalInt i = i
evalBool b = b
eval e = e evalInt evalBool
```

```haskell
Prelude> :t eval
eval :: ((p1 -> p1) -> (p2 -> p2) -> t) -> t
Prelude> eval (makeI 3)
3
Prelude> eval (makeB True)
True
Prelude> :t eval
eval :: ((p1 -> p1) -> (p2 -> p2) -> t) -> t
```

It seems to work. Something must be wrong. Maybe it's the fact that I haven't used a recursive call anywhere.

Let's try to implement the original motivating problem for GADTs.

```haskell
makeI :: Int  -> Expr Int
makeB :: Bool -> Expr Bool
makeAdd :: Expr Int -> Expr Int -> Expr Int

eval :: Expr t -> t
eval (I v) = v
eval (B v) = v
eval (Add x y) = (eval x) + (eval y)

-- Test cases that should fail.
-- add3 (i3 10) (b3 True)

-- Test cases that should pass
-- (makeI 12) :: Expr3 Int (Not sure what this means in the above system.)
-- (eval (makeAdd (makeI 3) (makeI 4))) :: Int (not something else)
```

```haskell
Prelude> :t makeAdd [1, 2] "yo"
makeAdd [1, 2] "yo"
  :: Num a => p1 -> p2 -> ([a] -> [Char] -> t3) -> t3

-- This is obviously bad.

Prelude> :t makeI 10
makeI 10 :: Num t1 => (t1 -> t2) -> p1 -> p2 -> t2

Prelude> :t makeAdd (makeI 3) (makeI 4)
makeAdd (makeI 3) (makeI 4)
  :: (Num t4, Num t5) =>
     p4
     -> p5
     -> (((t4 -> t6) -> p6 -> p7 -> t6)
         -> ((t5 -> t7) -> p8 -> p9 -> t7) -> t3)
     -> t3

-- Utter fail!
```

Note that I'm not even able to express the recursive type `Expr a`

```haskell
Prelude> type Expr a = (Int -> a) -> (Bool -> a) -> (Expr Int -> Expr Int -> a) -> a

<interactive>:49:1: error:
    Cycle in type synonym declarations:
      <interactive>:49:1-75: type Expr a =
                                 (Int -> a) -> (Bool -> a) -> (Expr Int -> Expr Int -> a) -> a
```

Current session:

```haskell
typo Expr = \A. forall R::*. (Nat -> A) -> (Bool -> A) -> (R -> R -> A) -> A
typo test = Expr Nat
makeI = \A. \x:Nat. \R. \fi: Nat->A. \fb: Bool -> A. \fa: R -> R -> A. fi x
makeB = \A. \x:Bool.\fi: Nat->A. \fb: Bool -> A. \fa: Expr Nat -> Expr Nat -> A. fb x
typo test = forall A . Expr Nat -> A
foo = \x: Expr Nat. x [Expr Nat] succ (\b: Bool. 0) (\e1: Expr Nat. \e2: Expr Nat. 1)
\e1: Expr Nat. \e2: Expr Nat. 1
foo (makeI [Nat] 1)
makeAdd = \A. \R. \a: R. \b: R.\fi: Nat->A. \fb: Bool -> A. \fa: R -> R -> A. fa a b

evalI = \i: Nat. i
evalB = \b: Bool. b
evalAdd = \R. \e1 e2: R. 1
```

### Church Encoding vs Scott Encoding without Recursive Types

Let's compare their properties. First, the Church encoding of a list:

```haskell
Prelude> nilC c n = n
Prelude> :t nilC
nilC :: p1 -> p2 -> p2
Prelude> consC h t c n = c h (t c n)
Prelude> :t consC
consC
  :: t1 -> ((t1 -> t2 -> t3) -> t4 -> t2) -> (t1 -> t2 -> t3) -> t4 -> t3
Prelude> xs1 = (consC 1 nilC)
Prelude> :t xs1
xs1 :: Num t1 => (t1 -> t2 -> t3) -> t2 -> t3
Prelude> xs2 = consC 2 (xs1)
Prelude> :t xs2
xs2 :: Num t1 => (t1 -> t2 -> t2) -> t2 -> t2
Prelude> xs3 = consC 3 xs2
Prelude> :t xs3
xs3 :: Num t1 => (t1 -> t2 -> t2) -> t2 -> t2
Prelude> xs3 (+) 2
8
```

Naive Scott encoding (in a language without recursive types):

```haskell
Prelude> nilS c n = n
Prelude> consS h t c n = c h t
Prelude> :t consS
consS :: t1 -> t2 -> (t1 -> t2 -> t3) -> p -> t3
Prelude> :t nilS
nilS :: p1 -> p2 -> p2
Prelude> :t consS 1 nilS
consS 1 nilS :: Num t1 => (t1 -> (p1 -> p2 -> p2) -> t3) -> p -> t3
Prelude> :t consS 2 (consS 1 nilS)
consS 2 (consS 1 nilS)
  :: (Num t2, Num t4) =>
     (t2 -> ((t4 -> (p1 -> p2 -> p2) -> t5) -> p4 -> t5) -> t6)
     -> p5 -> t6
Prelude> :t consS 3 (consS 2 (consS 1 nilS))
consS 3 (consS 2 (consS 1 nilS))
  :: (Num t2, Num t4, Num t5) =>
     (t2
      -> ((t4 -> ((t5 -> (p1 -> p2 -> p2) -> t6) -> p4 -> t6) -> t7)
          -> p5 -> t7)
      -> t8)
     -> p6 -> t8

Prelude> nilS undefined 0
0
Prelude> consS 1 nilS (\a1 fa1 -> a1 + fa1 undefined 0) undefined
1
Prelude> consS 2 (consS 1 nilS) (\a2 fa2 -> a2 + fa2 (\a1 fa1 -> a1 + fa1 undefined 0) undefined) undefined
3
```

Notice how the unpacking function keeps getting more and more convoluted for longer lists. You have to specify what to do with each element `a2`, `a1`, and the base value. With Church encoding, you had the same simple function used at all levels `(+)`.

This means I could have done `a2 - (a1 + a0)` instead of `a2 + (a1 + a0)`:

```haskell
Prelude> consS 2 (consS 1 nilS) (\a2 fa2 -> a2 - fa2 (\a1 fa1 -> a1 + fa1 undefined 0) undefined) undefined
1
```

The consequence of such a handwritten, element-by-element function is that there is no "typechecking". You can have elements of arbitrary types:

```haskell
Prelude> xs = consS "two" (consS 1 nilS)
Prelude> xs (\a2 fa2 -> a2 ++ " (this is so wrong) " ++ show (fa2 (\a1 fa1 -> a1 + fa1 undefined 0) undefined)) undefined
"two (this is so wrong) 1"
```

Contrast that to the Church encoded list:

```haskell
Prelude> xs = consC "two" (consC 1 nilC)

<interactive>:48:25: error:
    + No instance for (Num [Char]) arising from the literal '1'
    + In the first argument of 'consC', namely '1'
      In the second argument of 'consC', namely '(consC 1 nilC)'
      In the expression: consC "two" (consC 1 nilC)
```

So, unless you have recursive types in your language, the arguments that are supposed to be of the recursive types (like the second argument of `consS`, which is supposed to be a list) may be given a value of any arbitrary type in the continuation.

The [blog post](https://kseo.github.io/posts/2016-12-13-scott-encoding.html) describing Scott encoding works only because it is written in Haskell, which allows recursive type definitions like:

```haskell
newtype ListS a =
   ListS {
     unconsS :: forall r. (a -> ListS a -> r) -> r -> r
   }
```

This is very different from our naive Scott encoding above, in which the "list" cannot be written in a form like above because its type changes based on its length and the different types of its elements.

* * * * *

Resources:

+ Blog post - [Scott encoding of Algebraic Data Types](https://kseo.github.io/posts/2016-12-13-scott-encoding.html)

+ Paper that introduced Scott encoding - [Comprehensive Encoding of Data Types and Algorithms in the Lambda-Calculus (Functional Pearl)](http://www.nlda-tw.nl/janmartin/papers/jmjansenLambdapaper.pdf)

## Algebraic Data Types (ADTs)

+ I've already got product types in the form of records.

+ How to get sum types? I need something for deconstructing a union like `True | False`. I could encode the names as string literals and thus types.

+ Polymorphic data types

One option is to go for full System F-omega so that you can type operators like `List`. Another option is to use `|` to have unions of different types, such as `{a: Bool} | {c: Int}`. How will you type-match them, though? You could use the entire type as such.

TypeScript uses a special field called `kind` to represent the name of the variant in an ADT:

```typescript
function area(s: Shape) {
    switch (s.kind) {
        case "square": return s.size * s.size;
        case "rectangle": return s.height * s.width;
        case "circle": return Math.PI * s.radius ** 2;
    }
}
```

## Type-level Programming

+ Peano arithmetic: Logically, I know that `Add (Succ n) m -> Succ (Add n m)`. How to express that?

+ Sized vector: Can you implement a sized vector type? What kind of errors can it catch at compile time? Just empty lists when a non empty one was expected? Can it ask for anything more specific, like... Oh, two lists of the same size? Then one list with size greater than the other? Look at the prelude for ideas about size constraints.

+ Other type-level computations, such as a mini-interpreter.

# Functional Programming Feature Wish List

+ Union type: `pet: Bird | Fish` - allow `pet.numEggs()` but not `pet.numFins()`.

+ Generic type with syntax.

+ Nested generic types. Tuples - basically Pair<A,B>. Maybe you can get this using Container<{a: A, b: B}>.

+ Strings as keys for records.

+ in operator: `function move(pet: Fish | Bird) {if ("swim" in pet) {return pet.swim();} return pet.fly();}`

+ generic types: `type Container<T> = { value: T };`

+ Lists! Maybe use strings as indices - ix0, ix1, etc.

+ Prove soundness.

+ What would it take to have higher-kinded types? (Answer: System F-omega)

+ Type class

+ Try implementing functors. Can you prove the functor laws?

+ Intersection type: `Person & Loggable` (has a log function field). Optional.

+ self-referential types: `type LinkedList<T> = T & { next: LinkedList<T> };`
