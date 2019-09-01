
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
