
# TypeScript Feature Wish list

## Usability

+ Have a flag for toggling the type of type system.

## Organize the typechecker like a theorem-prover

Make the logical inference explicit.

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

## System F

+ System F requires this: "if a function has a polymorphic type then type applications must be explicitly indicated." (Theorems for Free paper)

I guess I'm currently *inferring* the types. That's why I had to write a `unify` function to solve the constraints.

# System F-omega

Don't know if I need this.

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
