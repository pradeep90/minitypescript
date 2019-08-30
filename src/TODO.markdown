# Functional Programming Feature Wish List

+ Generic type with syntax. What would its subtyping relation look like? Foo a === record with a in the right places.

+ Nested generic types. Tuples - basically Pair<A,B>. Maybe you can get this using Container<{a: A, b: B}>.

+ Strings as keys for records.

+ Lists! Maybe use strings as indices - ix0, ix1, etc.

+ Prove soundness.

+ What would it take to have higher-kinded types?

+ After conditional types are done, show that `f . g $ x` works by showing that the type of `g string` is `string`, not `string | null`.

+ Type class?

+ Try implementing functors.

+ Maybe.

+ Either.

+ What would it take to do type level computation, such as evaluation? Get an example.

+ Other features: higher rank types - for all a, a -> string.

# Typescript Feature Wish list

+ Intersection type: `Person & Loggable` (has a log function field). Optional.

+ Union type: `pet: Bird | Fish; pet.numEggs() but not pet.numFins()`

+ self-referential types: `type LinkedList<T> = T & { next: LinkedList<T> };`

+ exhaustiveness checking for ADTs: `match x with | Nil => 1 | Cons a b => 2;;`

+ Field names as constant types

+ in operator: `function move(pet: Fish | Bird) {if ("swim" in pet) {return pet.swim();} return pet.fly();}`

+ generic types: `type Container<T> = { value: T };`

+ index type query and index access operators: `function pluck<T, K extends keyof T>(o: T, propertyNames: K[]): T[K][] {return propertyNames.map(n => o[n]);}`

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

Source: Haskell Design Patterns by Ryan Lemmer

GADTs let you treat the result of `eval (Add ...)`  as the integer it is instead of treating it like some abstract type `t`.

Wow. To infer that, my typechecker would have to store the type of the function as `(Expr Int -> Expr Int -> Int) | (Bool -> Bool)`. Once the argument type is known to be `Expr Int`, it can infer that the return type must be `Expr Int -> Int`.

Hmm... So, the type `Int -> Bool` is really the logical implication `input type is Int => return type will be Bool`. I guess simple types lead to simple logical implications whereas things like GADTs lead to more complicated, multi-branch implications, like `input type is Expr Int => return type will be Int, but if input type is Bool => return type will be Bool`. The types are there to help the typechecker logically infer the types and thus check them against the provided types. You want to infer as much as possible and that's why you need really powerful type systems. (Curry-Howard correspondence, anybody?)

When you want three functions with the same name and overall input type but different return types, make their type signature `A1 -> B1 | A2 -> B2 | A3 -> B3` so that it's easier to typecheck. (I guess the problem for Haskell would have been in inferring, not in typechecking.)

I guess `match foo with | A1 -> ... | A2 -> ... | A3 -> ...` indexes into the above type union.

I'm also guessing that you can use the same technique to implement algebraic data types. Except there, the index for the union won't be the type of the function's first parameter, it will be the name of the constructor, which is conveniently a string and a type in TypeScript.

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
