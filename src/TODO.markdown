# Functional Programming Feature Wish List

+ Add a type declaration statement. Allow the name to be referenced.

+ Generic type with syntax. What would its subtyping relation look like? Foo a === record with a in the right places.

+ Nested generic types. Tuples - basically Pair<a,b>.

+ What would it take to have higher-kinded types?

+ After conditional types are done, show that `f . g $ x` works by showing that the type of `g string` is `string`, not `string | null`.

+ Type class?

+ Try implementing functors.

+ Maybe.

+ Either.

+ What would it take to do type level computation, such as evaluation? Get an example.

+ Other features: higher rank types - for all a, a -> string.

# Typescript Feature Wish list

+ Intersection type: `Person & Loggable` (has a log function field).

+ Union type: `pet: Bird | Fish; pet.numEggs() but not pet.numFins()`

+ self-referential types: `type LinkedList<T> = T & { next: LinkedList<T> };`

+ exhaustiveness checking for ADTs: `match x with | Nil => 1 | Cons a b => 2;;`

+ Field names as constant types

+ in operator: `function move(pet: Fish | Bird) {if ("swim" in pet) {return pet.swim();} return pet.fly();}`

+ generic types: `type Container<T> = { value: T };`

+ index type query and index access operators: `function pluck<T, K extends keyof T>(o: T, propertyNames: K[]): T[K][] {return propertyNames.map(n => o[n]);}`

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
