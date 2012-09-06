% Typeclasses
% John Chee
% MyLife

Literate Haskell
-

The source for this presentation can be parsed, type checked and run by GHC.

- The name of this module is `TypeClasses`
- We want the typeclass definition of `Monoid` and common implementations

\begin{code}
module TypeClasses where

import Data.Monoid
\end{code}

Outline
-

- Polymorphism
    - Parametric Polymorphism
    - What are typeclasses? Ad-hoc polymorphism.
- Typeclasses to the rescue
    - `Num` class
    - `Num Int` instance
    - Natural numbers
    - `Num Nat` instance
    - A well defined `Num Nat` instance
    - Back to `square`
- Even more typeclasses
    - The three ways to define data types in Haskell
    - `Monoid` class
    - `Monoid NatSum` instance
    - `Monoid NatProduct` instance
    - A better way to do things
    - `Sum` and `Product` instances
    - `Sum Nat` is a valid `Monoid`
    - `Enum`erable types
    - `Enum Nat` instance
    - `Enum Sum a` instance
    - Back to `length`

Polymorphism
=

Parametric Polymorphism
-

- Hindley-Milner type inference gets us this far

< length :: [a] -> Int
< length [] = 0
< length (_ : xs) = 1 + length xs

What are typeclasses? Ad-hoc polymorphism.
-

- In object oriented programming terminology: Method overloading
- In functional programming terminology: Function overloading

< square :: Numeric -> Numeric
< square x = x * x

- But `Numeric` is not a type!
- We could write:

< squareInt :: Int -> Int
< squareInt x = x `timesInt` x
<
< squareDouble :: Double -> Double
< squareDouble x = x `timesDouble` x

- We want `Numeric` to be the collection of types that implement `(*)`

< (*) :: Numeric -> Numeric -> Numeric
< (*) x y = -- To implement we need to know the type and representation of x and y

Typeclasses to the rescue
=

`Num` class
-

< class Num a where
<     (+)                 :: a -> a -> a
<     (-)                 :: a -> a -> a
<     (*)                 :: a -> a -> a
<     negate              :: a -> a
<     abs                 :: a -> a
<     -- | Sign of a number.
<     -- The functions 'abs' and 'signum' should satisfy the law:
<     --
<     -- > abs x * signum x == x
<     signum              :: a -> a
<     fromInteger         :: Integer -> a
<
<     x - y               = x + negate y
<     negate x            = 0 - x

- From the documentation: "Minimal complete definition: all except `negate` or `(-)`"
- `(-)` and `negate` have default implementations

Instances:

< Num Int
< Num Double
< Integral a => Num (Ratio a)

`Num` class features
-

- The `Num` class also gives us overloaded numeral parsing
    - `10`, `0` or `42` in Haskell source is translated to `fromInteger 10`, `fromInteger 0` or `fromInteger 42` respectively

< *TypeClasses> :t 0
< 0 :: Num a => a

`Num Int` instance
-

< class Num a where
<     (+)                 :: a -> a -> a
<     (-)                 :: a -> a -> a
<     (*)                 :: a -> a -> a
<     negate              :: a -> a
<     abs                 :: a -> a
<     signum              :: a -> a
<     fromInteger         :: Integer -> a

Specialized code for 64 bit `Int`s

< instance Num Int where
<     (I64# x#) + (I64# y#)  = I64# (x# `plusInt64#`  y#)
<     (I64# x#) - (I64# y#)  = I64# (x# `minusInt64#` y#)
<     (I64# x#) * (I64# y#)  = I64# (x# `timesInt64#` y#)
<     negate (I64# x#)       = I64# (negateInt64# x#)
<     abs x | x >= 0         = x
<           | otherwise      = negate x
<     signum x | x > 0       = 1
<     signum 0               = 0
<     signum _               = -1
<     fromInteger i          = I64# (integerToInt64 i)

Natural numbers
-

Let's define a natural number type

<!---
data Nat :: * where
  Z :: Nat
  S :: Nat -> Nat
--->

\begin{code}
data Nat = Z | S Nat
  deriving (Show)
\end{code}

- We'll derive `Show` for debugging purposes

`Num Nat` instance
-

< class Num a where
<     (+)                 :: a -> a -> a
<     (-)                 :: a -> a -> a
<     (*)                 :: a -> a -> a
<     abs                 :: a -> a
<     signum              :: a -> a
<     fromInteger         :: Integer -> a

\begin{code}
instance Num Nat where
  Z + x = x
  S x + y = x + S y

  Z - x = Z
  x - Z = x
  S x - S y = x - y

  Z * x = Z
  S x * y = y + (x * y)

  abs n = n

  signum Z = Z
  signum (S _) = S Z

  fromInteger n =
    if n <= 0 then Z else S (fromInteger (n - 1))
\end{code}

A well defined `Num Nat` instance
-

From the documentation of `Nat`:

The functions 'abs' and 'signum' should satisfy the law:

< abs x * signum x == x

By induction on `x`

< abs Z * signum Z = Z
< abs Z * signum
< = Z * Z {- definition of abs and signum -}
< = Z     {- definition of (*) -}

< Given that abs n * signum n = n, abs (S n) * signum (S n) = S n
< abs (S n) * signum (S n)
< = (S n) * (S Z)     {- definition of abs and signum -}
< = (S Z) + (n * S Z) {- definition of (*) -}
< = S Z + n           {- S Z is the identity of (*) -}
< = Z + S n           {- definition of (+) -}
< = S n               {- definition of (+) -}

Back to `square`
-

Now we can implement `square`

Read the type signature as "Given that `n` is a member of the typeclass `Num`, `square` has type `n` to `n`"

\begin{code}
square :: (Num n) => n -> n
square x = x * x
\end{code}

< *TypeClasses> import Data.Ratio
< *TypeClasses Data.Ratio> square 42
< 1764
< *TypeClasses Data.Ratio> square 42.5
< 1806.25
< *TypeClasses Data.Ratio> square (89 % 2)
< 7921 % 4
< *TypeClasses Data.Ratio> square 3 :: Nat
< S (S (S (S (S (S (S (S (S Z))))))))

- Typeclasses allow us to implement one function for a range of types
    - Given the typeclass instances for those types

<center>![Thumper smiling](./thumper.png)</center>

Even more typeclasses
=

The three ways to define data types in Haskell
-

< data Foo = Bar | Baz
<
< type Strings = [String]
< type NonEmptyMaybeList a = ([Maybe a], a, [Maybe a])
<
< newtype Miles = Miles Int
< newtype Kilometers = Kilometers Int

- `data`
    - The default way of doing things
- `type`
    - Simple aliasing
    - Restricted to aliasing (no sum types)
    - Same performance as aliased type
- `newtype`
    - Distinct types
        - Unlike `type` definitions
    - Restricted to aliasing (no sum types)
    - Same performance as aliased type (constructors erased at compile time)

`Monoid` class
-

Another useful typeclass

< class Monoid a where
<         mempty  :: a
<         mappend :: a -> a -> a
<         mconcat :: [a] -> a
<
<         mconcat = foldr mappend mempty

- `mconcat` has a default implementation

From the documentation:

- The class of monoids (types with an associative binary operation that has an identity).
- Instances should satisfy the following laws:

< mappend mempty x = x
< mappend x mempty = x
< mappend x (mappend y z) = mappend (mappend x y) z
< mconcat = foldr mappend mempty

`Monoid NatSum` instance
-

< class Monoid a where
<         mempty  :: a
<         mappend :: a -> a -> a

<!---
data NatSum :: * where
  Sum :: Nat -> NatSum
--->

\begin{code}
newtype NatSum = NatSum Nat
  deriving (Show)

instance Monoid NatSum where
  mempty = NatSum Z
  mappend (NatSum x) (NatSum y) = NatSum (x + y)
\end{code}

- There may be another `Monoid` instance of `Nat`
- So we wrap this instance to signify that it's for 'summing'

`Monoid NatProduct` instance
-

< class Monoid a where
<         mempty  :: a
<         mappend :: a -> a -> a

<!---
data NatProduct :: * where
  Product :: Nat -> NatProduct
--->

\begin{code}
newtype NatProduct = NatProduct Nat

instance Monoid NatProduct where
  mempty = NatProduct (S Z)
  mappend (NatProduct x) (NatProduct y) = NatProduct (x * y)
\end{code}

- Another potential `Monoid` instance of `Nat`
- I'm still working on the associativity proof
    - Don't use this code in production

A better way to do things
-

It's common that a `Num` type has multiple valid `Monoid` instances

In `Data.Monoid`:

< -- | Monoid under addition.
< newtype Sum a = Sum { getSum :: a }
<         deriving (Eq, Ord, Read, Show, Bounded)
<
< instance Num a => Monoid (Sum a) where
<         mempty = Sum 0
<         Sum x `mappend` Sum y = Sum (x + y)
<
< -- | Monoid under multiplication.
< newtype Product a = Product { getProduct :: a }
<         deriving (Eq, Ord, Read, Show, Bounded)
<
< instance Num a => Monoid (Product a) where
<         mempty = Product 1
<         Product x `mappend` Product y = Product (x * y)

`Sum` and `Product` instances
-

Instances:

< Bounded a => Bounded (Sum a)
< Eq a => Eq (Sum a)
< Ord a => Ord (Sum a)
< Read a => Read (Sum a)
< Show a => Show (Sum a)
< Num a => Monoid (Sum a)
<
< Bounded a => Bounded (Product a)
< Eq a => Eq (Product a)
< Ord a => Ord (Product a)
< Read a => Read (Product a)
< Show a => Show (Product a)
< Num a => Monoid (Product a)

- For this presentation we'll only be using `Sum`
- For our own definitions of `Num` and thus `Sum`, we need to prove:

< mappend mempty x = x
< mappend x mempty = x
< mappend x (mappend y z) = mappend (mappend x y) z
< mconcat = foldr mappend mempty

`Sum Nat` is a valid `Monoid`
-

< mappend mempty x = x
<
< mappend mempty x
< = mappend (Sum Z) (Sum x') {- definition of mempty, x = Sum x', fromInteger 0 = Z -}
< = Sum (Z + x')             {- definition of mappend -}
< = Sum x'                   {- definition of (+) -}
< = x                        {- x = (Sum x') -}
<
< mappend x mempty = x
<
< mappend x mempty
< = mappend (Sum x') (Sum Z) {- definition of mempty, x = Sum x', fromInteger 0 = Z -}
< = Sum (x' + Z)             {- definition of mappend -}
< = Sum x'                   {- Z is the identity of (+) -}
< = x                        {- x = (Sum x') -}

< mappend x (mappend y z) = mappend (mappend x y) z
<
< mappend x (mappend y z)
< = mappend (Sum x') (mappend (Sum y') (Sum z')) {- x = Sum x', y = Sum y', z = Sum z' -}
< = mappend (Sum x') (Sum (y' + z'))             {- definition of mappend -}
< = Sum (x' + (y' + z'))                         {- definition of mappend -}
< = Sum ((x' + y') + z')                         {- (+) is associative -}
< = mappend (Sum (x' + y')) (Sum z')             {- definition of mappend -}
< = mappend (mappend (Sum x') (Sum y')) (Sum z') {- definition of mappend -}
< = mappend (mappend x y) z                      {- x = Sum x', y = Sum y', z = Sum z' -}

< mconcat = foldr mappend mempty {- definition of mconcat -}

`Enum`erable types
-

< -- | Class 'Enum' defines operations on sequentially ordered types.
< class Enum a where
<     succ                :: a -> a
<     pred                :: a -> a
<
<     toEnum              :: Int -> a
<     fromEnum            :: a -> Int
<
<     enumFrom            :: a -> [a]
<     enumFromThen        :: a -> a -> [a]
<     enumFromTo          :: a -> a -> [a]
<     enumFromThenTo      :: a -> a -> a -> [a]
<
<     succ                   = toEnum . (`plusInt` oneInt)  . fromEnum
<     pred                   = toEnum . (`minusInt` oneInt) . fromEnum
<     enumFrom x             = map toEnum [fromEnum x ..]
<     enumFromThen x y       = map toEnum [fromEnum x, fromEnum y ..]
<     enumFromTo x y         = map toEnum [fromEnum x .. fromEnum y]
<     enumFromThenTo x1 x2 y = map toEnum [fromEnum x1, fromEnum x2 .. fromEnum y]

Instances:

< Enum Int
< Enum Double
< Integral a => Enum (Ratio a)

Not from the documentation, but on inspection of the source code we can see that a minimal complete definition of `Enum` is `toEnum` and `fromEnum`.

`Enum Nat` instance
-

< class Enum a where
<     toEnum              :: Int -> a
<     fromEnum            :: a -> Int

\begin{code}
instance Enum Nat where
  toEnum n       = if n <= 0 then Z else S (toEnum (n - 1))
  fromEnum Z     = 0
  fromEnum (S n) = 1 + fromEnum n
  succ n         = S n
  pred Z         = Z
  pred (S n)     = n
\end{code}

`Enum Sum a` instance
-

< class Enum a where
<     toEnum              :: Int -> a
<     fromEnum            :: a -> Int

- I'm not sure why this isn't already implemented in `Data.Monoid`

\begin{code}
instance (Enum a) => Enum (Sum a) where
  toEnum n                = Sum (toEnum n)
  fromEnum (Sum n)        = fromEnum n
\end{code}

Back to `length`
-

Given that `peano` is a member of the typeclasses `Monoid` and `Enum`, `peanoLength` has type list of `a` to `peano`

\begin{code}
peanoLength :: (Monoid peano, Enum peano) => [a] -> peano
peanoLength [] = mempty
peanoLength (x : xs) = succ (peanoLength xs)
\end{code}

< *TypeClasses> import Data.Ratio
< *TypeClasses Data.Ratio> peanoLength [(), (), ()] :: Sum Int
< Sum {getSum = 3}
< *TypeClasses Data.Ratio> peanoLength [(), (), ()] :: Sum Double
< Sum {getSum = 3.0}
< *TypeClasses Data.Ratio> peanoLength [(), (), ()] :: Sum Rational
< Sum {getSum = 3 % 1}
< *TypeClasses Data.Ratio> peanoLength [(), (), ()] :: Sum Nat
< Sum {getSum = S (S (S Z))}

The end
-

- [How to make ad-hoc polymorphism less ad hoc](http://homepages.inf.ed.ac.uk/wadler/papers/class/class.ps)
    - If you are interested in the implementation of typeclasses
- [Typeclassopedia](http://www.haskell.org/haskellwiki/Typeclassopedia)
    - The best exposition of Haskell's typeclasses
- Bruce Ricard (MyLife) helped with the proofs

`Z` is the identity of `(+)`
-

< Lemma Z is the identity of (+)
< for n :: Nat. n + Z = Z + n = n

By induction on `n`

< Z + Z = Z + Z = Z
< Z + Z
< = Z     {- definition of (+) -}
< = Z + Z {- definition of (+) -}

< Given n + Z = Z + n, S n + Z = Z + S n = S n
< S n + Z
< = n + S Z     {- definition of (+) -}
< = n + Z + S Z {- definition of (+) -}
< = Z + n + S Z {- inductive hypothesis -}
< = Z + S n + Z {- definition of (+) -}
< = S Z + n + Z {- definition of (+) -}
< = S Z + Z + n {- inductive hypothesis -}
< = Z + S Z + n {- definition of (+) -}
< = Z + Z + S n {- definition of (+) -}
< = Z + S n     {- definition of (+) -}
< = S n         {- definition of (+) -}

`S Z` is the identity of `(*)`
-

< Lemma S Z is the identity of (*)
< forall n :: Nat. S Z * n = n * S Z = n

< S Z * Z = Z * S Z = Z
< S Z * Z
< = Z + (Z * Z) {- definition of (*) -}
< = Z           {- definition of (+) and (*) -}
< = Z * S Z     {- definition of (*) -}

< Given that S Z * n = n * S Z, S Z * S n = S n * S Z = S n
< S Z * S n
< = S n + (Z * S n)   {- definition of (*) -}
< = S n + Z           {- definition of (*) -}
< = Z + S n           {- Lemma Z is the identity of (+) -}
< = S n               {- definition of (+), S Z * S n = S n was demonstrated -}
< = Z + S n           {- definition of (+) -}
< = S Z + n           {- definition of (+) -}
< = S Z + (Z + n)     {- definition of (+) -}
< = S Z + (n + Z)     {- Lemma Z is the identity of (+) -}
< = S Z + n + (Z * n) {- definition of (*) -}
< = S Z + (S Z * n)   {- definition of (*) -}
< = S Z + (n * S Z)   {- inductive hypothesis -}
< = S n * S Z         {- definition of (*) -}

`(+) :: Nat -> Nat -> Nat` is associative
-

< Lemma succ is 1 +
< forall n :: Nat. S n = S Z + n

< S n
< = Z + S n {- definition of (+) -}
< = S Z + n {- definition of (+) -}

< Lemma S applies left
< forall m n :: Nat. S (m + n) = S m + n

By induction on `m`

< S (Z + n) = S Z + n
< S (Z + n)
< = S n     {- definition of (+) -}
< = S Z + n {- succ is 1 + -}

< Given forall n. S (m + n) = S m + n, S (S m + n) = S (S m) + n
< S (S m + n)
< = S (m + S n) {- definition of (+) -}
< = S m + S n   {- inductive hypothesis -}
< = S (S m) + n {- definition of (+) -}

< (+) is associative
< forall m n o :: Nat. m + (n + o) = (m + n) + o

By induction on `m`

< Z + (n + o) = (Z + n) + o
< Z + (n + o)
< = n + o       {- definition of (+) -}
< = (Z + n) + o {- definition of (+) -}

< Given forall n o :: Nat. m + (n + o) = (m + n) + o, S m + (n + o) = (S m + n) + o
< (S m + n) + o
< = (m + S n) + o {- definition of (+) -}
< = m + (S n + o) {- inductive hypothesis -}
< = m + S (n + o) {- S applies left -}
< = S m + (n + o) {- definition of (+) -}

The real end
-

- Functional programming is a ghetto
- Functional programming with dependent types is an ivory tower

Functor class
-

< class Functor f where
<     fmap        :: (a -> b) -> f a -> f b
<
<     -- | Replace all locations in the input with the same value.
<     -- The default definition is @'fmap' . 'const'@, but this may be
<     -- overridden with a more efficient version.
<     (<$)        :: a -> f b -> f a
<     (<$)        =  fmap . const

From the documentation:
The Functor class is used for types that can be mapped over. Instances of Functor should satisfy the following laws:

 fmap id  ==  id
 fmap (f . g)  ==  fmap f . fmap g

Functor Maybe instance
-

< class Functor f where
<     fmap        :: (a -> b) -> f a -> f b

<!---
data Maybe :: * -> * where
  Nothing :: Maybe a
  Just :: a -> Maybe a
--->

< data Maybe a = Nothing | Just a
<
< instance Functor Maybe where
<   fmap _ Nothing = Nothing
<   fmap f (Just x) = Just (f x)

Functor List instance
-

< class Functor f where
<     fmap        :: (a -> b) -> f a -> f b

<!---
data List :: * -> * where
  Nil :: List a
  Cons :: a -> List a -> List a
--->

\begin{code}
data MyList a = Nil | Cons a (MyList a)

instance Functor MyList where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
\end{code}

More examples
-

TODO copy some examples from: http://www.codeproject.com/Articles/432071/Computational-Types-in-Csharp-and-Fsharp
