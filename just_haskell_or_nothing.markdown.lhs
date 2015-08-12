% Just Haskell or Nothing
% John Chee
% @chee1bot on Twitter | @cheecheeo on Github

Some libraries that we'll be using for this presentation
-

\begin{code}
module JustHaskellOrNothing where

import Control.Applicative
import Data.Maybe
import Data.Monoid
\end{code}

<center>![Donald Knuth](./dek-14May10-2-resize.jpeg)</center>

Outline
-

- Maybe in action
- Maybe itself
    - Definition & usage
    - Examples
    - Some functions that construct Maybes
    - Parsing North American phone numbers
    - Destructing or eliminating values
- More tools to deal with Maybes
    - Here, have a default
    - Another way to think of Maybes
    - Dealing with collections of Maybes
- Opening up the Typeclassopedia
    - fmappable things AKA Functor instances
    - Applicative instance
    - liftAn
- Further Study

Maybe in action
=

Maybe can be used to represent nulled values
-

In Javascript:

< > 20 * 2 + 2 + null
< 42

In SQL:

< mysql> select 20 * 2 + 2 + null;
< +-------------------+
< | 20 * 2 + 2 + null |
< +-------------------+
< |              NULL |
< +-------------------+
< 1 row in set (0.01 sec)

In Haskell:

< λ: fmap (20 * 2 + 2 +) Nothing
< Nothing

Maybe can be used to represent possibly-failed computations
-

Rather than:

< λ: head []
< *** Exception: Prelude.head: empty list

< λ: safeHead []
< Nothing
< λ: safeHead [1,2,3,4]
< Just 1
< λ: safeHead [42]
< Just 42

Or:

< λ: 12 `div` 0
< *** Exception: divide by zero

< λ: 12 `safeDiv` 0
< Nothing
< λ: 42 `safeDiv` 2
< Just 21

Maybe itself
=

Definition & usage
-

< data Maybe a = Nothing | Just a

- When you have a type `Maybe Foo` you know you have values like:
    - Nothing
    - Just f where f is a value with type `Foo`

Examples
-

- `Integer`s
- `Maybe Integer`s
- `Bool`s
- `Maybe Bool`s

. . .

< λ: [0..5]
< [0,1,2,3,4,5]

< λ: [Nothing] ++ map Just [0..5]
< [Nothing,Just 0,Just 1,Just 2,Just 3,Just 4,Just 5]

< λ: [True, False]
< [True,False]

< λ: [Nothing] ++ map Just [True, False]
< [Nothing,Just True,Just False]

Some functions that construct `Maybe`s
-

- Possibly-failed computations:

\begin{code}
safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (x : _) = Just x
\end{code}

\begin{code}
safeDiv :: Integer -> Integer -> Maybe Integer
safeDiv n d =
  if d == 0
    then Nothing
    else Just (n `div` d)
\end{code}

Parsing North American phone numbers
-

In this case a phone number that isn't valid maps to Nothing.

\begin{code}
data PhoneNumber = PhoneNumber Integer Integer Integer
  deriving (Show)

parsePhoneNumber :: Integer -> Maybe PhoneNumber
parsePhoneNumber n =
  if n >= 2002000000 && n < 10000000000 -- the correct number of digits
    then let (areaCode, m) = n `divMod` 10000000
             (centralOfficeCode, subscriberNumber) = m `divMod` 10000
         in if areaCode >= 200 && centralOfficeCode >= 200 && centralOfficeCode `mod` 100 /= 11
              then Just (PhoneNumber areaCode centralOfficeCode subscriberNumber)
              else Nothing
    else Nothing
\end{code}

. . .

Examples:

< λ: parsePhoneNumber 1234567890
< Nothing
< λ: parsePhoneNumber 4155551234
< Just (PhoneNumber 415 555 1234)
< λ: parsePhoneNumber 4159112277
< Nothing
< λ: parsePhoneNumber 4159128347
< Just (PhoneNumber 415 912 8347)

Destructing or eliminating values
-

If we have a `Maybe` and we're ready to handle both the `Nothing` case and
the `Just` case we can:

pattern match:

\begin{code}
showMaybeInteger :: Maybe Integer -> String
showMaybeInteger m =
  case m of
    Nothing -> "There's nothing here."
    Just x  -> "I have something: " <> show x
\end{code}

. . .

< λ: showMaybeInteger Nothing
< "There's nothing here."
< λ: showMaybeInteger (Just 42)
< "I have something: 42"

or eliminate:

< maybe :: b -> (a -> b) -> Maybe a -> b

\begin{code}
showMaybeInteger' :: Maybe Integer -> String
showMaybeInteger' m =
  maybe
    "There's nothing here."                -- Nothing
    (\x -> "I have something: " <> show x) -- Just x
    m
\end{code}

- I can think of some good reasons to prefer the `maybe` function can you? (audience involvement)

Reasons to prefer `maybe`
-

< maybe :: b -> (a -> b) -> Maybe a -> b

- always cover all cases (without the compiler's help!)
- information hiding
    - rename `Just` to `Some` or `Nothing` to `None`
        - This won't actually happen

<center>![Eliminated](./bigstock-Eliminated-Red-Square-Grungy-S-66335674-583x388.jpg)</center>

More tools to deal with `Maybe`s
=

Here, have a default
-

<center>![Grexit](./IMFGoHome_3254366b.jpg)</center>

- If you're concerned about having to construct and destruct `Maybe` values all over the place
    - Don't worry


< fromMaybe :: a -> Maybe a -> a

If we want to do the Javascript thing and make `Nothing` be `0`

< λ: let (x, y) = (Nothing, Just 30000)
< λ: let jsNumbers = fromMaybe 0
< λ: 20 * 2 + 2 + jsNumbers x
< 42
< λ: jsNumbers y + 1337
< 31337

Another way to think of `Maybe`s
-

- A list with at most one element

< listToMaybe :: [a] -> Maybe a
< maybeToList :: Maybe a -> [a]

\begin{code}
safeHead' = listToMaybe
\end{code}

< λ: maybeToList Nothing
< []
< λ: maybeToList (Just "hello")
< ["hello"]
< λ: listToMaybe [1..10]
< Just 1
< λ: listToMaybe []
< Nothing

Dealing with collections of `Maybe`s
-

< catMaybes :: [Maybe a] -> [a]
< mapMaybe :: (a -> Maybe b) -> [a] -> [b]

< λ: catMaybes [Nothing,Just 1,Just 2,Nothing,Just 4,Just 5]
< [1,2,4,5]
< λ: length . mapMaybe parsePhoneNumber $ [4150000000..4159999999]
< 7920000

- With just these functions and `maybe` you can write a large number of useful programs

Opening up the Typeclassopedia
=

`fmap`pable things AKA `Functor` instances
-

- Just like we can `map` (or equivalently `fmap`) across lists
- We can `fmap` across `Maybe`s

< fmap :: (a -> b) -> Maybe a -> Maybe b

We can write our function without being concerned about `Maybe`:

\begin{code}
call :: PhoneNumber -> String
call (PhoneNumber areaCode centralOfficeCode subscriberNumber) =
  "I called (" <> show areaCode <> ") "
  <> show centralOfficeCode <> " " <> show subscriberNumber
  <> " and had a great conversation!"
\end{code}

and we'll perform computations if we have a `Just` and otherwise get `Nothing`

< λ: fmap call (parsePhoneNumber 4155551234)
< Just "I called (415) 555 1234 and had a great conversation!"
< λ: fmap call (parsePhoneNumber 1234567890)
< Nothing

We can `text` too
-

- Let's make another type

\begin{code}
data Phone = Android | IPhone | Hipster
\end{code}

\begin{code}
text :: Phone -> String
text p =
  case p of
    Android -> "I love swiping when I text."
    IPhone  -> "I love to type character by character."
    Hipster -> "Texting is so 2014."
\end{code}

We can use `Maybe Phone` to represent those rare times when you forget your cell phone.

< λ: fmap text (Just Android)
< Just "I love swiping when I text."
< λ: fmap text (Just Hipster)
< Just "Texting is so 2014."
< λ: fmap text Nothing
< Nothing

`Applicative` instance
-

- Let's make another function that uses both `Phone` and `PhoneNumber`

\begin{code}
callWithPhone :: Phone -> PhoneNumber -> String
callWithPhone phone (PhoneNumber areaCode centralOfficeCode subscriberNumber) =
  let prettyPhoneNumber = "(" <> show areaCode <> ") " <> show centralOfficeCode <> " " <> show subscriberNumber
  in case phone of
      Android -> "Google knows I just called " <> prettyPhoneNumber <> "."
      IPhone  -> "I called " <> prettyPhoneNumber <> " on the best phone!"
      Hipster -> "I called " <> prettyPhoneNumber <> " and asked for their Snapchat ID."
\end{code}

. . .

- `Maybe` is also an `Applicative` instance
    - This means we can embed expressions in `Maybe`s using `Just`
    - And combine `Maybe` computations using (<*>)

. . .

 - We want to call someone but what if we have a `Maybe Phone` and a `Maybe PhoneNumber`?
    - Don't be afraid of the syntax

< λ: (pure callWithPhone) <*> (pure Android) <*> (parsePhoneNumber 4155551234)
< Just "Google knows I just called (415) 555 1234."
< λ: (pure callWithPhone) <*> (pure IPhone)  <*> (parsePhoneNumber 1234567890)
< Nothing
< λ: (pure callWithPhone) <*> (pure Hipster) <*> (parsePhoneNumber 5035551234)
< Just "I called (503) 555 1234 and asked for their Snapchat ID."
< λ: (pure callWithPhone) <*> Nothing        <*> (parsePhoneNumber 1234567890)
< Nothing

Pop quiz
-

What about? (audience participation)

< λ: Nothing <*> Nothing <*> Nothing

. . .

<center>![Mind blown](./mind_blown-resize.gif)</center>

You don't even need a function for `Maybe`s `Applicative` instance
-

< λ: Nothing <*> Nothing <*> Nothing
< Nothing

<center>![A show about nothing](./seinfeld-cast-resize.jpg)</center>

`liftAn`
-

- If you can't get over the syntax (liftA, liftA2 and liftA3 are helpful)
- `liftA` is really just `fmap`
- `liftA2` can be thought of as `fmap` but for functions that take two arguments

< λ: liftA text (Just Android)
< Just "I love swiping when I text."

< λ: liftA2 callWithPhone (Just Android) (parsePhoneNumber 4155551234)
< Just "Google knows I just called (415) 555 1234."
< λ: liftA2 callWithPhone (Just IPhone) (parsePhoneNumber 1234567890)
< Nothing
< λ: liftA2 callWithPhone (Just Hipster) (parsePhoneNumber 5035551234)
< Just "I called (503) 555 1234 and asked for their Snapchat ID."
< λ: liftA2 callWithPhone Nothing (parsePhoneNumber 1234567890)
< Nothing

Further Study
-

- [Data.Maybe](http://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Maybe.html) in `base`
    - `Maybe` is an instance of `Monoid`, `Functor`, `Applicative`, `Monad`, `Foldable`, `Traversable`, `Alternative` and others.
- [witherable](http://hackage.haskell.org/package/witherable-0.1.3/docs/Data-Witherable.html#v:mapMaybe)
    - `mapMaybe` and `catMaybes` can have more generic types that allow them to work with `Traversable` instances rather than just lists.
- [MaybeT](http://hackage.haskell.org/package/transformers-0.4.3.0/docs/Control-Monad-Trans-Maybe.html#t:MaybeT)
    - Enrich any `Monad` with `Maybe` semantics
- [maybeT](http://hackage.haskell.org/package/errors-2.0.0/docs/Control-Error-Util.html#v:maybeT)
    - provides an eliminator for `MaybeT` values
- [Either](http://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Either.html#t:Either)
    - Like `Maybe` but allows you to hold data where you would have `Nothing`

Questions or comments?
-

`Maybe` wrappers
-

- You can think of "combining" or "adding" two `Maybe`s together
    - There are 3 useful ways of doing that:
        - First `First`

< λ: First (Just 123) <> First (Just 456) <> First Nothing
< First {getFirst = Just 123}

        - Second `Last`
< λ: Last (Just 123) <> Last (Just 456) <> Last Nothing
< Last {getLast = Just 456}

- TODO need to use Control.Newtype here

`Maybe` wrappers
-

- Finally `Maybe`
    - If it's possible to "add" the things that `Maybe` is holding we can "add" two `Maybe`s together
    - `Sum` and `Product` wrap `Integers` and tell `<>` how to "add" them together

< λ: (Just . Sum $ 1) <> (Just . Sum $ 41)
< Just (Sum {getSum = 42})
< λ: Nothing <> (Just . Sum $ 41)
< Just (Sum {getSum = 41})

        - Only need to use the inner `<>` when we have two `Just`s, othwise ignored.

< λ: (Just . Product $ 21) <> (Just . Product $ 2)
< Just (Product {getProduct = 42})
< λ: (Just . Product $ 21) <> Nothing
< Just (Product {getProduct = 21})

Addable things in Haskell
-

- Why the scare quotes?
- Haskell calls Addable things with an identity element a `Monoid`
    - Just like your abstract algebra textbook, or Wikipedia:

< Suppose that S is a set and • is some binary operation S × S → S, then S with • is a monoid if it satisfies the following two axioms:
<
< Associativity
<     For all a, b and c in S, the equation (a • b) • c = a • (b • c) holds.
< Identity element
<     There exists an element e in S such that for every element a in S, the equations e • a = a • e = a hold.

A rose by any other name
-

- We `mappend` x and y rather than "add" x and y
- `mempty` is our _e_ or _identity_ element

< class Monoid a where
<         mempty  :: a
<         -- ^ Identity of 'mappend'
<         mappend :: a -> a -> a
<         -- ^ An associative operation
<         mconcat :: [a] -> a
<         mconcat = foldr mappend mempty
<
< -- | An infix synonym for 'mappend'.
< --
< (<>) :: Monoid m => m -> m -> m
< (<>) = mappend

<img src="./rose.gif" style="float: left; width: 400px" alt="Rose" />

