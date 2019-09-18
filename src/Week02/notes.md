# Week 02 Notes

## Types of our very own

* we used a lot of recursion, but most of the time you use functions that hide
* we've seen built-in types (`Int`, `Bool`, `[]`, `(a, b)` etc.)
* you may have seen type aliases e.g. `type String -> [Char]`
* we can define our own types using the `data` keyword which defines
an 'algebraic data type'
* we can use data constructors without arguments to get an enum-like type
* data constructors must start with a capital letter
* data constructors are _just functions_
  this away
* EX: GroceryItem

## The magic of deriving

* trying to print out our ADTs, or compare two values doesn't work
* we can get "out of the box" funcitonality using the `deriving` keyword
* common type classes to derive are `Eq`, `Show`, and `Ord`
* `Read` allows you to parse strings to types

## More than enumerations

* Just have data constructors that take arguments!
was used
* data type grammar (from notes)
* EX: GroceryRequest

## Pattern matching

* Pattern matching working out which data constructor was used
* every function you write uses pattern matching
* we've seen pattern mathcing on the LHS of the equals
* there is also a way to pattern match on the RHS of the equals
* this is actually what _always_ happens, Haskell just does the
transformation for us
* the pattern matching grammar (from lecture notes)
* work in `where` and `let...in`
* EX: acceptRequest :: String -> GroceryRequest
  * pattern matching more than one item in a list
  * the `read` function
  * reword to use the case of

## Recursive Data Types

* data constructors can be recursive, so a data contructor will
use a value of the type it's constructing

data IntList = [] | Int : IntList Int

* EX: GroceryList as a recursive type

## Misc tips

* modules, just enough. I don't know any more than that.
* hoogle refresher, and getting to module from hoogle/hackage
  * use the "included with ghc" set when hoogling
  * Can use package and module filters, e.g. `a -> String package:base module:Prelude`
* browsing and importing module in ghci
* `read`, `words`, `unwords`, `lines`, `unlines`
* using a named pattern e.g. lm@(LogMessage ...)
* when inserting into your binary tree
  * insert tests favour insertion to the left and
  * don't allow the insertion of an `Unknown` log event

