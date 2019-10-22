# Week 03 Notes

## More on Package and Module Docs, and the Prelude

* We are going to start using more from the Prelude, so let's look at Hackage
* Hackage is the central _package_ archive.
* Can search Hackage directly, or get there from Hoogle
* A _package_ contains one or more _modules_, e.g. the `base` package contains the `Prelude` module.
* Top level search is for _packages_
* Once you have selected a package, you can search across all it's contents
* The module level page (should) have docs/examples.
* You can also get straight to the actual source
* You can also browse modules from your REPL
* Links to Prelude and list operations are broken

## Recursion Patterns: `map` and `filter`

* Language Extensions, i.e. `NoImplicitPrelude`
* We'll make a list and make Cons right-associative
* We've done a lot of recursion based around lists, and some common patterns
keep coming up.
  * `map`: perform some operation on every element of a list
    * This is `Select` in Linq
  * `filter`: keep only some elements of a list, and throw others away, based on a test
    * This is `Where` in Linq
  * `fold`: ... next week.
    * This is often called `reduce`
    * This is `Aggregate` in Linq
    * maybe a quick example. Keep em keen.

## Polymorphism

* Can write generic functions using type parameters
* Type parameters have to be lowercase letters. You usually see letters from the start
of the alphabet, e.g. `a`, `b`, `c`
* Lots of types are 'mappable', not just lists. A more generic version of the
map function is `<$>`, pronounced "fmap". We'll cover this when we talk about Functors.
* Similarly lots of types are Foldable, which enables filtering
  * try deriving Foldable for our generic list?

## Total and Partial Functions

* functions map values in their _domain_ (i.e. their inputs) to values in their _range_
(i.e. their output)
* if every value in the _domain_ maps to a value in the _codomain_, then the
function is `total`, and a force for good in the world
* if there are _any_ values in the _domain_ which do not map to a value in
the _codomain_, then the function is `partial`. Partial functions were invented
by terrorists.
* We can turn a partial function into a total function, and should
strive to do so, by either _expanding the codomain_ or _shrinking the domain_.
* EX: `head` + `Maybe` and `NEL`

## Homework Tips

* Use the Preludes `map` and `filter`
* Look into `zip` (and it's other variants)
* tuples have a `fst` and `snd`
* Generally, start more verbose then try and reduce
