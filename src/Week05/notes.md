# Week 05

# Example

* Calculating a shopping bill for people and teams
* Start in C#
  * We can check types at runtime, risking runtime exception
  * We can use ad-hoc polymorphism, e.g. introduce Groceryable interface
  * We can use parametric polymorphism (without much guarantees), using generics
* Move to Haskell
  * We can't do any run time checks
  * We have parametric polymorphism, which provides the caller _guarantees_
  but _constrains_ the implementor.
  * So what to do when we want the ad-hoc kind? i.e. we do want _different_
  behaviour for different types? .... TYPE CLASSES!

## More thoughts on Parametricity

* Talked about _constraining_ the possible implementations
* Because you can't do any runtime type checks, this means you can
have guarantees about what a function will do. Sometimes parametricity
even means that there can only be a finite (or one/none) compilable implementations
* You can't pull values out of thin air in Haskell (no default(T))
* EX: Play the parametricity game with Haskell and C#

## Differences between interfaces and type classes?

* classes must declare and implement the interface contract when they are defined
vs type class instances can be defined separately
* multiple argument functions are more elegantly expressed with type classes
* multiple vs single dispatch means class Blerg a b can't really be expressed
with generics and interfaces? I don't really understand this very well.

## Homework

* you'll need an ord instance for MaxMin
* says to do either exercise 5 _or_ 6. Feel free to do them both :)
* for exercise 6 you can copy `withVars` from the spec file
