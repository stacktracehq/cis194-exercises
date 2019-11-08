## Anonymous functions (aka lambda functions)
* lambda functions, we've all seen these by now?
* let's you avoid writing separate functions
* EX: filter krankies with and without a lambda
* Compare to Linqs Where
* We can actually think of all the functions we write as lambdas
* EX: rewrite a function to just be a LHS lambda
* interesting historical aside: Alonzo Church + Alan Turing
* explain newtypes

## Currying and Partial Application
* It's all lambdas. Shhhhhh.
* function arrow associates to the right
* function application associates to the left
* You can _partially apply_ functions that are _curried_
* EX: multi-arg add
* EX: remove the xs from our Krankie filter
* EX: `:set -XTypeApplications`

## Operator Sections (Partially applying binary operators)
* Binary operator is a function that takes "two" args and is usually used infix
* We can partially apply them to the left or right operand

## Function Composition
* Deriving compose (it only has one implementation that will compile)
* this allows us to write 'point-free' functions, i.e. functions that don't
reference their arguments on lhs of =
* can read `.` as "after", ex: length of a Krankie-filtered list

## Folds
* We talked about the `map` and `filter` recursion patterns last week
* We've also seen another pattern where we 'reduce'/'aggregate'/'fold' a list into a value
* EX: `sum`, `product`, `length`
  * write with manual recursion
  * rewrite with a fold (right)
* EX: Hand trace fold exeuctions
* EX: Folds as ASTs https://gist.github.com/CMCDragonkai/9f5f75118dda10131764
* foldr can early return due to lazyness, foldl can't (but foldl' can)
  * EX: anyTrue on 8 million falses
* left and right won't always give the same answer
  * EX: using (-) as our fold func

## Used in Homework

* First exercise is pretty tricky. Be prepared to stare at it for a while. I used
  * `fun1`: `foldr`, `subtract`, `(.)`
  * `fun2`: `iterate`, `takeWhile`, `(.)`
  * for fun2 exeucute by hand and look at the recurision pattern
* Second exercise is about inserting into a binary tree (_not_ a binary _search_ tree)
  * suggest writing a `height :: Tree a -> Integer` function. What's the height of a `Leaf`?
  * drawing out graphs and hand tracing can help
* There is an optional exercise to implement foldl using foldr. There is a simple solution, but
I don't think this is what they are after.

