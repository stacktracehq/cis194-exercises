# In-house Training Based on CIS 194 Haskell Course

## What is this?

We decided we wanted to ramp up the team's functional programming skills. To this end, we committed to working through [Brent Yorgey's CIS 194 lecture series on Haskell](http://www.seas.upenn.edu/~cis194/spring13). This is a 12 week course and there are homework assignments for each week.

Our goal is to work through a week of the course's content every Friday afternoon. Our first session will be on Friday the 20th of July, 2018.

Jump into the #fp channel in the Stacktrace slack to chat about the course, or any FP concepts in general.

## How to work with this Repository

### Setup

#### Option 1 (recommended)

1. Install the [Nix package manager](https://nixos.org/nix/)

2. Run the following

  ```
  $ nix-shell --command 'cabal configure --enable-tests && cabal test'
  ```
3. To run ghci

  ```
  $ nix-shell --command ghci
  ```

From now on you _can_ type `nix-shell` and be put into a bash shell which has `ghc`, `ghci`, `cabal`, `ghcid`, `hlint`, and `hindent`. For more info on using Nix ask in #dotfiles.

#### Option 2

1. Install the [Haskell Platform](https://www.haskell.org/platform/). This includes the ghc compiler, ghci repl, and cabal build tool.

2. Run the following

  ```
  $ cabal configure
  $ cabal install cabal-install
  $ cabal install --only-dependencies --enable-tests
  $ cabal configure --enable-tests
  $ cabal test
  ```

3. To run ghci

  ```
  $ ghci
  ```

### Using ghci

You'll be spending a lot of time in `ghci`, which is a **R**ead, **E**val, **P**rint, **L**oop (aka 'repl'). Once you enter `ghci` you can run commands, which are prefixed with ':'. Entering `:help` will print a list of available commands.

The most helpful of these are likely to be `:info` and `:type`.

`:info` or `:i` for short will tell you everything GHCI can about an expression.

```
 > :i Maybe
data Maybe a = Nothing | Just a         -- Defined in ‘GHC.Base’
instance Applicative Maybe -- Defined in ‘GHC.Base’
... MORE

 > :i (+)
class Num a where
  (+) :: a -> a -> a
  ...
        -- Defined in ‘GHC.Num’
infixl 6 +
```

`:type` or `:t` for short will tell you the type of an expression.

```
 > :t Just
Just :: a -> Maybe a

 > :t (+)
(+) :: Num a => a -> a -> a
```

### Homework Exercises

Each week we'll be adding a set of [hspec](https://hspec.github.io/) specs in a `./test/Week<N>/` directory. We'll also add some files in `./src/Week<N>/` to serve as a starting point for your implementations. To run _only_ the tests for a given week you can use GHCI. Below is an example of running only the credit card tests for week one.

```
$ ghci test/Week01/CreditCardValidatorSpec.hs
GHCi, version 8.2.2: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /Users/bradparker/code/cosmos/training/CIS194/.ghci
Ok, two modules loaded.
 > hspec spec

CreditCardValidator
  toDigits
    converts positive Integers to a list of digits FAILED [1]
  toDigitsRev
    does what toDigits does in reverse FAILED [2]
  doubleEveryOther
    doubles every second digit starting from the second last FAILED [3]
  sumDigits
    sums the sum of the digits of all numbers in a list FAILED [4]
  validate
    indicates whether an Integer could be a valid credit card number FAILED [5]

Failures:
... error details and more!
```

You can also run `dev/watch` to get a file-watching test-re-running setup going with the help of GHCID.

To run only the tests for a given spec file using the watch script (defaults to all specs):

```
$ dev/watch test/Week01/CreditCardValidatorSpec.hs
```

### Tmux

There is a script `dev/up` that will tmux up your editor, tests (using `dev/watch`) and a repl for a given week.

_Note_: Both `tmux` and `tmux-up` are provided by `nix` so it is best to run it from a `nix-shell` *detatched from any tmux session(s)*.

Args are

1. "windows" | "panes"
2. "WeekXX" | "All"
3. An editor to run

For example:

```sh
$ nix-shell
$ dev/up windows Week02 vim
```

Another example
```
$ nix-shell
$ dev/up panes All code
```
