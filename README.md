# rosac - the rosalia compiler

Rosalia is a functional programming language.
You're looking at the compiler for it. It's written in Haskell.

## Building

- Install cabal
- Install the latest version of GHC
- `cabal build`
- If that doesn't work, you may want to report an issue.

## The Rosalia language

Rosalia is a functional programming language. It's a bit like Haskell, especially
in its syntax, and should be familiar to anyone who's used Haskell before.
The main differences were made to make Rosalia more suitable for beginners, and to
make writing Rosalia code more fun compared to Haskell. Rosalia is a statically
typed language.

### Syntax

Sample code can be found in the `examples` directory.

Notice that Rosalia does not use `::` for type annotations. Instead, it uses a single `:`.
Additionally, although not shown in the example, Rosalia supports a variety of unicode
syntactic sugar, such as `λx` for `\x`, `→` for `->`, `∀` for `forall`, and `∈` for `in`.
This was inspired by Agda.

### Types

Rosalia has a several built-in types. Here's a list of the most important ones:

- Integer - Arbitrary precision integers
- Float - Floating point numbers (Double precision)
- String - A true string - NOT lists of characters, like in Haskell!
- Char - A single character
- EntryPoint - The type of the main function - allows IO, but is unique in that
 Rosalia uses it to determine the entry point of the program. Thus, you can name
 `main` whatever you want.
- IO - The IO monad. Allows you to do IO operations, like reading from the
 console or writing to a file.
- List - A list of elements. Lists are homogenous, meaning that all elements
 must be of the same type. Lists are immutable, meaning that you cannot change
 the elements of a list. You can, however, create a new list with the elements
 of the old list and some new elements.

### In-built functions

- say - Prints a string to the console
- exit - Exits the program
- eval - hahahahahaha
