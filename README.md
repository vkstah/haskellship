# Haskellship

A simple CLI Battleship game written in Haskell.

## Requirements

[GHC](https://www.haskell.org/ghc/) >= 9.2.7

## Quick Start

You can run the application with minimal steps using `runghc`:

```console
$ runghc Main.hs
```

Alternatively, you can compile the application and run it:

```console
$ ghc --make Main.hs
$ ./Main
```

NOTE: The application will attempt to clear the terminal each player turn to prevent players from seeing eachothers' boards. Under the hood, the application runs the command `clear` each turn, which is NOT available in Windows environments. You can disable this feature by running the program with `noclear` flag like so:

```console
$ runghc Main.hs noclear
```
