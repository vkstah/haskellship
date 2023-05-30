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

You can run a premade sample game that uses some example input to demonstrate that the game works:

```console
$ ghc Main.hs
$ ./Main < sample-game
```

## Caveats

### Terminal clearing

The application will attempt to clear the terminal each player turn to prevent players from seeing eachothers' boards. Under the hood, the application runs the command `clear` each turn, which is NOT available in Windows environments. You can disable this feature by running the program with `noclear` flag like so:

```console
$ ghc Main.hs
$ ./Main noclear
```

You can do this with a sample game as well:

```console
$ ghc Main.hs
$ ./Main < sample-game noclear
```

### Debug mode

The debug mode will provide you with useful information about what's going on during each turn. You can enter debug mode by including the `debug` flag when running the application:

```console
$ ghc Main.hs
$ ./Main debug
```
