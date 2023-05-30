# Haskellship

A simple CLI Battleship game written in Haskell that simulates naval warfare.

## Description

The game is played by two players, each with their own grid-based game board representing a fleet of ships. The objective of the game is to strategically deploy your ships on the grid and successfully guess the location of your opponent's ships, ultimately sinking them.

At the beginning of the game, players place their ships on their own grids, and the ships are hidden from the opposing player. The types of ships and their sizes vary: carrier (5), battleship (4), cruiser (3), submarine (3), and destroyer (2). The ships are positioned either horizontally or vertically on the grid, and they cannot overlap or extend beyond the boundaries.

Once the ships are set up, players take turns calling out coordinates on the opponent's grid, attempting to hit their ships. The grids are labeled with numbers along the horizontal axis and letters along the vertical axis, creating a coordinate system. If a player's guess hits a ship, the game will declare it as a hit, and the hit location is marked on the grid. The player can continue to guess in the same vicinity to sink the entire ship. If the guess misses, it is declared as a miss, and the game marks it on the grid to keep track.

The game progresses as players strategically deduce the possible locations of their opponent's ships based on the hits and misses. It requires logical thinking, deduction, and a bit of luck to outmaneuver the opponent and sink their fleet before they sink yours. The first player to successfully sink all of their opponent's ships is declared the winner of the game.

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

## Instructions

### Coordinates

The grids are labeled with numbers along the horizontal axis and letters along the vertical axis, creating a coordinate system. For example, the coordinates `D5` represent the 5th column of the 4th row.

### Placing ships

To place ships, you need to enter a range of coordinates that indicate the head and tail of the ship. This is done by entering two coordinates that must comply with the following conditions:

- The coordinates must reside inside a 10x10 grid. For example, `D10` is a valid coordinate, but `D11` is not.
- When the ship is placed horizontally, the coordinates must range from left to right e.g. `C3 C6`
- When the ship is placed vertically, the coordinates must range from top to bottom e.g. `B2 D2`

## Firing

In order to fire at the opponent's board, you need to enter a single coordinate. The coordinates given must reside inside a 10x10 grid.

## Caveats

### Terminal clearing

The application will attempt to clear the terminal each player turn to prevent players from seeing eachothers' boards. Under the hood, the game runs the command `clear` each turn, which is NOT available in Windows environments. You can disable this feature by running the program with `noclear` flag like so:

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

The debug mode will provide you with useful information about what's going on during each turn. You can enter debug mode by including the `debug` flag when running the game:

```console
$ ghc Main.hs
$ ./Main debug
```
