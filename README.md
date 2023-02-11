# sht-editor: simple haskell text editor

A terrible text editor, intended to be a project to learn a little bit more Haskell. The application loosely follows [the Elm architecture](https://guide.elm-lang.org/architecture) (probably a terrible choice for a *real* text editor).


## Exhaustive list of **FEATURES**
* open a file
* move around the file with arrows / Home / End / PageUp / PageDown
* type in the file by pressing letter/number keys
* erase characters by pressing backspace
* erase entire line by pressing ctrl+k
* save a file with ctrl+s (there's no way to specify a different location than the one initially opened)


## Instructions

All of the instructions below assume your working directory is the root of the repository.

### Building & run locally
1. Install [GHC](https://www.haskell.org/ghc/) and [Cabal](https://www.haskell.org/cabal/) if you don't already have them
2. Update cabal package index: `cabal update`
3. Build & run with `cabal run`

For development consider installing GHCup and HLS (language server for Haskell).

### Run it inside docker
1. Build the image `docker build --tag sht .`
2. Run the container `docker run --rm -it sht`
