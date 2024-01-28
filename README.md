# ForSyDe-radar-co-simulation
This is a repo for co-simulation group work in IL2232 Embedded Systems Design Project.

# ForSyde IO Graph Representation and DFB Component Implementation

## Overview

This repository contains files related to the ForSyde IO graph representation and the re-implementation of the original DFB component. It showcases the use of the ConverSyde tool and inline-C library in embedding C functions into Haskell programs.

## Install Instruction
1. cabal update
   cabal install inline-c
2. convert ghc version to 9.0.1
  ghcup set ghc 9.0.1
3. edit package.yaml like this file:
name:                aesa-shallow
version:             0.1.0.0
github:              "githubuser/aesa-shallow"
license:             BSD3
author:              "Author name here"
maintainer:          "example@example.com"
copyright:           "2019 Author name here"

extra-source-files:
- README.md
- ChangeLog.md

### Metadata used when publishing your package
### synopsis:            Short description of your package
### category:            Web

### To avoid duplicated efforts in documentation and dealing with the
### complications of embedding Haddock markup inside cabal files, it is
### common to point users to the README.md file.
description:         Please see the README on GitHub at <https://github.com/githubuser/aesa-shallow#readme>

dependencies:
- base >= 4.7 && < 5
- vector >= 0.12
- raw-strings-qq
- aesa-atom
- forsyde-shallow
- forsyde-shallow-extensions
### for the executables
- directory
- filepath
- process
- bytestring-lexing
- bytestring
- double-conversion
- deepseq
- parallel
- inline-c



library:
  source-dirs: src

executables:
  aesa-shallow:
    main:                Main.hs
    source-dirs:         app
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - aesa-shallow
    - vector >= 0.12
    - raw-strings-qq

tests:
  aesa-shallow-test:
    main:                Spec.hs
    source-dirs:         test
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - aesa-shallow
    - vector >= 0.12
    - raw-strings-qq

## Files and Folders Description

### Graph Representation

- `dfb.fiodl`: The original ForSyde IO model file.
- `DFB_IO.png`: A screenshot of the `dfb.kgt` graph, visually representing the ForSyde IO model.
- `dfb.kgt`: A graph representation of the ForSyde IO model, generated from `dfb.fiodl` using the ConverSyde tool.

### DFB Component Implementation

- `DFB_Shallow.png`: Visual representation of the DFB component implemented in ForSyde-Shallow.
- `dfb.fiodl`: Re-implementation of the original DFB component in ForSyde-Shallow.
- `dfb.c`: A C language re-implementation of the DFB component, including CSV file IO features.
- `output.csv`: Output file generated by applying test data to `dfb.c`.

### Inline-C Examples

- `inlineC/`: A folder containing three examples demonstrating the use of the inline-C library to embed C functions into Haskell programs.

## Usage

The files in this repository can be used for studying the graph representation of ForSyde IO, understanding the re-implementation of the DFB component, and learning about integrating C functions into Haskell programs using the inline-C library.

## Tools

- **ConverSyde Tool**: Used for converting `dfb.fiodl` into the `dfb.kgt` graph.
- **Inline-C Library**: Facilitates embedding C functions into Haskell programs, as demonstrated in the `inlineC` folder.



## Contact

For any questions or contributions regarding this project, please contact the following individuals:

- Yaoyu Zhang: yaoyuz@kth.se
- Shiyuan Shan: shiyuan@kth.se
- Tao Xiong: taox@kth.se

Feel free to reach out with inquiries, suggestions, or potential contributions to the project.


