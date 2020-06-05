# sinh-language
A JavaScript-like programming language developed using Haskell

## Prerequisites
Make sure you have downloaded [Haskell](https://www.haskell.org/downloads) and [Stack](https://docs.haskellstack.org/en/stable/README/).

This project requires [Happy Parser](https://www.haskell.org/happy/) and [Alex](https://www.haskell.org/alex/) to parse the language.

You can get all of the aforementioned packages in one step by installing [Haskell Platform](https://www.haskell.org/platform/).

## How to run
Clone or download and unzip this repository and open terminal to the project directory.
* `stack install`: To install the SINH language globally (see alternate way to run without installation below)
* `sinh`: To run the shell (see shell commands below)
* `sinh <filename>`: To run a program (see example programs and syntax in `examples` folder)


### Run without installation (Alternate way)
After cloning this repository:
* `stack build`: To buld the SINH language program
* `stack exec sinh`: To run the shell (see shell commands below)
* `stack exec sinh <filename>`: To run a program (see example programs and syntax in `examples` folder)


## Shell commands
* Run any program (see `examples` folder for syntax)
* `:h` to see list of all commands
