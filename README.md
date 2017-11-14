# Compiler for WACC
[![pipeline status](https://gitlab.doc.ic.ac.uk/waskell/compiler/badges/master/pipeline.svg)](https://gitlab.doc.ic.ac.uk/waskell/compiler/commits/master)
[![coverage report](https://gitlab.doc.ic.ac.uk/waskell/compiler/badges/master/coverage.svg)](https://gitlab.doc.ic.ac.uk/waskell/compiler/commits/master)

Hello and welcome to Waskell, recommended reading before stating this project:

* [Tackling the Awkward Squad](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/mark.pdf)
  by Simon PEYTON JONES. Especially the section on how to perform * unsafe IO *
* [Alex Lexer Documentation](https://www.haskell.org/alex/doc/html/index.html)
* [Happy Parser Documentation](https://www.haskell.org/happy/doc/html/index.html)

You'll find all required information to understand the Front-End of the compiler
in those. As for global the project structure:
* `src` will contains all the library files we have written
* `src-test` contains tests for the library
* `src-exe` contains the executable
* `src-benchmark` contains a quick benmarking routine
* `Makefile` will make the project and create and executable that can be run by
  `./compile [-v] (file)+`

The [Haddock documentation](https://gitlab.doc.ic.ac.uk/waskell/compiler/-/jobs/artifacts/master/download?job=doc)
is generated automatically by the pipeline, go check
it out!

Go checkout the [test coverage](https://gitlab.doc.ic.ac.uk/waskell/compiler/-/jobs/artifacts/master/download?job=test)
too! 

## Communication

We decided to use a variaty of tools for communication and sharing code, our
main channel was Slack, to which we added hooks from Gitlab to notify about
pipeline issues, pushed to master etc. A Slack bot was added to easily create 
issue right into Slack, tracked by gitlab.

## Miscelaneous

### Style guide

You can find the style guide used for the project in [`STYLE.md`](https://gitlab.doc.ic.ac.uk/waskell/compiler/blob/master/STYLE.md).
As expected, writting a styleguide for Haskell is very difficult as sometimes
GHC's behavior is unpredicatable and varies depending of the position in the
file. The ultimate judge shouldn't be this styleguide but the programmer that
only take it as a coherence guideline.

### BNF Converter generator file

This tool was used to get us started quickly on semantic analysis and have an
ADT to use. It was decided to clutter the tree with as many informations as
possible and cherry picking in the analysis.  
The file is available [here](https://www.gitlab.doc.ic.ac.uk/waskell/compiler/blob/master/STYLE.md)
for legacy purposes. Starting quickly enabled us to share the workload. Each
team could work on implementing the semantic analysis and improving the parser
separatly. The merge was made on the weekend of the 4th of November.

## Team

Waskell group 26 formed of:
  * Kyle-James Condon    (kc1616)
  * Pranav Kalidindin    (pvk16)
  * Nathaniel Oshunniyi  (ono16)
  * Mike Fournial        (mmf115)