# Compiler for WACC
[![pipeline status](https://gitlab.doc.ic.ac.uk/waskell/compiler/badges/master/pipeline.svg)](https://gitlab.doc.ic.ac.uk/waskell/compiler/commits/master)
[![coverage report](https://gitlab.doc.ic.ac.uk/waskell/compiler/badges/master/coverage.svg)](https://gitlab.doc.ic.ac.uk/waskell/compiler/commits/master)

Hello and welcome to Waskell. Recommended reading before starting this project:

* [Tackling the Awkward Squad](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/mark.pdf)
  by Simon Peyton Jones, especially the section on how to perform *unsafe IO*
* [Alex Lexer Documentation](https://www.haskell.org/alex/doc/html/index.html)
* [Happy Parser Documentation](https://www.haskell.org/happy/doc/html/index.html)

These resources should suffice to provide all information required to understand the Front-End of the compiler.
As for global the project structure:
* `src` will contain all the library files we have written
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

We decided to use a variety of tools for communication and sharing code; our
main channel was Slack, to which we added hooks from Gitlab to notify about
pipeline issues, pushes to master etc. A Slack bot was added to easily create 
issues right into Slack, tracked by gitlab.

## Miscellaneous

### Style guide

You can find the style guide used for the project in [`STYLE.md`](https://gitlab.doc.ic.ac.uk/waskell/compiler/blob/master/STYLE.md).
As expected, writting a styleguide for Haskell is very difficult as sometimes
GHC's behavior is unpredicatable and varies depending on the position in the
file. The ultimate judge shouldn't be this styleguide, the programmer should
only take it as a coherence *guideline*.

### BNF Converter generator file

This tool was used to get us started quickly on semantic analysis and have an ADT to use. 
However, it cluttered the tree with too much information, and was cherry picking in the analysis.  
(The file is available [here](https://www.gitlab.doc.ic.ac.uk/waskell/compiler/blob/master/STYLE.md)
for legacy purposes.) Starting quickly enabled us to share the workload; each
team could work on implementing the semantic analysis and improving the parser
separatly. The merge was made on the weekend of the 4th of November.

## Team

Waskell group 26 formed of:
  * Kyle-James Condon    (kc1616)
  * Pranav Kalidindin    (pvk16)
  * Nathaniel Oshunniyi  (ono16)
  * Mike Fournial        (mmf115)
