# Compiler Construction Supplementary Code

Use `dune utop src` to play with the Lexer/Parser.

Do `git checkout exercise` to open up the exercises on the
interpreters/compilers.

Build with `dune build src/CompConstr.a --profile release` when
writing the code.

Once all the holes have been filled in:
 - Remove the `[@@@ocaml.warning "-27"]` from the top on any file that has it
 - `dune build src/CompConstr.a` should give no warnings or errors
