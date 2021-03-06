# cria

[Cria](http://www.wep.co.nz/wep/member-images/85/image/New%20Cria%20/Snow%20Legend%20x%20Truly%20female%20cria.JPG) (n): a baby llama or alpaca

Goofing about with LLVM and OCaml. How the hell else am I going to learn it?

The goal is to implement a functional language with Lisp syntax (or lack thereof) and ML-like semantics, which will compile to LLVM.


Dependencies
============
* [opam](https://opam.ocaml.org/doc/Install.html). Linux users: don't install it from the package manager, use their installer script.
* OCaml 4.0+. opam will install an OCaml compiler for you.
* OCaml libraries: Menhir, Core, OUnit. Installable with opam.
* LLVM 3.4+


Useful links
============

* [Mapping high-level constructs to LLVM.](http://llvm.lyngvig.org/Articles/Mapping-High-Level-Constructs-to-LLVM-IR) Some info about how to implement lambda in LLVM.
* [LLVM BF compiler](https://github.com/jeremyroman/llvm-brainfuck)
* [Haskell LLVM example.](http://www.stephendiehl.com/llvm/) Does Kaleidoscope with Haskell LLVM bindings.
* [Clasp](https://drmeister.wordpress.com/2014/09/18/announcing-clasp/) Common Lisp compiler atop LLVM.
* [How to compile lambda](http://matt.might.net/articles/closure-conversion/)
* [Scheme compiler workshop](http://www.cs.indiana.edu/eip/compile/)
* [Small self-hosting scheme compiler](https://github.com/darius/ichbins)
* [Introduction to Scheme and its Implementation](ftp://ftp.cs.utexas.edu/pub/garbage/cs345/schintro-v14/schintro_toc.html)
* [Make a Lisp](https://github.com/kanaka/mal/blob/master/process/guide.md) Interpreter for a Lisp in 20+ different languages, guide to making an interpreter.
* [OCaml JIT compilation with LLVM](http://brierwoodapps.com/writing-a-jit-compiler-part-1/) first of a 3-part series on using OCaml bindings for LLVM to JIT compile things; [git repo](https://github.com/shawnhyam/son-of-blub)
* [Types and Programming Languages](http://www.cis.upenn.edu/~bcpierce/tapl/) OCaml code for Pierce's Types and Programming Languages book
* [Lexing and Parsing in OCaml](https://realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html)
