# Parser Combinators in C++

Source code accompanying the presentation which you can [find here](https://slides.com/qqwy/parser-combinators-in-c/).
This presentation was originally given at 2021-03-30 during the final lecture of the [University of Groningen's C++ course](http://www.icce.rug.nl/edu/).

Run `make` in either of the two folders (`simple` or `optimized`). The source code requires C++20 (at least the `optimized` version does, compiler bounds for the `simple` version are probably more lenient).

Each of the two implementations currently contain all code in a single file. This is not very clean, but hopefully makes tinkering easier.

Code is heavily documented. Enjoy :-).


~Wiebe-Marten Wijnja

## References

If you want to read more about this subject, here are a couple resources:

### Historic:

- [Parsing: a timeline](https://jeffreykegler.github.io/personal/timeline_v3): A semi-complete (the last ~11 years are missing, and the author might be slightly biased) history of grammars and parsing techniques. Contains a _lot_ more information about different kinds of parsing, and how they were introduced/used/superseded during the last 100 years.

### Practical:
- [C++'s Boost.Spirit v2 guide](https://www.boost.org/doc/libs/1_75_0/libs/spirit/doc/html/index.html), documentation and getting-started guide for the current LTS release of the Boost.Spirit combinator-based parser/lexer/prettyprinter (resp. named 'Spirit.Qi', 'Spirit.Lex', and 'Spirit.Karma').
- [C++'s Boost.Spirit X3 guide](https://www.boost.org/doc/libs/1_75_0/libs/spirit/doc/x3/html/index.html), documentation and getting-started guide for the next release of Boost.Spirit, which is optimized in many ways because it only targets C++14 and beyond. However, it's not as feature-complete as v2 at this time (or at least the guides are not).
- [C++'s PEGTL](https://github.com/taocpp/PEGTL) is an alternative C++ parser combinator library, whose syntax uses structs inheriting from templates, rather than using operator overloading. Also have very good documentation.
- [C++'s Lexy](https://github.com/foonathan/lexy) another C++-based parser combinator library with slightly different design choices, and also a nice 'getting started' tutorial.
- [Rust's Nom](https://crates.io/crates/nom) library page. A 'zero-copy' parser combinator focused mainly on parsing binary formats. Might give a nice insight in how parser combinators might be written/used in other systems languages.
- [Haskell's Parsec](https://hackage.haskell.org/package/parsec) was the first, original, implementation based on the theoretical papers, with top-notch documentation. Haskell nowadays has many 'spin-off'-libraries that to the user work exactly like Parsec but behind the scenes try to optimize for different use-cases. Parsec's source-code itself remains the most readable of the bunch. A lot of the code I wrote for the presentation is based on Parsec's examples and source code.

### Theoretical:

- [Monadic Parser Combinators](https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf) A long technical report about the Monadic-style (the original approach which is more flexible but less optimizable) of Combinator-based parsing.
- [Generalized Parser Combinators](https://dinhe.net/~aredridel/.notmine/PDFs/Parsing/SPIEWAK%2C%20Daniel%20%282010%29%20-%20Generalized%20Parser%20Combinators.pdf) A paper on writing Parser Combinators outputting a GLL-parser (that can thus accept ambiguous grammars and deal with left recursion).
- [Invertible Syntax Descriptions: Unifying Parsing and Pretty Printing](https://www.mathematik.uni-marburg.de/~rendel/rendel10invertible.pdf) talks about using the same EDSL to create a parser-prettyprinter at the same time. This technique is already used widely in some Haskell web-frameworks to allow type-safe URL generation/recognition from a single URL description.
