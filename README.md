Stitch
======

Stitch is a full implementation of the simply typed lambda-calculus. The main
`stitch` executable is a REPL, allowing the user to write lambda-expressions and
evaluating them. Run `:help` at the prompt to see other commands.

Under the hood, Stitch is implemented using a type-indexed abstract syntax tree,
meaning that only well-typed Stitch terms can be built internally. This feat
requires extensive use of modern GHC/Haskell features.

Copyright
---------

As per the licence, all code here is Richard Eisenberg's. It's publicly
available [here](https://cs.brynmawr.edu/~rae/papers/2018/stitch/stitch.tar.gz)
but not available on Github and I thought it was really cool so I put it
up.
