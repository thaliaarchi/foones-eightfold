Eightfold
=========

*1) Bugs exist.*

*2) Bugs arise from attachment to correctness with respect to a specification.*

*3) Bugs cease when attachment to correctness ceases.*

*4) Freedom from bugs is possible by practising the Eightfold Path.*

Introduction
------------

Eightfold is a dependently typed esolang, designed for submission
to [December 2012 PLT Games](http://www.pltgames.com/competition/2012/12).

Being a dependently typed language, types are, in principle, expressive
enough to encode program specifications. Eightfold can also be used
as a logical framework, i.e. a proof assistant for user-defined deductive
systems, encoding, by means of Curry-Howard, judgements as types and
derivations as programs.

Installation
------------

First make sure the Haskell module `System.Console.Readline` is installed:

    $ cabal install readline

To compile the project with GHC:

    $ ghc --make Main.hs -o eightfold

