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

Overview
--------

Eightfold is a combination of a reduction and a type-checking
engine.
A program is a sequence of declarations and queries.

When Eightfold is started interactively, the following
read-eval-print-loop is displayed:

    $ ./eightfold
     ___ _      _   _    __     _    _
    | __(_)__ _| |_| |_ / _|___| |__| |
    | _|| / _` | ' \  _|  _/ _ \ / _` |
    |___|_\__, |_||_\__|_| \___/_\__,_|
          |___/
    Copyright (c) 2012 - Pablo Barenbaum <foones@gmail.com>
    Usage: eightfold [options]
    Options:
        file.8f     read the given input file
        -s          do not start toplevel interaction

    8f> 

The following is a very simple interaction:

    8f> t : *            # declare "t" as a new type
    t : *.

    8f> a : t            # declare "a" as a new constant of type "t"
    a : t.

    8f> ? a              # query : what is the type of "a"?
    ! a : t.             # answer: "a" is of type "t"

    8f> ? :xt.x          # query : what is the type of a function that takes
                         #         an "x" of type "t" and returns "x"
    ! :xt.x : >tt.       # answer: it has type ">tt", i.e. it takes
                         #         a term of type "t" and returns a term
                         #         of type "t"

    8f> ?? (:xt.x)a      # query : what is the result of applying the
                         #         identity to "a"
    !! (:xt.x)a : t = a. # answer: it has type "t" and evaluates to "a"

    8f> b : >tt          # declare "b" as a new constructor that
                         # takes a term of type "t" and returns
                         # a term of type "t"
    b : >tt.

    8f> ?? ba            # query : what is the result of applying such
                         #         constructor to a?
    !! ba : t = ba.      # answer: it has type "t" and evaluates to "ba"

    8f> ?? bb            # query : what is the result of applying "b"
                         #         to "b"
                         # (answer is the following error message)

    Error:
    in application bb -- argument has wrong type
    types do not match: t -- >tt in env {b : >tt ; a : t ; t : * ; * : *}

In Eightfold, the variable `*` is globally bound to the basic
kind, i.e. the kind of all data types. The type of `*` is `*`,
which presumably makes the system inconsistent (and thus uninteresting)
from a strictly logical point of view:

    8f> ? *
    ! * : *.

Syntax
------

### Syntax for terms

Eightfold terms are given by a syntax similar to that
of simply typed lambda calculus, with the difference that
type annotations do not belong to a separate syntactic
category, but are regular terms:

    [term] ::= [var]                       # variable
             | [term] [term]               # application
             | : [var] [term] . [term]     # abstraction

In terms of lexical syntax, a variable is either a lowercase character
followed by zero or more digits (`[a-z][0-9]*`),
a sequence of digits (`[0-9]+`),
a symbol (`[_+-*/]`), or an uppercase character followed by
lowercase characters and digits (`[A-Z][_a-z0-9]*`).
Notice that two uppercase characters
in the same identifier are not allowed, which is for `FooBar` to
be parsed as `Foo Bar`.

The associativity and precedence rules are those usual in
lambda-calculus (application is left-associative and has
higher precedence than abstraction). As usual,
free occurrences of `x` in `N` are bound in `:xM.N`.

For instance, `:XInt.ConsXNil` is an abstraction
that takes an `X` of type `Int` and applies `Cons` to
`X` and `Nil`. (This could, for example, be a function that
takes an integer and builds a singleton).

Beyond the syntax presented above, Eightfold allows
two abbreviations for denoting abstractions:

`> [term1] [term2]` is an abbreviation for
`: _ [term1] . [term2]`, where `_` is a variable that does
not occur free in `[term2]`. Thus, `>ta` is a function that
takes any term of type `t` and returns `a`.

`: [var1] [type1], [...] . [body]` is
an abbreviation for
`: [var1] [type1] . : [...] . [body]`.
Thus, `:XBool,YBool.AndXY` abbreviates
`:XBool.:YBool.AndXY`.

### Syntax for programs

An Eightfold program is a sequence of rules and queries,
terminated by periods.

By Curry-Howard, one can view an Eightfold script
either as a program or as a proof. Most language
constructs can be analyzed under the light of
two possible interpretations:

* Seen as a programming language, terms encode programs
  and types encode the usual data types. In this case
  the focus is usually centered in the dynamic semantics
  of the programs, i.e. one is interested in normalizing
  a term to know its normal form.

* Seen as a proof assistant, types encode propositions
  and terms encode proofs. Here, one is usually
  less interested in normalizing terms, and more
  interested in typechecking a term to ensure it effectively
  has a certain type.

Facts can be of three types: type declarations, value declarations or
type/value declarations.

Queries can be of two types: type queries or type/value queries.

#### Type declarations

Type declarations are of the form `[var] : [term]`.

Under the program interpretation, this declares a basic type,
type constructor, constant or constructor. For example:

    # Declare "Bool" as a basic type and "True" and "False"
    # as boolean constants:

    Bool : *.

    True : Bool.
    False : Bool.

    # Declare "List" as type constructor,
    # so that "List Bool" is a type.
    List : >**.

    # Declare "Nil" as a constructor that given a type "a"
    # returns a list of "a".
    # For instance "Nil Bool" denotes the empty list of booleans.
    Nil : :a*.List a.

    # Declare "Cons" as a function that given a type "a", an
    # element of type "a", and a list of "a" returns another
    # list of "a". For instance "Cons Bool True (Nil Bool)"
    # denotes the singleton list `[True]`.
    Cons : :a*.>a>(List a)(List a).

When this program is loaded, Eightfold checks that all
declarations are correct, and prints them out.
Declarations are read from top to bottom.
For a declaration to be correct, the right hand side,
i.e. the type, has to have a kind in the current context
(given by all previous declarations).

