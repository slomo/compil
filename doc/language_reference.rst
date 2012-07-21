Language Features
=================

This document intends to give a short overview, how to write programms that the
compiler may compile to llvm.

Introduction
------------


The compiler takes a language written in s-expr and transforms it to llvm ir
code. This language is heavily based on the typed lambda calculus.

Here is a short list of features:

* basic types int and bool
* basic operations on those types
* branching via cond
* bindings using let expression
* anonymous functions (lambdas)
* functions as values
* lexical scoped functions
* recursion via the fix point operator


Basic types and operations
---------------------------

As state above int and bool are supported as well as the following operations:

With signature `int x int -> int`, some basic number operators:

* ``+``
* ``-``
* ``*``
* ``/``

The above operators may also be used with the signature `int x ... x int -> int`

With signature `bool x bool -> bool`, the basic boolean operators:

* ``and``
* ``or``

Please consider that both sides are always evaluated, other than in common
programming languages. For non evaluation use ``cond``.

With signature `int x int -> bool`, the following comparison operators:

* ``>``
* ``<``
* ``=``

The literals for int are integer numbers (surprise, surprise). For bool can
``true`` and ``false`` be used.

Call operators or functions
---------------------------

To call anything that is callable with given parameters, use an s-expr list,
with the target as first element and the args following.

::

    ( target arg1 arg2 ... argn )

The spaces after ``(`` and before ``)`` are optional.

To wrap up the last two parts a few examples:

::

    ( + 3 2 1 ) ;; returns 6

    ( = ( + 3 2 ) ( - 5 0 ) ) ;; returns true

    ( or false false ) ;; returns false

In- and Output
--------------

Testing the last examples, one may ask how to get feedback, or how to output
expressions. Easy: you don't. There is no in/output handling (yet). But the
value of the last expression is returned as return value. Given you have the
program in the file prog.lisp and installed lli (llvm interpreter), just run:

::

    lli prog.lisp && echo $?

Conditional Execution
----------------------

As if-then-else in other programming languages, ``cond`` may be used to express
conditional execution. Cond takes a boolean expression and two other
expressions of the same type. If the first expression is true, it evaluates the
second, otherwise the third.

::

    ( cond ( = 3 3 )
        ( + 1 4 )
        ( + 0 0 )
    ) ;; returns 5

Different types in both branches do not work, because then the compiler
couldn't determine the type.

**ATTENTION**: Right now there is a quick fix in the llvm gen, that allows only
integer expressions. I will remove this if I have time.

Names bindings through let
--------------------------

The ``let`` expression allows to introduce new names into an expression. It
expects two arguments: a list of bindings and an expression. (It makes sense to
use the bindings in that expression.)

::

    ( let
        ((a 2) (b (+ 2 1)))     ;; list of bindings
        (+ a b)                 ;; expression
    ) ;; returns 5

In a binding definition no bindings from the outer let can be seen (not even
the definition itself). As names everything should be usable, I tested with
small letter words, that are not reserved.

Anonymous functions
-------------------

The ``lambda`` expression is used to create an lambda abstraction with multiple
parameters. First argument is a list of formal parameters. Second argument is
an expression, that will be evaluated upon execution.  The resulting value is
callable.

::

    (
        ( lambda (x) (+ x 2) )  ;; lambda abstraction
        3                       ;; argument it is called with
    ) ;; return 5

Functions as values (closures)
------------------------------

A lambda function may be used as a value, and be executed later.

::

    (                           ;; call to function that is returned
        (                       ;; call to function that returns function
            (lambda (y)         ;; function that returns a function
                (lambda (x)     ;; returned function
                    (+ y x)))
        2)                      ;; argument for x
    3 )                         ;; argument for y

The above example shows, that also lexical scoping is implemented.

**ATTENTION**: I had some cases where closures with more than one
argument (or binding ???) crashed the compiler.

Recursion with fix
------------------

Since functions can't be named, except for let bindings, in which the name can
not be used, a different approach to recursion was needed. Therefore the fix
point operator exists.

Fix takes a function as argument, and produces a callable. The function must be
special in some sense, that it takes two arguments, first a function f with the
same signature as itself, except that this first parameter is missing, and
second a value. It also must return a value of the same type as the second.

During the execution f is a reference to the function itself (actually to a
wrapper) and therefore may be used for recursive calls.

An example implementing the fibonacci function:

::

    (
        (fix (lambda (f a)
            (cond (< a 3)       ;; if a==1 or a==2
                1               ;; return 1
                (+
                    (f (- a 2)) ;; recurs
                    (f (- a 1))
                )
            )
        ))
        10                      ;; call fib(10)
    )  ;; returns 55


**NOTE**: It may be possible to define recursive functions with more than one
actual parameter, but as of now in my opinion this is not covered by the
mathematical fix point operator.

**ATTENTION**: Currently there is a quickfix in type detection and llvm
generation that allows only recursive functions with `int -> int` and crashes
if more than one fix expression exists.

**ATTENTION**: Assuming that the type detection for recursive function works as
planned, the compiler will not be able to figure out types for primitive
infinite recursive functions ( eg. `` f x = f x `` ).
