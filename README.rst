compil.erl
=========

This is a student project from the fu berlin compiler construction course_ in summer 2012.

about
-----

Compil.erl is an erlang written compiler, that compiles a static, inferred and
strong typed lisp ddialect into textual llvm ir.

quickstart
----------

Clone and build, build all erlang files:

::

    git clone git://github.com/slomo/compil.git
    cd compil
    ./build.sh

Given valid source code is in file code.lisp run:

::

    ./compil.sh code.lisp | tee | lli

or for llvm compilation instead of interpretation:

::

    ./compil.sh code.lisp | llc | gcc -x assembler -

REMEMBER: upon excution there will be no output (check return type)

Tested on erlang R14B02 and llvm 2.9.

reading order
-------------

* `doc/presentation.pdf` are slides to a 15 min talk about the language
* `doc/language_reference.rst` explains the language

thanks
------

* rvirding for lfe_ (took yecc and xrl file/ generated code from there )
* alxanders gave me good advice with fix point operator

Yves Müller, July 2012

.. _lfe: https://github.com/rvirding/lfe/
.. _course: https://page.mi.fu-berlin.de/konzackma12
