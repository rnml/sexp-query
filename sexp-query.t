This file tests sexp-query.exe, a command line tool for pulling out
bits of an s-expression.

  $ function sexp-query { $TESTDIR/sexp-query.exe "$@"; }

The "(index N)" query selects the Nth element out of a list.  The
valid indices for a list of length N are {0..N}.

  $ echo '(a b c)' | sexp-query '(index 0)'
  a

  $ echo '(a b c)' | sexp-query '(index 1)'
  b

  $ echo '(a b c)' | sexp-query '(index 2)'
  c

Anything past that fails.

  $ echo '(a b c)' | sexp-query '(index 3)'

The "each" query pulls out each element in a list.

  $ echo '(a b c)' | sexp-query 'each'
  a
  b
  c

If there are multiple input sexps, sexp-query operates on each of them
in turn.

  $ echo '(a b c) (1 2 3)' | sexp-query '(index 0)'
  a
  1

  $ echo '(a b c) (1 2)' | sexp-query '(index 1)'
  b
  2

  $ echo '(a b c) (1 2)' | sexp-query '(index 2)'
  c

  $ echo '(a b c) (1 2)' | sexp-query 'each'
  a
  b
  c
  1
  2

One can pipe the output of query Q1 into query Q2 by using a "pipe"
query "(pipe Q1 Q2)".  Let's run on a nested list to see how this works

  $ echo '((a b c d) (1 2 3) (hello world))' \
  >   | sexp-query '(pipe (index 0) (index 1))'
  b

  $ echo '((a b c) (1 2 3) (hello world))' \
  >   | sexp-query '(pipe (index 1) (index 2))'
  3
