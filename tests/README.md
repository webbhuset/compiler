# Tests

Each directory here holds one test script, `run.mjs`, which compiles small
generated Elm projects with the compiler it is given and checks the result:
an error report, the compiled program's output, or memory use.

Run them all with

    make test

which builds the compiler and runs every `tests/*/run.mjs` against `./whelm`.
To run one against any binary:

    node tests/<name>/run.mjs <path to elm binary>

A script exits 0 when every case passes, and non-zero otherwise. Failing
cases keep their generated projects in a temporary directory and print
where.

To add a test, add a directory with a `run.mjs` following the same
convention; `make test` picks it up.

## Not the long-term setup

These Node scripts are a stopgap. They should be replaced with something
proper, probably tests written in Elm or Haskell, keeping JavaScript only
where a test needs it directly (for example to run the compiled program).
