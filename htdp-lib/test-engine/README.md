# Test Engine

The test engine is a small library for writing automated tests, mainly
to support check-expect & friends in the HtDP and DeinProgramm
teaching languages.

## Files in this directory

- `info.rkt`: package information
- `test-engine.rkt`: register tests, run them, collect & inspect results
- `racket-tests.rkt`: surface syntax for the teaching languages
- `syntax.rkt`: utilities for defining surface syntax in a way that
  works with the stepper
- `markup-gui.rkt`: graphical rendering for markup in a `text%` object
- `test-markup.rkt`: convert test results into markup
- `srcloc.rkt`: extract source location from the stack trace in an exception
- `test-display-gui.rkt`: separate window/panel for test results in DrRacket
- `test-tool.rkt`: hook test engine into DrRacket

## How it all works together

In typical usage, `test-engine/racket-tests` provides surface syntax
such as `check-expect`.  These call into `test-engine/test-engine` to
register the tests and record the results. The
`test-engine/test-engine` module has an abstract representation
for failed checks: `test-engine/racket-tests` generates that
representation.

For rendering, the test engine uses a two-stage process: First,
`test-engine/test-markup` converts the test results into *markup* from
the `simple-tree-text-markup` package.

The teaching languages use `test-engine/markup-gui` to render in
DrRacket.
