
# SLITE

![Screenshot of Slite in Action](https://tdrhq.com/slite-screenshot.png)

Slite stands for SLIme TEst runner. Slite interactively runs your
Common Lisp tests (currently only FiveAM supported). It allows you to
see the summary of test failures, jump to test definitions, rerun
tests with debugger all from inside Emacs.

Slite allows for fast Test Driven Development with CL.

At the moment, treat this software as Alpha quality. Things will
break, APIs will change, but your feedback will be very valuable.

## Installation

I'll expand on this later. Basically you need to `(asdf:load-system
:slite)`, and also need to load `slite.el` into emacs. That should be
all you need to do.

## Run all you FiveAM tests

This is the function you'll call most often. In a Common Lisp buffer,
press `C-c v` or `M-x slite-run`. You can then provide the expression
as `(slite:run-all-fiveam-tests)`. You could run a specific test suite
with `(fiveam:run test-suite-name)`.

You'll get a table of all the tests and their results. The failed
tests will be on the top.

Press `RET` on a test to see details about the failure. On the table,
or in the details buffer, press `r` to re-run the test with debugger
(i.e. when the test fails the debugger will open up).

Press `M-.` on the test name in the details view to jump to the
test. (This *only* works on Lispworks, and *only* if you use a
[patched](https://github.com/tdrhq/fiveam) version of FiveAM)

Honestly, that's about all you need to know. Although, there's one
more neat little feature:


## Running shell commands when tests pass

When I do TDD, I usually run my tests and immediately if the test
passes I run `git commit -a -m ...`. So my Emacs compile command might
look like `make tests && git commit ... `.

There are two ways to do this with slite: you could do this as part of
the lisp expression that you pass to slite-run, or you could use the
`slite-success-shell-hook` from the Emacs side. The latter option will
work even when you're using a remote slime session.

For example, `(setq slite-success-shell-hook "cd ~/code && git commit
-a -m ...")` will do essentially what I was suggesting earlier. This
API is a little awkward, so we might change this in the future

# Authors

Built by Arnold Noronha <arnold@tdrhq.com>
