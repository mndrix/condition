# Synopsis

    :- use_module(library(condition)).
    main :-
        handle(stuff, oops, warn).
    stuff :-
        writeln('Doing something useful...'),
        % oh no, a problem!
        signal(oops, Restart),
        ( Restart = ignore ->
            true
        ; Restart = warn ->
            print_message(warning, oops)
        ; Restart = error ->
            print_message(error, oops)
        ; % otherwise ->
            type_error(oneof([ignore,warn,error]), Restart)
        ).

# Description

A [condition system](http://www.nhplace.com/kent/Papers/Condition-Handling-2001.html) is a mechanism for working with software errors.  The idea was popularized by Common Lisp.  Here's an analogy, modified from that Kent Pitman link, explaining roughly how it works:

Think of the process of signaling and handling as analogous to finding a fork in a road that you do not commonly travel.  You don't know which way to go, so you make known your dilemma (signal a condition).  Various sources of wisdom (handlers) present themselves, and you consult each, placing your trust in them because you have no special knowledge yourself of what to do.  Not all sources of wisdom are experts on every topic, so some may decline to help, some may disagree.  Using those sources of wisdom, you act.  The situation has been handled.

In the following description, condition system terminology is *highlighted*.

When a predicate encounters a problematic situation and doesn't know how to proceed, it can *signal* a *condition* (instead of throwing an exception).  A *handler* higher up in the call stack can respond to this condition with a *restart*.  Unlike with exceptions, the call stack is never unwound so the precise context of the error is preserved in case it's needed for continuing the computation.  A condition communicates information from a signaler to a handler.  Based on this information, the handler sends a restart which communicates in the opposite direction.

In the Synopsis above, `stuff/0` encounters a problem.  It indicates this problem by sending the condition `oops/0`, which could have been any term.  Fortunately, `main/0` has a handler for that condition.  In this case, it responds by sending a `warn/0` restart.  Again, the restart could have been any term.  `stuff/0` continues based on the restart value.

It's good practice to document which conditions a predicate might signal as well as the restarts that it understands.  For publicly accessible predicates, conditions and restarts should be considered part of the API.

This library departs from Common Lisp by allowing all handlers a chance to respond to a condition, not just the innermost, matching handler.  The innermost handler gets the first attempt, but on backtracking `signal/2` iterates all restarts.  One could act on just the first, the most popular, try them all, etc.

# Examples

The first time one hears about a condition system, it's not apparent how it might be used.  Here are some examples to help.

  * A filesystem operation might signal when a disk runs out of space.
    Upon receiving this condition, a handler might delete some large
    temporary files and send a restart to try again.
  * A CSV parsing predicate might signal when it encounters an invalid
    line or column.  A handler might choose to correct or skip the offending
    content.
  * An HTTP library might signal when it encounters a 500 error or
    network timeout.  A handler might choose restarts based on an
    exponential backoff algorithm.

Of course, there are dozens of ways to address these same use cases without a condition system.  They often involve passing configuration values or callbacks or using `multifile` predicates.

A condition system decouples all participating software components by agreeing on a protocol by which they may communicate.  Configuration and callbacks don't have to be passed down the call stack through arbitrary intermediaries.  It doesn't have to rely on an HTTP library, for example, to implement the exact backoff strategy that's needed.  A library provides some primitive conditions and restarts.  Its users decide how to compose those as they see fit.

# Changes in this Version

  * First public release

# Installation

Using SWI-Prolog 6.3 or later:

    ?- pack_install(condition).

This module uses [semantic versioning](http://semver.org/).

Source code available and pull requests accepted at
http://github.com/mndrix/condition

@author Michael Hendricks <michael@ndrix.org>
@license BSD
