:- use_module(library(condition)).

% helper predicates
restarter_foo(foo, bar).
restarter_oops(oops(Y), Y).

a1(X) :-
    b(X).

a2(X) :-
    handle(b(X), restarter_foo).

b(X) :-
    handle(c(X), restarter_oops).

c(Restarts) :-
    findall( Restart
           , signal(oops(ok), Restart)
           , Restarts
           ).



:- use_module(library(tap)).

% add tests showing common usage
'det handler in parent' :-
    a1(X),
    X == [ok].

'det handler in parent, grandparent handler ignored' :-
    a2(X),
    X == [ok].

'no handlers present' :-
    c(X),
    X == [].
