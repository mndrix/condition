:- use_module(library(condition)).

% helper predicates
restarter_foo(foo, bar).
restarter_oops(oops(Y), Y).

a1(X) :-
    b(X).

a2(X) :-
    handle(b(X), restarter_foo).

a3(X) :-
    handle(b2(X), restarter_oops).

b(X) :-
    handle(c(X), restarter_oops).

b2(X) :-
    handle(c(X), oops(_), shortcut).

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

'no handlers present'(throws(oops(ok))) :-
    c(_).

'no handlers present: default' :-
    Condition = obscure,
    signal(Condition,default,Restart),
    Restart == default.

'no handlers present: var' :-
    signal(obscure,Restart,Restart),
    var(Restart).

'using handle/3 variant' :-
    b2(X),
    X == [shortcut].

'multiple handlers match' :-
    a3(X),
    X == [shortcut, ok].  % innermost first
