:- module(condition, [ handle/2
                     , handle/3
                     , signal/2
                     ]).
:- use_module(library(lambda)).

%%  signal(+Condition, -Restart) is nondet.
%
%   Signal a Condition and allow handlers to bind Restart.
%   This predicate is the mechanism
%   by which a piece of code indicates that it doesn't know how to
%   proceed and is requesting assistance in choosing a path forward.
%
%   It's possible for ancestors to disagree about Restart (aka,
%   different values on backtracking). In this scenario, it's acceptable
%   to choose the first Restart, iterate each Restart in turn or
%   consider all Restart values simultaneously (quorum?).
%   It's also possible for no ancestor to have an opinion
%   (aka, failing without any solutions).
%
%   It's quite common for signal/2 to leave dangling, `false`
%   choicepoints. If you're only interested in the first Restart value,
%   use once/1 or a similar construct to explicitly state that intent.
:- thread_local signal/2.
% handle_condition/{2,3} adds clauses to this predicate dynamically


%%  handle(:Goal, +Condition, +Restart)
%
%   Convenience on top of handle/2. Behaves exactly like handle/2 but
%   builds a restarter which only uses Restart if the condition unifies
%   with Condition. It addresses the common situation where one knows
%   both Condition and Restart before calling Goal.
%
%   For example,
%
%       handle(stuff, oops(_), carry_on)
:- meta_predicate condition:handle(0,?,?).
handle(Goal, Condition, Restart) :-
    handle(Goal, \C^R^(C=Condition -> R=Restart)).


%%  handle(:Goal, +Restarter)
%
%   Handles a condition signaled by Goal. Goal is called as with
%   call/1. If Goal, or a child goal thereof, signals a condition
%   (via signal/2) execute `call(Restarter, Condition, Restart)` to
%   determine which Restart value should be sent to the signaler.
%   Restarter may fail which allows other handlers higher up the call
%   stack to address this condition. Restarter may produce multiple
%   Restart values on backtracking, if multiple restarts are plausible.
%
%   When using handle/2, consult the documentation of those predicates
%   that might signal conditions. They'll explain which Restart values
%   are acceptable, what they do and whether generating Restart values
%   on backtracking makes sense.
%
%   If more than one handle/2 is in effect within the current call
%   stack, Restarter values are executed from the innermost to
%   the outermost ancestor. This allows those "closest" to the signal a
%   chance to handle it before it propagates outward.  Of course, if
%   a signaler looks at multiple solutions, other handlers will be
%   executed too.
:- meta_predicate condition:handle(0,2).
handle(Goal, Restarter) :-
    setup_call_cleanup(
        condition:asserta((signal(C,R) :- call(Restarter,C,R)), Ref),
        Goal,
        erase(Ref)
    ).
