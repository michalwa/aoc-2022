% scryer-prolog -g run

:- use_module(library(dcgs)).
:- use_module(library(pio)).
:- use_module(library(format)).
:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module('shared.pl').

% Grammar
instructions([])     --> [].
instructions([I|Is]) --> instruction(I), "\n", instructions(Is).
instruction(noop)    --> "noop".
instruction(addx(A)) --> "addx ", integer(A).

cycle([noop|Is], state(X, C),   state(X, 0), Is) :- C #= 0.
cycle(Is0,       state(X0, C0), state(X, C), Is) :-
    Is0 = [addx(A)|Rest],
    ( C0 #= 1, X #= X0 + A, C #= 0,      Is = Rest
    ; C0 #= 0, X #= X0,     C #= C0 + 1, Is = Is0 ).

cycle_states(Is, Ss) :- cycle_states_(Is, state(1, 0), Ss).
cycle_states_([], S, [S]).
cycle_states_(Is0, S0, [S0|Ss]) :-
    cycle(Is0, S0, S, Is1),
    cycle_states_(Is1, S, Ss).

run :-
    read_term(InputPath, []),
    phrase_from_file(instructions(Is), InputPath),
    cycle_states(Is, Ss),
    % portray_clause(Ss),
    findall(Score, (
        N #= 20 + 40 * _,
        nth1(N, Ss, state(X, _)),
        Score #= N * X
        % format("~d: ~w", [N, Score]),
        % nl
    ), Scores),
    sum(Scores, #=, TotalScore),
    portray_clause(TotalScore),
    halt.
