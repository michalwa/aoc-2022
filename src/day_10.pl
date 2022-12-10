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

states_pixels(Ss, Ps) :- states_pixels_(Ss, 0, Ps).
states_pixels_([], _, []).
states_pixels_([state(X, _)|Ss], C, [P|Ps]) :-
    H #= C rem 40,
    ( abs(X - H) #=< 1 -> P = '@' ; P = ' ' ),
    states_pixels_(Ss, C + 1, Ps).

pixels_rows(L, Ps, [Ps]) :- length(Ps, L1), L1 #=< L.
pixels_rows(L, Ps, [R|Rs]) :-
    append(R, Rest, Ps),
    length(R, L),
    pixels_rows(L, Rest, Rs).

run :-
    read_term(InputPath, []),
    phrase_from_file(instructions(Is), InputPath),
    cycle_states(Is, Ss),

    % Part 1
    findall(Score, (
        N #= 20 + 40 * _,
        nth1(N, Ss, state(X, _)),
        Score #= N * X
    ), Scores),
    sum(Scores, #=, TotalScore),
    portray_clause(TotalScore),

    % Part 2
    states_pixels(Ss, Ps),
    pixels_rows(40, Ps, Rs),
    maplist(portray_clause, Rs),

    halt.
