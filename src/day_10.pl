% scryer-prolog -g run

% Post-submission optimizations inspired by solutions from fellow coders:
% * Instead of maintaining a separate cycle state for the `addx` instruction,
%   parse it as `noop` followed by `addx`, which then immediately changes X.

:- use_module(library(dcgs)).
:- use_module(library(pio)).
:- use_module(library(format)).
:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module('shared.pl').

% Grammar
instructions([])                --> [].
instructions([noop|Is])         --> "noop", "\n", instructions(Is).
instructions([noop,addx(A)|Is]) --> "addx ", integer(A), "\n", instructions(Is).

cycle(noop,    X,  X).
cycle(addx(A), X0, X) :- X #= X0 + A.

cycle_states(Is, Xs) :- cycle_states_(Is, 1, Xs).
cycle_states_([], X, [X]).
cycle_states_([I|Is], X0, [X0|Xs]) :-
    cycle(I, X0, X),
    cycle_states_(Is, X, Xs).

states_pixels(Xs, Ps) :- states_pixels_(Xs, 0, Ps).
states_pixels_([], _, []).
states_pixels_([X|Xs], C, [P|Ps]) :-
    H #= C rem 40,
    ( abs(X - H) #=< 1 -> P = '@' ; P = ' ' ),
    states_pixels_(Xs, C + 1, Ps).

pixels_rows(L, Ps, [Ps]) :- length(Ps, L1), L1 #=< L.
pixels_rows(L, Ps, [R|Rs]) :-
    append(R, Rest, Ps),
    length(R, L),
    pixels_rows(L, Rest, Rs).

run :-
    read_term(InputPath, []),
    phrase_from_file(instructions(Is), InputPath),
    cycle_states(Is, Xs),

    % Part 1
    findall(Score, (
        N #= 20 + 40 * _,
        nth1(N, Xs, X),
        Score #= N * X
    ), Scores),
    sum(Scores, #=, TotalScore),
    portray_clause(TotalScore),

    % Part 2
    states_pixels(Xs, Ps),
    pixels_rows(40, Ps, Rs),
    maplist(portray_clause, Rs),

    halt.
