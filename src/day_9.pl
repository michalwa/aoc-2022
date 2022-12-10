% scryer-prolog -g run

:- use_module(library(dcgs)).
:- use_module(library(clpz)).
:- use_module(library(pio)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module('shared.pl').

% Grammar
instructions([])     --> [].
instructions([I|Is]) --> instruction(I), "\n", instructions(Is).
instruction(D-N)     --> [D], " ", integer(N).

% Logic
step(_-0, R, R, []). % for completeness

step('L'-1, [(Hx0,Hy)|Ts0], R, [R]) :-
    R = [(Hx,Hy)|Ts],
    Hx #= Hx0 - 1,
    iterate_follow((Hx,Hy), Ts0, Ts).

step('U'-1, [(Hx,Hy0)|Ts0], R, [R]) :-
    R = [(Hx,Hy)|Ts],
    Hy #= Hy0 + 1,
    iterate_follow((Hx,Hy), Ts0, Ts).

step('R'-1, [(Hx0,Hy)|Ts0], R, [R]) :-
    R = [(Hx,Hy)|Ts],
    Hx #= Hx0 + 1,
    iterate_follow((Hx,Hy), Ts0, Ts).

step('D'-1, [(Hx,Hy0)|Ts0], R, [R]) :-
    R = [(Hx,Hy)|Ts],
    Hy #= Hy0 - 1,
    iterate_follow((Hx,Hy), Ts0, Ts).

step(D-S, R0, R, [P|Rs]) :-
    S #> 1, S1 #= S - 1,
    step(D-1, R0, R1, [P]),
    step(D-S1, R1, R, Rs).

% FIXME: Incredibly slow
follow((Hx, Hy), (Tx0, Ty0), (Tx, Ty)) :-
    Dx #= Hx - Tx0,
    Dy #= Hy - Ty0,
    ( [Dx, Dy] ins -1'..'1,            Tx #= Tx0, Ty #= Ty0
    ; ( abs(Dx) #= 2 ; abs(Dy) #= 2 ), Tx #= Tx0 + sign(Dx), Ty #= Ty0 + sign(Dy) ).

iterate_follow(_, [], []).
iterate_follow(H, [T0|Ts0], [T|Ts]) :-
    follow(H, T0, T),
    iterate_follow(T, Ts0, Ts).

simulation_steps(RopeLength, Is, Rs) :-
    R0 = [(0,0)|_],
    same(R0),
    length(R0, RopeLength),
    simulation_steps_(Is, R0, Rs).

simulation_steps_([], _, []).
simulation_steps_([I|Is], R0, Rs) :-
    step(I, R0, R, Rs1),
    simulation_steps_(Is, R, Rs2),
    append(Rs1, Rs2, Rs).

same([]).
same([_]).
same([X,X|Xs]) :- same([X|Xs]).

last([X], X).
last([_|Xs], X) :- last(Xs, X).

run :-
    read_term(InputPath, []),
    phrase_from_file(instructions(Is), InputPath),
    % Part 1: simulation_steps(2, Is, Rs),
    simulation_steps(10, Is, Rs),
    maplist(last, Rs, Ts),
    list_to_set(Ts, UTs),
    length(UTs, N),
    portray_clause(N),
    halt.
