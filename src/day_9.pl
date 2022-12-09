% scryer-prolog -g run

:- use_module(library(dcgs)).
:- use_module(library(clpz)).
:- use_module(library(pio)).
:- use_module(library(charsio)).
:- use_module(library(format)).
:- use_module(library(lists)).

% Grammar
instructions([])     --> [].
instructions([I|Is]) --> instruction(I), "\n", instructions(Is).
instruction(D-N)     --> [D], " ", integer(N).

integer(I)           --> digits(Ds), { number_chars(I, Ds) }.
digits([D|Ds])       --> digit(D), maybe_digits(Ds).
maybe_digits([])     --> [].
maybe_digits([D|Ds]) --> digit(D), maybe_digits(Ds).
digit(D)             --> [D], { char_type(D, decimal_digit) }.

% Logic
step(_-0, R, R, []). % for completeness

step('L'-1, (Hx0,Hy)-(Tx0,Ty0), P, [P]) :-
    P = (Hx,Hy)-(Tx,Ty),
    Hx #= Hx0 - 1,
    ( Tx0 - Hx #= 2,  Tx #= Hx + 1, Ty #= Hy
    ; Tx0 - Hx #\= 2, Tx #= Tx0,    Ty #= Ty0 ).

step('U'-1, (Hx,Hy0)-(Tx0,Ty0), P, [P]) :-
    P = (Hx,Hy)-(Tx,Ty),
    Hy #= Hy0 + 1,
    ( Hy - Ty0 #= 2,  Tx #= Hx,  Ty #= Hy - 1
    ; Hy - Ty0 #\= 2, Tx #= Tx0, Ty #= Ty0 ).

step('R'-1, (Hx0,Hy)-(Tx0,Ty0), P, [P]) :-
    P = (Hx,Hy)-(Tx,Ty),
    Hx #= Hx0 + 1,
    ( Hx - Tx0 #= 2,  Tx #= Hx - 1, Ty #= Hy
    ; Hx - Tx0 #\= 2, Tx #= Tx0,    Ty #= Ty0 ).

step('D'-1, (Hx,Hy0)-(Tx0,Ty0), P, [P]) :-
    P = (Hx,Hy)-(Tx,Ty),
    Hy #= Hy0 - 1,
    ( Ty0 - Hy #= 2,  Tx #= Hx,  Ty #= Hy + 1
    ; Ty0 - Hy #\= 2, Tx #= Tx0, Ty #= Ty0 ).

step(D-S, R0, R, [P|Ps]) :-
    S #> 1, S1 #= S - 1,
    step(D-1, R0, R1, [P]),
    step(D-S1, R1, R, Ps).

iterate_steps(Is, Ps) :- iterate_steps_(Is, (0,0)-(0,0), Ps).
iterate_steps_([], _, []).
iterate_steps_([I|Is], P0, Ps) :-
    step(I, P0, P, Ps1),
    iterate_steps_(Is, P, Ps2),
    append(Ps1, Ps2, Ps).

pair_second(_-X, X).

run :-
    phrase_from_file(instructions(Is), 'input/day_9/input.txt'),
    iterate_steps(Is, Ls),
    maplist(pair_second, Ls, Ts),
    list_to_set(Ts, UTs),
    length(UTs, N),
    portray_clause(N),
    halt.
