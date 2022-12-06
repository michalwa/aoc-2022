% scryer-prolog -g run

:- use_module(library(dcgs)).
:- use_module(library(charsio)).
:- use_module(library(pio)).
:- use_module(library(lists)).
:- use_module(library(clpz)).

range_pairs([])     --> [].
range_pairs([P|Ps]) --> range_pair(P), "\n", range_pairs(Ps).

range_pair((As-Ae)-(Bs-Be)) --> range(As, Ae), ",", range(Bs, Be).

range(S, E) --> integer(S), "-", integer(E).

integer(I) --> digits(Ds), { number_chars(I, Ds) }.

digits([D|Ds])   --> digit(D), digits_r(Ds).
digits_r([D|Ds]) --> digit(D), digits_r(Ds).
digits_r([])     --> [].
digit(D)         --> [D], { char_type(D, decimal_digit) }.

% Part 2
ranges_overlap(As-Ae, Bs-Be) :- As #=< Be, Ae #>= Bs.

run :-
    phrase_from_file(range_pairs(Ps), 'input/day_4/input.txt'),
    findall((A-B), (member(A-B, Ps), ranges_overlap(A, B)), Rs),
    length(Rs, Count),
    write(Count), nl,
    halt.
