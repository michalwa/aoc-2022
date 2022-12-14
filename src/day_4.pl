% scryer-prolog -g run

:- use_module(library(dcgs)).
:- use_module(library(pio)).
:- use_module(library(lists)).
:- use_module(library(dif)).
:- use_module('shared.pl').

range_pairs([])      --> [].
range_pairs([P|Ps])  --> range_pair(P), "\n", range_pairs(Ps).
range_pair(A-B)      --> range(A), ",", range(B).
range(S-E)           --> integer(S), "-", integer(E).

range_subrange(As-Ae, Bs-Be) :- As =< Bs, Be =< Ae.
ranges_overlap(As-Ae, Bs-Be) :- As =< Be, Ae >= Bs.

run :-
    read_term(InputPath, []),
    phrase_from_file(range_pairs(Ps), InputPath),
    findall(A-B, (
        member(A-B, Ps),

        % Part 1
        ( range_subrange(A, B)
        % We have to assert dif(A, B) here, because otherwise the second goal
        % of the disjuction counts as a duplicate solution if `A = B`
        ; dif(A, B), range_subrange(B, A) )

        % Part 2
        % ranges_overlap(A, B)
    ), Rs),
    length(Rs, Count),
    write(Count), nl,
    halt.
