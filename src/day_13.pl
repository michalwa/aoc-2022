:- use_module(library(dcgs)).
:- use_module(library(pio)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module('shared.pl').

% Grammar
pairs([P])      --> pair(P).
pairs([P|Ps])   --> pair(P), "\n", pairs(Ps).
pair(A-B)       --> packet(A), "\n", packet(B), "\n".
packets([P])    --> packet(P).
packets([P|Ps]) --> packet(P), ",", packets(Ps).
packet([])      --> "[]".
packet(Ps)      --> "[", packets(Ps), "]".
packet(I)       --> integer(I).

order(A, B) :- number(A), number(B), A < B.
order([], [_|_]).
order([A|As], [B|Bs]) :- order(A, B) ; A = B, order(As, Bs).
order(A, Bs) :- number(A), length(Bs, _), order([A], Bs).
order(As, B) :- length(As, _), number(B), order(As, [B]).

sort_by(_, [], []).
sort_by(_, [P], [P]).
sort_by(Pred, [P|Ps], Ls) :-
    findall(Q, ( member(Q, Ps), call(Pred, Q, P) ), Qs),
    findall(R, ( member(R, Ps), \+ call(Pred, R, P) ), Rs),
    sort_by(Pred, Qs, Qs1),
    sort_by(Pred, Rs, Rs1),
    append(Qs1, [P|Rs1], Ls).

collect_pairs([], []).
collect_pairs([A-B|Ps], [A,B|Xs]) :- collect_pairs(Ps, Xs).

run :-
    read_term(InputPath, []),
    phrase_from_file(pairs(Ps), InputPath),

    % Part 1
    % findall(I, ( nth1(I, Ps, A-B), order(A, B) ), Is),
    % sum_list(Is, Sum),
    % portray_clause(Sum),

    % Part 2
    collect_pairs(Ps, Xs),
    sort_by(order, [[[2]],[[6]]|Xs], Xs1),
    nth1(I, Xs1, [[2]]),
    nth1(J, Xs1, [[6]]),
    Solution is I * J,
    portray_clause(Solution),

    halt.
