:- use_module(library(dcgs)).
:- use_module(library(pio)).
:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(library(format)).
:- use_module('shared.pl').

% Grammar
sensor_beacons([])          --> [].
sensor_beacons([S-B|SBs])   --> sensor_beacon(S, B), "\n", sensor_beacons(SBs).
sensor_beacon(SX-SY, BX-BY) -->
    "Sensor at x=", integer(SX), ", y=", integer(SY),
    ": closest beacon is at x=", integer(BX), ", y=", integer(BY).

manhattan_distance(X0-Y0, X1-Y1, D) :-
    D #= abs(X0 - X1) + abs(Y0 - Y1).

position_in_range(P, S-B) :-
    manhattan_distance(S, B, D0),
    manhattan_distance(S, P, D1),
    D1 #=< D0.

fd_union(A, B, A \/ B).

run :-
    read_term(InputPath, []),
    phrase_from_file(sensor_beacons(SBs), InputPath),
    Y = 2000000,

    % Calculate number of positions in range of any of the sensors
    findall(Dom, (
        member(SB, SBs),
        position_in_range(X-Y, SB),
        fd_dom(X, Dom)
    ), Doms),
    foldl(fd_union, Doms, 1..0, Dom),
    X in Dom,
    fd_size(X, NPositions),

    % Calculate number of beacons on that row
    setof(X, member(_-(X-Y), SBs), Xs),
    length(Xs, NBeacons),

    Answer #= NPositions - NBeacons,
    portray_clause(Answer),

    halt.
