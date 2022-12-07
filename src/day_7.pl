% scryer-prolog -g run

:- use_module(library(dcgs)).
:- use_module(library(charsio)).
:- use_module(library(pio)).
:- use_module(library(lists)).
:- use_module(library(format)).
:- use_module(library(clpz)).

include(Goal, Ls0, Ls) :-
    include_(Ls0, Goal, Ls).

include_([], _, []).
include_([L|Ls0], Goal, Ls) :-
    ( call(Goal, L) ->
      Ls = [L|Rest]
    ; Ls = Rest ),
    include_(Ls0, Goal, Rest).

% DCG Non-terminals

input_lines([])             --> [].
input_lines([L|Ls])         --> input_line(L), "\n", input_lines(Ls).
input_line(cd(D))           --> "$ cd ", filename(D).
input_line(ls)              --> "$ ls".
input_line(dir(D))          --> "dir ", filename(D).
input_line(file_size(F, S)) --> integer(S), " ", filename(F).

filename([C|Cs])            --> filename_char(C), maybe_filename(Cs).
maybe_filename([])          --> [].
maybe_filename([C|Cs])      --> filename_char(C), maybe_filename(Cs).
filename_char(C)            --> [C], { member(C, "./") ; char_type(C, alphanumeric) }.

integer(I)                  --> digits(Ds), { number_chars(I, Ds) }.
digits([D|Ds])              --> digit(D), maybe_digits(Ds).
maybe_digits([])            --> [].
maybe_digits([D|Ds])        --> digit(D), maybe_digits(Ds).
digit(D)                    --> [D], { char_type(D, decimal_digit) }.

% Problem-specific logic

command_extend_tree(cd(D), tree_cd(T, Cd0), tree_cd(T, Cd)) :-
      D = "/"  -> Cd = []
    ; D = ".." -> append(Cd, [_], Cd0)
    ;             append(Cd0, [D], Cd).
command_extend_tree(ls, T, T).
command_extend_tree(dir(D), tree_cd(T0, Cd), tree_cd(T, Cd)) :-
    tree_path_insert(Cd, dir_files(D, []), T0, T).
command_extend_tree(file_size(F, S), tree_cd(T0, Cd), tree_cd(T, Cd)) :-
    tree_path_insert(Cd, file_size(F, S), T0, T).

tree_path_insert([], F, dir_files(D, Df), dir_files(D, [F|Df])).
tree_path_insert([P|Ps], F, dir_files(D, Df0), dir_files(D, Df)) :-
    select(Nested0, Df0, Df1),
    Nested0 = dir_files(P, _),
    tree_path_insert(Ps, F, Nested0, Nested),
    select(Nested, Df, Df1).

dir_sizes(file_size(_, _), []).
dir_sizes(dir_files(_, []), [0]).
dir_sizes(DF, [S|Ss]) :-
    DF = dir_files(_, Fs),
    dir_size(DF, S),
    maplist(dir_sizes, Fs, SLists),
    append(SLists, Ss).

dir_size(file_size(_, S), S).
dir_size(dir_files(_, Fs), S) :-
    maplist(dir_size, Fs, Ss),
    sum(Ss, #=, S).

at_most_100000(S) :- S #=< 100000.

run :-
    phrase_from_file(input_lines(Ls), 'input/day_7/input.txt'),
    T0 = tree_cd(dir_files("/", []), []),
    foldl(command_extend_tree, Ls, T0, tree_cd(T, _)),
    dir_sizes(T, Ss),
    include(at_most_100000, Ss, Ss1),
    sum(Ss1, #=, S),
    portray_clause(S),
    halt.
