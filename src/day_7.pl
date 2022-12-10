% scryer-prolog -g run

:- use_module(library(dcgs)).
:- use_module(library(charsio)).
:- use_module(library(pio)).
:- use_module(library(lists)).
:- use_module(library(format)).
:- use_module(library(clpz)).

% Relates `Ls0` to those of its elements `Ls` which satisfy `Goal`.
% Directly copied from the source of library(clpz), sadly it's not exported
include(Goal, Ls0, Ls) :-
    include_(Ls0, Goal, Ls).

include_([], _, []).
include_([L|Ls0], Goal, Ls) :-
    ( call(Goal, L) ->
      Ls = [L|Rest]
    ; Ls = Rest ),
    include_(Ls0, Goal, Rest).

% Grammar

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

% mutate_tree(TerminalLine, TreeWd0, TreeWd)
%
% Relates a current state of a file tree and working directory `TreeCd0`
% to the next state `TreeWd`, extended by the information given by `TerminalLine`.
%
% `TreeWd0` and `TreeWd` are of the form `tree_wd(Tree, Path)`, where `Path`
% is a list of descending directory names.
mutate_tree(cd(D), tree_wd(T, Cd0), tree_wd(T, Cd)) :-
      D = "/"  -> Cd = []
    ; D = ".." -> append(Cd, [_], Cd0)
    ;             append(Cd0, [D], Cd).
mutate_tree(ls, T, T).
mutate_tree(dir(D), tree_wd(T0, Cd), tree_wd(T, Cd)) :-
    tree_path_insert(Cd, dir_files(D, []), T0, T).
mutate_tree(file_size(F, S), tree_wd(T0, Cd), tree_wd(T, Cd)) :-
    tree_path_insert(Cd, file_size(F, S), T0, T).

% tree_path_insert(Path, File, Tree0, Tree)
%
% Relates a file tree `Tree0` to a tree `Tree` which is `Tree0` after inserting
% the given `File` at `Path`.
%
% `Path` is a list of descending directory names.`
% `Tree0` and `Tree` are of the form `dir_files(DirName, Files)`, where `Files` is a list of
% nested `dir_files/2` terms.
tree_path_insert([], F, dir_files(D, Df), dir_files(D, [F|Df])).
tree_path_insert([P|Ps], F, dir_files(D, Df0), dir_files(D, Df)) :-
    select(Nested0, Df0, Df1),
    Nested0 = dir_files(P, _),
    tree_path_insert(Ps, F, Nested0, Nested),
    select(Nested, Df, Df1).

% dir_sizes(Tree, Sizes)
%
% Relates a file tree `Tree` to a list of integers `Sizes` representing the size
% of each descendant directory, including the top-most.
dir_sizes(file_size(_, _), []).
dir_sizes(dir_files(_, []), [0]).
dir_sizes(DF, [S|Ss]) :-
    DF = dir_files(_, Fs),
    dir_size(DF, S),
    maplist(dir_sizes, Fs, SLists),
    append(SLists, Ss).

% dir_size(DirOrFile, Size)
%
% Relates a directory or file (represented by `dir_files/2` or `file_size/2` respectively)
% to its total size. The total size is the sum of the sizes of all descendant files.
dir_size(file_size(_, S), S).
dir_size(dir_files(_, Fs), S) :-
    maplist(dir_size, Fs, Ss),
    sum(Ss, #=, S).

at_most(N, S) :- S #=< N.
at_least(N, S) :- S #>= N.

run :-
    read_term(InputPath, []),
    phrase_from_file(input_lines(Ls), InputPath),
    T0 = tree_wd(dir_files("/", []), []),
    foldl(mutate_tree, Ls, T0, tree_wd(T, _)),
    dir_sizes(T, Ss),

    % Part 1
    % include(at_most(100000), Ss, Ss1),
    % sum(Ss1, #=, S),
    % portray_clause(S),

    % Part 2
    dir_size(T, TotalSize),
    FreeSpace #= 70000000 - TotalSize,
    SpaceNeeded #= 30000000 - FreeSpace,
    include(at_least(SpaceNeeded), Ss, Ss1),
    list_min(Ss1, Min),
    portray_clause(Min),

    halt.
