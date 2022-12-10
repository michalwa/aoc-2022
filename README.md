# Advent of Code 2022

To run a solution:
* Clone the repo
* Place input/example files under `input/<solution-name-without-extension>/<input-name>.txt`
* Run `cargo run -- <solution-file-name> <input-name>`

Currently native Rust modules and Prolog scripts are supported as solutions. The build script will generate logic for calling any `day_X.rs` or `day_X.pl` found in the `src/` directory.

Rust solutions must export a `solve` function:

```rs
pub fn solve(_: BufReader<File>) { /* ... */ }
```

Prolog solutions must define a predicate `run/0` and read the input file path from stdin:

```prolog
run :-
    read_term(InputPath, []),
    % ...
    halt.
```

By default, Prolog scripts are run with [`scryer-prolog`](https://github.com/mthom/scryer-prolog), but this can be overriden by setting `PROLOG` to a path to an interpreter.
