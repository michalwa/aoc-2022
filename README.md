# Advent of Code 2022

## Usage

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

## Progress

| Day | Answers | Rust                | Prolog              |
|----:|---------|:-------------------:|:-------------------:|
|   1 | ⭐ ⭐   | [✅](src/day_1.rs)  |                     |
|   2 | ⭐ ⭐   | [✅](src/day_2.rs)  |                     |
|   3 | ⭐ ⭐   | [✅](src/day_3.rs)  |                     |
|   4 | ⭐ ⭐   | [✅](src/day_4.rs)  | [✅](src/day_4.pl)  |
|   5 | ⭐ ⭐   | [✅](src/day_5.rs)  |                     |
|   6 | ⭐ ⭐   | [✅](src/day_6.rs)  |                     |
|   7 | ⭐ ⭐   |                     | [✅](src/day_7.pl)  |
|   8 | ⭐ ⭐   | [✅](src/day_8.rs)  |                     |
|   9 | ⭐ ⭐   |                     | [🤔](src/day_9.pl)  |
|  10 | ⭐ ⭐   |                     | [✅](src/day_10.pl) |
|  11 | ⭐ ⭐   | [✅](src/day_11.rs) |                     |
|  12 | ⭐ ⭐   | [🤔](src/day_12.rs) |                     |
|  13 | ⭐ ⭐   |                     | [✅](src/day_13.pl) |
|  14 |         |                     |                     |
|  15 | ⭐      |                     | [✅](src/day_15.pl) |
|  16 |         |                     |                     |
|  17 |         |                     |                     |
|  18 |         |                     |                     |
|  19 |         |                     |                     |
|  20 |         |                     |                     |
|  21 | ⭐      | [🤔](src/day_21.rs) |                     |
|  22 |         |                     |                     |
|  23 |         |                     |                     |
|  24 |         |                     |                     |
|  25 | ⭐      | [✅](src/day_25.rs) |                     |

- ✅ - correct and happy with
- 🤔 - suboptimal/ugly solution
