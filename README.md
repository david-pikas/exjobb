## How to run the program

Requirements: the "nightly" version of the compiler.

There are two binaries, `rustsmith` and `generate-code`. `generate-code` generates a single program using stdin as data. `rustsmith` can run a sequence of programs and calls `generate-code`. For this reason, it is not enough to run `cargo run` because this won't compile the `generate-code` binary. If the binary isn't generated or needs to be updated, run `cargo build` first. This needs to be done separately for the release version

To generate code and test it, run

```
cargo build && cargo run -- [FLAGS]
```

For more information on different flags available, run 

```
cargo run -- --help
```

If running several tests it is recommended to include the release flag (`cargo build --release && cargo run --release -- [FLAGS]`) because this significantly speeds up code generation at the cost of longer compilation times.

## Progress

Can generate a fair amount of syntactically correct programs.

The program is able to correctly handle types and ownership. The main remaining class of illegal programs being generated is due to the program not handling multiple things having the same name properly. For example, it might generate function signatures with multiple parameters named the same thing (which is illegal). It might also attempt to reference a variable somewhere else that has been shadowed by another variable. Stack overflows also happen occasionally when generating programs. Both of these things happen about once or twice every 10 000 programs

An (incomplete) list of missing language features:

 * If/else and match
 * loops
 * type synonyms
 * modules and `use`
 * unsafe code
   - unions
   - pointers
 * Generics for structs
 * Early returns from functions
 * Blocks, if/else and so on in the middle of expressions

