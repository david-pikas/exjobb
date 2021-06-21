## How to run the program

Requirements: the "nightly" version of the compiler

To generate code and test it, run

```
cargo run -- [FLAGS]
```

For more information on different flags available, run 

```
cargo run -- --help
```

## Progress

Can generate a fair amount of syntactically correct programs.

To potential syntactically bugs have been found, namely `./interesting_cases/break_labled_block_minimal.rs` and `./interesting_cases/half_open_ranges.rs`.

Only simpler syntactically valid programs can be generated. These programs sometimes fail to compile because rust is unable to infer correct types, for example programs containing the following: `drop(None)`. Another common way for programs to fail is that they contain types who's size can't be determined at runtime, e.g. recursive types

The programs tend to not use ownership in any interesting way and thus tends to pass the borrow checker, it should be theoretically possible (although I have not seen it) to generate programs that do not pass the borrow checker such as:

```rust
fn main() {
    let a = String::new();
    drop(a);
    drop(a);
}
```
