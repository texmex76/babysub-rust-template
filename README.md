# babysub-rust

This is the Rust template for the SAT preprocessor exercise.

The data structures are not as refined as in the C++ equivalent, so expect slower performance. I will try to improve them.

If you have any suggestions or find some bugs, pull requests are welcome!

# Running

```
cargo run -- [OPTIONS] [CNF PATH] [OUT PATH]
```

# Testing

Since simplification is not implemented now, all tests will fail.

```
cargo test
```

# Logging

```
cargo run --features "logging" -- [OPTIONS] [CNF PATH] [OUT PATH]
```
