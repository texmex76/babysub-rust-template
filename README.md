# babysub-rust-template

This is the Rust template for the SAT preprocessor exercise.

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
