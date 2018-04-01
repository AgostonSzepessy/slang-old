# Slang
[![Build Status](https://jenkins.agoston.codes/buildStatus/icon?job=slang)](https://jenkins.agoston.codes/job/slang/)

Slang is a simple scripting language.

## Building
In order to build Slang, Git and Rust must be installed.

```
git clone git@github.com:AgostonSzepessy/slang.git
cd slang
cargo build
```

## Tutorial
To get an overview of Slang, read [Slang Overview](docs/language.md).

## TODO
* [x] Include row number within tokens.
* [x] Include column number on which token starts within tokens.
* [ ] Refactor `ParseError` so more error types are reused.
* [ ] Add tokenization for strings.

## License
Slang is licensed under the Mozilla Public License, version 2.0. For more details,
see [the license](LICENSE).
