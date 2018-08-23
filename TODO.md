# Features
- [ ] Generate build reports in the format of `cloc` (`gocloc
  --output-type=sloccount .`)
- [ ] `.poly.toml` project file?
- [ ] per-file output like `tokei`/`loc`
- [ ] parse directories on the command line starting with a `-` by using escapes
- [ ] detect documentation comments
- [ ] feature to use data from quasi-quotes (e.g. haskell)
- [ ] block comments
- [ ] Allow generation of HTML tables from output.
- [ ] JSON output? Or alternately some other form of output?
- [ ] HTML pretty-printer would be nice.
- [ ] Detailed help on how to use a type?
- [ ] Allow/disallow symlinks
- [ ] Library for semantic analysis?
- [ ] Avoid overlaps when subdirectories passed as arguments?
- [ ] Make intelligent guesses also work w/ `thefuck`? Write python idk.
# Performance
- [x] parallelism
  - [ ] better parallelism
- [ ] make sure everything is tail recursive
- [ ] linear types everywhere!!
- [ ] get rid of GC (?)
- [ ] Make loops call-by-reference?
- [ ] Look at https://github.com/llogiq/bytecount (beat it?)
  - [ ] count lines with SIMD? not just search for stuff
- [ ] also maybe https://github.com/boyerjohn/rapidstring
- [ ] Use `strpbrk` during parsing comments?
# Distribution
- [x] Make a debianization?
  - [ ] Add to debian
  - [ ] Package `compleat`
- [ ] Compress via travis
- [ ] `pkg-config` stuff for library
# Portability
- [ ] Figure out Windows (?)
# Bugs
- [ ] `poly te` hangs indefinitely
- [ ] Figure out why it occasionally hangs indefinitely
- [ ] Comments on borders between buffers
- [ ] `.bf` could be befunge
- [ ] Handle dhall files w/ no extension?
- [ ] doesn't disambiguate objective C/C headers
- [ ] `poly -t README.md` adds spurious line of output
- [ ] `poly -tc` still adds colors
- [ ] handle `\-dir` for directories
- [ ] Allow three characters for comments (J's `NB.`)
- [ ] Bug w/ comments: https://www.reddit.com/r/rust/comments/99e4tq/reading_files_quickly_in_rust/e4nh5nf
# Code Maintenance
- [ ] Add benchmark suite
- [ ] Library for this
  - [ ] `.sats` files
  - [ ] Library stanza & Haskell bindings
  - [ ] Executable that depends on the library
- [ ] linear types to ensure each field gets printed at the end?
  - [ ] or alternately a macro
- [ ] Use a `Makefile` rather than a `Justfile`
# Libraries
- [ ] TOML parser for ATS (or sufficient bindings)
- [ ] Library for `.gitignore` files (and `.ignore` or `_darcs/boring` files)
- [ ] Chase-levenshtein work-stealing queue
- [ ] Parallel directory traversal (templates?)
- [ ] Library for cross-platform directory access?
# Documentation
- [ ] Benchmark against sloccount?
- [ ] Document build/configuration options
- [ ] Compare to http://hackage.haskell.org/package/cantor ?
## Heuristics
### Heuristics for determining file type:
- [ ] parent directory (`.cargo/config`)
- [ ] modeline
- [ ] `.gitattributes`
- [ ] `.poly.dhall` project file
### Heuristics for determining file relevance:
- [ ] `.gitignore`/darcs boringfile/`.ignore` file
- [ ] `.gitattributes`
- [ ] `.poly.toml` project file
