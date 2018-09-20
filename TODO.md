# Features
- [ ] Generate build reports in the format of `cloc` (`gocloc
  --output-type=sloccount .`)
- [ ] Split on lines with `memchr` and then count comments?
- [ ] `.poly.dhall` project file?
- [ ] per-file output like `tokei`/`loc`
- [ ] parse directories on the command line starting with a `-` by using escapes
- [ ] detect documentation comments
- [ ] feature to use data from quasi-quotes (e.g. haskell)
- [ ] block comments
- [ ] Allow generation of HTML tables from output.
- [ ] JSON output? Or alternately some other form of output?
- [ ] HTML pretty-printer would be nice.
- [ ] Allow/disallow symlinks
- [ ] Library for semantic analysis?
- [ ] Avoid overlaps when subdirectories passed as arguments?
- [ ] Make intelligent guesses also work w/ `thefuck`? Write python idk.
- [ ] Point to github URL?
# Performance
- [ ] Make command-line parser call-by-reference
- [x] parallelism
  - [ ] better parallelism
- [ ] Use `memcmp` for string/comment comparisons? (`strcmp`)
  - [ ] Also filter by length?
  - [ ] Potentially useful in the `ifcase` things...
- [ ] make sure everything is tail recursive
- [ ] get rid of GC (?)
  - [ ] Use more linear types
  - [ ] Write my own dirstream??
- [ ] Look at https://github.com/llogiq/bytecount (beat it?)
  - [ ] count lines with SIMD - not just search for stuff
- [ ] also maybe https://github.com/boyerjohn/rapidstring
- [ ] Use `strpbrk` during parsing comments?
- [ ] Benchmark memory footprint!
- [ ] `check_keywords` is very inefficient
# Distribution
- [x] Make a debianization?
  - [ ] Add to debian
  - [ ] Package `compleat`
- [ ] Compress via travis
- [ ] `pkg-config` stuff for library
# CI
- [ ] Figure out how to download/unpack/etc. https://github.com/jgm/pandoc/releases/download/2.2.3.2/pandoc-2.2.3.2-linux.tar.gz
# Portability
- [ ] Figure out Windows (?)
  - [ ] Cross-compilation?
# Bugs
- [ ] Use as a test case: git@github.com:kovidgoyal/kitty.git
- [ ] Fortran `c` comments
- [ ] Allow multiple syntaxes for comments, e.g. `#` for assembly
- [ ] First comment in a file is ignored
- [ ] Handle `sed`-style multiline comments
- [ ] Comments on borders between buffers
- [ ] Is `.ac` always M4?
- [ ] `.bf` could be befunge
- [ ] Handle dhall files w/ no extension?
- [ ] doesn't disambiguate objective C/C headers
- [ ] `poly -t README.md` adds spurious line of output
- [ ] handle `\-dir` for directories
- [ ] Allow three characters for comments (J's `NB.`)
- [ ] Fix library linking for distributable Mac binaries
- [ ] Bug w/ comments: https://www.reddit.com/r/rust/comments/99e4tq/reading_files_quickly_in_rust/e4nh5nf
# Code Maintenance
- [ ] Flag for printing debug info?
- [ ] Library for this
  - [ ] Library stanza & Haskell bindings
  - [ ] Executable that depends on the library
- [ ] linear types to ensure each field gets printed at the end?
  - [ ] or alternately a macro
- [ ] Use a `Makefile` rather than a `Justfile`
# Libraries
- [ ] Dhall library for ATS (or sufficient bindings)
- [ ] Library for `.gitignore` files (and `.ignore` or `_darcs/boring` files)
- [ ] Parallel directory traversal (templates?)
- [ ] Library for cross-platform directory access?
# Documentation
- [ ] Benchmark against sloccount?
- [ ] Document build/configuration options
- [ ] Compare to http://hackage.haskell.org/package/cantor ?
# Research
- [ ] Learn about `memcmp` and `memcpy`
- [ ] Look into state machines and how to actually make a parser
- [ ] Look into parallel directory traversals & data structures for such
  - [ ] Atomics?? See what I can write for atomics in ATS
  - [ ] Find out what mutexes/spinlocks do
# Build Tooling
- [ ] Auto-generate functions related to file types?
## Heuristics
### Heuristics for determining file type:
- [ ] parent directory (`.cargo/config`)
- [ ] modeline
- [ ] `.gitattributes`
- [ ] `.poly.dhall` project file
### Heuristics for determining file relevance:
- [ ] `.gitignore`/darcs boringfile/`.ignore` file
- [ ] `.gitattributes`
- [ ] `.poly.dhall` project file
