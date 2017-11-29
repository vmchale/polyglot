# polyglot

`poly` is a command-line tool that determines project contents. It is much
like [tokei](https://github.com/Aaronepower/tokei). It provides a good summary
of what to expect from a particular codebase.

`poly` is currently quite fast, though it
is not yet stable.

<img src=https://github.com/vmchale/polyglot/raw/master/screenshot.png>

## Benchmarks

On the Idris source code:

<table>
  <tr>
    <th>Tool</th>
    <th>Language</th>
    <th>Time</th>
  </tr>
  <tr>
    <td><code>polyglot</code></td>
    <td>ATS</td>
    <td>29.89 ms</td>
  </tr>
  <tr>
    <td><code>loc</code></td>
    <td>Rust</td>
    <td>25.30 ms</td>
  </tr>
  <tr>
    <td><code>loc</code> (single-threaded)</td>
    <td>Rust</td>
    <td>38.01 ms</td>
  </tr>
  <tr>
    <td><code>tokei</code></td>
    <td>Rust</td>
    <td>46.92 ms</td>
  </tr>
  <tr>
    <td><code>tokei</code> (single-threaded)</td>
    <td>Rust</td>
    <td>90.05 ms</td>
  </tr>
  <tr>
    <td><code>cloc</code></td>
    <td>Perl</td>
    <td>2.556 s s</td>
  </tr>
</table>

## Installation

If you install [stack](http://haskellstack.org/), [pandoc](http://pandoc.org/) and
[patscc](http://www.ats-lang.org/Downloads.html), you can install `poly` with

```bash
 $ ./shake.hs install
```

You may have to add `$HOME/.local/bin` to your `PATH` and
`$HOME/.local/share/man/man1` to your `MANPATH`.

### Documentation

You can view manpages for `poly` with

```
 $ man poly
```

## Support

File formats detected:
  - [x] Vimscript
  - [x] Haskell
  - [x] Rust
  - [x] YAML
  - [x] TOML
  - [x] Happy
  - [x] Alex
  - [x] Idris
  - [x] Madlang
  - [x] ATS
  - [x] Python
  - [x] Elm
  - [x] PureScript
  - [x] C
  - [x] Go
  - [x] Futhark
  - [x] Cabal
  - [x] Verilog
  - [x] VHDL
  - [x] HTML
  - [x] CSS
  - [x] Markdown
  - [x] Ruby
  - [x] Julia
  - [x] R (`.r`)
  - [x] COBOL (`.cob`)
  - [x] Lua
  - [x] Perl
  - [x] OCaml
  - [x] Agda
  - [x] tcl
  - [x] C++
  - [x] LALRPOP
  - [ ] Sixten
  - [ ] Pony (`.pony`)
  - [ ] Dhall
  - [ ] iPKG
  - [ ] ion
  - [ ] Shakespearean languages
  - [ ] Justfile
  - [ ] Makefile
  - [ ] Java
  - [ ] Swift (`.swift`)
  - [ ] assembly (`.asm`, `.s`, `.S`)
  - [ ] crystal (`.cr`)
  - [ ] PHP
  - [ ] D (`.d`)
  - [ ] C#
  - [ ] Mercury
  - [ ] mustache
  - [ ] jinja
  - [ ] C header
  - [ ] agda library
  - [ ] groovy
  - [ ] literate haskell
  - [ ] literate idris
  - [ ] Scala
  - [ ] Elixir
  - [ ] Erlang
  - [ ] FORTRAN
  - [ ] F\*
  - [ ] F#
  - [ ] Isabelle
  - [ ] Kotlin (`.kt`, `.kts`)
  - [ ] REBOL
  - [ ] Intel hex
  - [ ] Pascal
  - [ ] Clojure
  - [ ] racket
  - [ ] rakefile
  - [ ] plaintext
  - [ ] Ada
  - [ ] elisp
  - [ ] JavaScript
  - [ ] TypeScript
  - [ ] CoffeeScript
  - [ ] C++ header file
  - [ ] clean
  - [ ] Ddoc (`.dd`)
