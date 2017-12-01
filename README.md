# polyglot

`poly` is a command-line tool that determines project contents. It is much
like [tokei](https://github.com/Aaronepower/tokei). It provides a good summary
of what to expect from a particular codebase.

`poly` is currently quite fast, though parallelism remains to be implemented.

<img src=https://github.com/vmchale/polyglot/raw/master/screenshot.png>

## Features

Reasons to use polyglot:

  * Faster than `tokei`
  * More accurate than `loc`, `tokei`, or `linguist`
  * Pretty magenta output

Reasons not to use polyglot:

  * It's written in ATS
  * Fewer features than `tokei`
  * No regex-based exclusions

## Benchmarks

On the Rust repo:

<table>
  <tr>
    <th>Tool</th>
    <th>Language</th>
    <th>Time</th>
  </tr>
  <tr>
    <td><code>polyglot</code></td>
    <td>ATS</td>
    <td>231.1 ms</td>
  </tr>
  <tr>
    <td><code>loc</code></td>
    <td>Rust</td>
    <td>163.6 ms</td>
  </tr>
  <tr>
    <td><code>tokei</code></td>
    <td>Rust</td>
    <td>345.2 ms</td>
  </tr>
  <tr>
    <td><code>cloc</code></td>
    <td>Perl</td>
    <td>15.51 s</td>
  </tr>
  <tr>
    <td><code>linguist</code></td>
    <td>Ruby</td>
    <td>16.21 s</td>
  </tr>
</table>

## Installation

The easiest way to install is to download a binary from the
[releases](https://github.com/vmchale/polyglot/releases) page and put it
somewhere on your `PATH`.

## Building

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

## Supported Languages

File formats detected:
  - [x] Vimscript
  - [x] Haskell
    - [x] Shakespearean languages
      - [x] cassius
      - [x] lucius
      - [x] hamlet
      - [x] julius
    - [ ] literate Haskell
  - [x] Rust
  - [x] YAML
  - [x] TOML
  - [x] Happy
  - [x] Alex
  - [x] Idris
    - [ ] literate Idris
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
  - [x] Tcl
  - [x] C++
  - [x] LALRPOP
  - [x] C header
  - [x] Sixten
  - [x] Dhall
  - [x] iPKG
  - [x] bash
  - [x] ion
  - [x] Justfile
  - [x] Makefile
  - [x] Yacc
  - [ ] Pony (`.pony`)
  - [ ] Elixir
  - [ ] Erlang
  - [ ] Java
  - [ ] Swift (`.swift`)
  - [ ] assembly (`.asm`, `.s`, `.S`)
  - [ ] crystal (`.cr`)
  - [ ] PHP
  - [ ] D (`.d`)
    - [ ] DDoc
  - [ ] Dockerfile
  - [ ] C#
  - [ ] Mercury
  - [ ] Objective C
  - [ ] mustache
  - [ ] jinja
  - [ ] Agda library
  - [ ] groovy
  - [ ] Scala
  - [ ] FORTRAN
  - [ ] F\*
  - [ ] F#
  - [ ] Isabelle
  - [ ] Kotlin (`.kt`, `.kts`)
  - [ ] REBOL
  - [ ] Intel hex
  - [ ] occam
  - [ ] Pascal
  - [ ] modula
  - [ ] Clojure
  - [ ] racket
  - [ ] Rakefile
  - [ ] Plaintext
  - [ ] Ada
  - [ ] elisp
  - [ ] JavaScript
  - [ ] TypeScript
  - [ ] CoffeeScript
  - [ ] C++ header file
  - [ ] clean
  - [ ] lean
  - [ ] nim
  - [ ] dash
  - [ ] fish
  - [ ] dyon
  - [ ] ketos
  - [ ] harlan
  - [ ] Objective C
  - [ ] Ragel
  - [ ] puppet
  - [ ] forth
  - [ ] Objective C++
  - [ ] J
  - [ ] Lex
  - [ ] Bison (`.yy` among others)
  - [ ] flex
  - [ ] Felix
  - [ ] Autoconf (`.in`)
  - [ ] Batch
  - [ ] PowerShell
  - [ ] Sass
  - [ ] Scons
  - [ ] Vala
  - [ ] Cmake
  - [ ] shen
  - [ ] ceylon
  - [ ] Dart
  - [ ] Korn shell
  - [ ] fish
  - [ ] jsx
  - [ ] ColdFusion
  - [ ] Balsa
  - [ ] Teak
  - [ ] SQL
