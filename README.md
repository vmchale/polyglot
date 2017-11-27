# polyglot

`poly` is a command-line tool that determines project contents. It is much
like [tokei](https://github.com/Aaronepower/tokei). Such tools can help one
orient oneself in an unfamiliar codebase.

`poly` is currently faster than all other code counting tools, though it
supports far fewer file formats than is desirable.

<img src=https://github.com/vmchale/polyglot/raw/master/screenshot.png>

## Benchmarks

The Rust repo:

<table>
  <tr>
    <th>Tool</th>
    <th>Language</th>
    <th>Time</th>
  </tr>
  <tr>
    <td><code>polyglot</code></td>
    <td>ATS</td>
    <td>98.24 ms</td>
  </tr>
  <tr>
    <td><code>loc</code></td>
    <td>Rust</td>
    <td>131.1 ms</td>
  </tr>
  <tr>
    <td><code>tokei</code></td>
    <td>Rust</td>
    <td>341.8 ms</td>
  </tr>
  <tr>
    <td><code>enry</code></td>
    <td>Go</td>
    <td>5.183 s</td>
  </tr>
  <tr>
    <td><code>cloc</code></td>
    <td>Perl</td>
    <td>16.47 s</td>
  </tr>
  <tr>
    <td><code>linguist</code></td>
    <td>Ruby</td>
    <td>16.70 s</td>
  </tr>
</table>

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
  - [ ] PureScript
  - [ ] C
  - [ ] C++
  - [x] Go
  - [ ] Sixten
  - [ ] Futhark
  - [ ] Dhall
  - [x] Cabal
  - [ ] iPKG
  - [x] Verilog
  - [x] VHDL
  - [ ] ion
  - [x] HTML
  - [x] CSS
  - [ ] Shakespearean languages
  - [x] Markdown
  - [ ] Justfile
  - [ ] Makefile
  - [ ] Java
  - [ ] Ruby
  - [ ] Swift
  - [ ] assembly
  - [ ] crystal
  - [ ] PHP
  - [ ] Julia
  - [ ] D
  - [ ] C#
  - [ ] Mercury
  - [ ] mustache
  - [ ] jinja
  - [ ] C header
  - [ ] agda library
  - [ ] groovy
  - [ ] literate haskell
  - [ ] literate idris
  - [ ] R
  - [ ] Scala
  - [ ] Elixir
  - [ ] Erlang
  - [ ] FORTRAN
  - [ ] F\*
  - [ ] F#
  - [ ] Isabelle
  - [ ] Kotlin
  - [ ] Lua
  - [ ] Pascal
  - [ ] Clojure
  - [ ] racket
