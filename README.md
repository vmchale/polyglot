# polyglot

`polyglot` is a command-line tool that determines project contents. It is much
like [tokei](https://github.com/Aaronepower/tokei). Such tools can help one
orient oneself in an unfamiliar codebase.

<img src=https://github.com/vmchale/polyglot/raw/master/screenshot.png>

## Benchmarks

<table>
  <tr>
    <th>Tool</th>
    <th>Language</th>
    <th>Time</th>
  </tr>
  <tr>
    <td><code>polyglot</code></td>
    <td>ATS</td>
    <td>16.60 ms</td>
  </tr>
  <tr>
    <td><code>tokei</code></td>
    <td>Rust</td>
    <td>156.7 ms</td>
  </tr>
    <td><code>loc</code></td>
    <td>Rust</td>
    <td>54.62 ms</td>
  </tr>
  <tr>
    <td><code>enry</code></td>
    <td>Go</td>
    <td>4.501 s</td>
  </tr>
  <tr>
    <td><code>cloc</code></td>
    <td>Perl</td>
    <td>11.61 s</td>
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
  - [ ] Go
  - [ ] Sixten
  - [ ] Futhark
  - [ ] Dhall
  - [x] Cabal
  - [ ] iPKG
  - [ ] nix
  - [ ] Coq
  - [ ] Verilog
  - [ ] VHDL
  - [ ] ion
  - [ ] HTML
  - [ ] CSS
  - [ ] Shakespearean languages
  - [x] Markdown
  - [ ] Justfile
  - [ ] Makefile
  - [ ] Java
  - [ ] Ruby
  - [ ] Swift
  - [ ] PHP
  - [ ] Julia
  - [ ] D
  - [ ] C#
  - [ ] Mercury
  - [ ] C header file
  - [ ] mustache
  - [ ] jinja
