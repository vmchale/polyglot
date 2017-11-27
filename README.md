# polyglot

`poly` is a command-line tool that determines project contents. It is much
like [tokei](https://github.com/Aaronepower/tokei). Such tools can help one
orient oneself in an unfamiliar codebase.

`poly` is currently faster than all other code counting tools, though it
supports far fewer file formats than is desirable. I currently use it alongside
`tokei`.

<img src=https://github.com/vmchale/polyglot/raw/master/screenshot.png>

## Benchmarks

Medium-sized directory:

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

This repo:

<table>
  <tr>
    <th>Tool</th>
    <th>Language</th>
    <th>Time</th>
  </tr>
  <tr>
    <td><code>polyglot</code></td>
    <td>ATS</td>
    <td>1.165 ms</td>
  </tr>
  <tr>
    <td><code>tokei</code></td>
    <td>Rust</td>
    <td>3.503 ms</td>
  </tr>
    <td><code>loc</code></td>
    <td>Rust</td>
    <td>2.999 ms</td>
  </tr>
  <tr>
    <td><code>enry</code></td>
    <td>Go</td>
    <td>27.50 ms</td>
  </tr>
  <tr>
    <td><code>cloc</code></td>
    <td>Perl</td>
    <td>223.6 ms</td>
  </tr>
  <tr>
    <td><code>linguist</code></td>
    <td>Ruby</td>
    <td>1.623 s</td>
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
  - [ ] assembly
  - [ ] crystal
  - [ ] PHP
  - [ ] Julia
  - [ ] D
  - [ ] C#
  - [ ] Mercury
  - [ ] mustache
  - [ ] jinja
