# polyglot

[![Build Status](https://travis-ci.org/vmchale/polyglot.svg?branch=master)](https://travis-ci.org/vmchale/polyglot)

`poly` is a command-line tool that determines project contents.

<img alt="Screenshot of sample output" src=https://github.com/vmchale/polyglot/raw/master/screenshot.png>

## Features

Reasons to use polyglot:

  * Fast: 266 ms to run on a fully checked-out GHC
  * Accurate: won't confuse Coq and Verilog
  * Pretty: magenta output

Reasons not to use polyglot:

  * It's written in ATS
  * No regex-based exclusions
  * Requires [MinGW](http://www.mingw.org/) on Windows.
  * GC is broken

### Benchmarks

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
    <td>188.1 ms</td>
  </tr>
  <tr>
    <td><code>loc</code></td>
    <td>Rust</td>
    <td>159.0 ms</td>
  </tr>
  <tr>
    <td><code>tokei</code></td>
    <td>Rust</td>
    <td>345.9 ms</td>
  </tr>
  <tr>
    <td><code>cloc</code></td>
    <td>Perl</td>
    <td>14.86 s</td>
  </tr>
  <tr>
    <td><code>linguist</code></td>
    <td>Ruby</td>
    <td>15.17 s</td>
  </tr>
</table>

### Heuristics

Polyglot distinguishes itself from `tokei` and `loc` by being able to disambiguate file names.
Thus, `poly` will not confuse Happy for Yacc (for instance).

## Installation

The easiest way to install is to use the installation script, like so:

```bash
curl -sSl https://raw.githubusercontent.com/vmchale/polyglot/master/bash/install.sh | bash -s
```

You may need to add `$HOME/.local/bin` to your `PATH` and
`$HOME/.local/share/man/man1` to your `MANPATH`.

You can optionally install [compleat](https://github.com/mbrubeck/compleat) for
shell completions as well.

## Building

First, install [GHC](https://www.haskell.org/ghc/download.html) and
[cabal](https://www.haskell.org/cabal/download.html).  Then run

```bash
 $ ./bash/bootstrap.sh
```

to bootstrap the build system. Next, install
[pats-filter](https://github.com/Hibou57/PostiATS-Utilities),
[pandoc](http://pandoc.org/), and
[patscc](http://www.ats-lang.org/Downloads.html). You should then be able to
build with

```bash
 $ ./build
```

### Documentation

You can view manpages for `poly` with

```
 $ man poly
```
