# polyglot

[![Build Status](https://travis-ci.org/vmchale/polyglot.svg?branch=master)](https://travis-ci.org/vmchale/polyglot)

`poly` is a command-line tool that determines project contents.

<img alt="Screenshot of sample output" src=https://github.com/vmchale/polyglot/raw/master/screenshot.png>

## Features

Reasons to use polyglot:

  * Fast: 215 ms to run on a fully checked-out GHC source tree
  * Accurate: won't confuse Coq and Verilog
  * Pretty: magenta output

Reasons not to use polyglot:

  * It's written in ATS
  * No regex-based exclusions
  * Only supported on Linux

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
    <td>126.0 ms</td>
  </tr>
  <tr>
    <td><code>loc</code></td>
    <td>Rust</td>
    <td>166.7 ms</td>
  </tr>
  <tr>
    <td><code>tokei</code></td>
    <td>Rust</td>
    <td>362.7 ms</td>
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

### From a Script

The easiest way to install is to use the installation script, like so:

```bash
curl -sSl https://raw.githubusercontent.com/vmchale/polyglot/master/bash/install.sh | bash -s
```

You may need to add `$HOME/.local/bin` to your `PATH` and
`$HOME/.local/share/man/man1` to your `MANPATH`.

You can optionally install [compleat](https://github.com/mbrubeck/compleat) for
shell completions as well.

## From Source

You can install [ats-pkg](http://hackage.haskell.org/package/ats-pkg)
with

```bash
 $ curl -sSl https://raw.githubusercontent.com/vmchale/atspkg/master/bash/install.sh | bash -s
```

And install `poly` with

```bash
 $ atspkg remote https://github.com/vmchale/polyglot/archive/0.3.32.tar.gz
```

### Documentation

You can view manpages for `poly` with

```
 $ man poly
```
