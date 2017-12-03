# polyglot

[![Build Status](https://travis-ci.org/vmchale/polyglot.svg?branch=master)](https://travis-ci.org/vmchale/polyglot)

`poly` is a command-line tool that determines project contents.

<img alt="Screenshot of sample output" src=https://github.com/vmchale/polyglot/raw/master/screenshot.png>
<img alt="Screenshot of tabular output" src=https://github.com/vmchale/polyglot/raw/master/tabular-output.png>

## Features

Reasons to use polyglot:

  * Fast: 217 ms to run on the entire GHC codebase
  * Accurate: won't confuse Coq and Verilog
  * Pretty: magenta output

Reasons not to use polyglot:

  * It's written in ATS
  * No regex-based exclusions

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
    <td>224.3 ms</td>
  </tr>
  <tr>
    <td><code>loc</code></td>
    <td>Rust</td>
    <td>135.5 ms</td>
  </tr>
  <tr>
    <td><code>tokei</code></td>
    <td>Rust</td>
    <td>331.4 ms</td>
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

If you install [stack](http://haskellstack.org/), [pandoc](http://pandoc.org/) and
[patscc](http://www.ats-lang.org/Downloads.html), you can install `poly` with

```bash
 $ ./shake.hs install
```

### Documentation

You can view manpages for `poly` with

```
 $ man poly
```
