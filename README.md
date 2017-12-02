# polyglot

`poly` is a command-line tool that determines project contents.

<img alt="Screenshot of sample output" src=https://github.com/vmchale/polyglot/raw/master/screenshot.png>
<img alt="Screenshot of tabular output" src=https://github.com/vmchale/polyglot/raw/master/tabular-output.png>

## Features

Reasons to use polyglot:

  * Faster than `tokei`
  * More accurate than `loc`
  * Pretty magenta output

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

Polyglot distinguishes itself by being able to disambiguate file names. Thus,
while `tokei` and `loc` confuse Verilog for Coq, `poly` will not.

## Installation

The easiest way to install is to use the installation script, like so:

```bash
curl -sSl https://nest.pijul.com/vamchale/polyglot:master/352f87c9ecd7775f1?raw | sh -s
```

## Building

If you install [stack](http://haskellstack.org/), [pandoc](http://pandoc.org/) and
[patscc](http://www.ats-lang.org/Downloads.html), you can install `poly` with

```bash
 $ ./shake.hs install
```

You may have to add `$HOME/.local/bin` to your `PATH` and
`$HOME/.local/share/man/man1` to your `MANPATH`.

You can optionally install [compleat](https://github.com/mbrubeck/compleat) for
shell completions as well.

### Documentation

You can view manpages for `poly` with

```
 $ man poly
```
