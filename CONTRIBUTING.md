# Contributing to Madlang

I emphatically welcome bug reports, issues you may encounter, documentation, and
pull requests. Feature requests will likely be implemented if and only they are
relatively small; feel free to request support for a particular language, no
matter how niche.

## Getting started

If you'd like ideas for ways to contribute, check out `TODO.md`. Feel free to
open a PR or an issue if you need guidance on how to implement something.

## Navigating the Code

ATS can be quite daunting. The following are things that are relatively easy to
implement:

  * Adding a new file type and its associated extension (note that the name of
    a file type must be less than 20 characters; otherwise there will be a type
    error!)
  * Adding a new directory to ignore (e.g. `.cabal-sandbox`)
  * Adding new shebang detection
  * Updating the `--help` or manpage.
  * Adding keywords to disambiguate extensions.

If you have any trouble figuring this out, feel free to open an issue and I will
give you guidance on where to look. I am occasionally in the `##ats` IRC
channel, and you can ask me any questions about the code that you'd like.

## Rules etc.
We follow the [rust standards of
conduct](https://www.rust-lang.org/en-US/conduct.html), with the addendum that
we are committed to providing a friendly, safe and welcoming environment
regardless of sex worker status or previous sex worker status.

In addition, please be aware that not everyone speaks English as a first
language.
