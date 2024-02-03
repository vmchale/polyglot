% poly (1)
% Vanessa McHale<vamchale@gmail.com>

# NAME

poly - a command-line tool for counting source code

# SYNOPSIS

  poly [DIRECTORY]... [OPTION]...

# DESCRIPTION

**poly** determines the contents of projects, giving a breakdown of all
languages present.

When no directory is given, polyglot will execute in the current directory.

# OPTIONS

**-h**, **-\-help** Display help and exit

**-V**, **-\-version** Display version and exit

**-e**, **-\-exclude** Exclude a directory

**-p**, **-\-no-parallel** Do not execute in parallel. Concurrency is already
disabled on some platforms.

**-t**, **-\-no-table** Show results in an alternate format.

**-c**, **-\-no-color** Don't colorize output

**-v**, **-\-verbose** Enable per-file output

**-\-html** Dump HTML output

**-s**, **-\-no-style** Do not dump CSS styles with HTML output

# EXAMPLES

Show the contents of ~/programming

```
poly ~/programming
```

Count lines of source code in the current directory, skipping any directory called forks

```
poly -e forks
```

Show the contents of ./project/src, displaying an alternate output in the terminal

```
poly ./project/src --no-table
```

Write a file called report.html containing line count information from the
current directory

```
poly --html > report.html
```

Append line count information in HTML to README.md

```
poly --html --no-table >> README.md
```

# BUG REPORTS

For bug reports and updates, go to https://github.com/vmchale/polyglot/issues
