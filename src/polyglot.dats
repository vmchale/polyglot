#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
#include "prelude/DATS/filebas.dats"
#include "libats/ML/DATS/filebas_dirent.dats"
#include "libats/libc/DATS/dirent.dats"
#include "libats/ML/DATS/list0.dats"

%{^
#include "libats/libc/CATS/string.cats"
%}

(* ****** ****** *)
staload "prelude/DATS/unsafe.dats"
staload "libats/ML/DATS/string.dats"
staload "libats/libc/SATS/stdio.sats"
staload "prelude/SATS/filebas.sats"
staload "src/filetype.sats"
staload EXTRA = "libats/ML/DATS/filebas.dats"
staload EXTRA = "libats/ML/SATS/filebas.sats"
(* ****** ****** *)

fun get_contents(s: string): string =
  let
    val stream = fileref_open_exn(s, file_mode_r)
    val contents = fileref_get_file_string(stream) // TODO get lines instead?
  in
    strptr2string(contents)
  end

fun line_count(s: string): int =
  let
    val ref = fileref_open_exn(s, file_mode_r)
    val viewstream: stream_vt(string) = $EXTRA.streamize_fileref_line(ref)
  in
    stream_vt_length(viewstream) - 1
  end

fun maybe_string(s: string, n: int): string =
  if n > 0
    then
      s + ": " + tostring_int(n) + "\n"
    else
      ""

fun with_nonempty(s1: string, s2: string): string =
  if s2 != ""
    then
      s1 + s2
    else
      ""

fun make_output(isc: source_contents): string =
  with_nonempty("[33mProgramming Languages:[0m\n",
    maybe_string("Rust", isc.rust) +
    maybe_string("Haskell", isc.haskell) +
    maybe_string("ATS", isc.ats) +
    maybe_string("Python", isc.python) +
    maybe_string("Elm", isc.elm) +
    maybe_string("Vimscript", isc.vimscript) +
    maybe_string("Idris", isc.idris) +
    maybe_string("Madlang", isc.madlang)
  ) +
  with_nonempty("\n[33mDocumentation:[0m\n",
    maybe_string("TeX", isc.tex) +
    maybe_string("Markdown", isc.markdown)
  ) +
  with_nonempty("\n[33mConfiguration:[0m\n",
    maybe_string("YAML", isc.yaml) +
    maybe_string("TOML", isc.toml) +
    maybe_string("Cabal", isc.cabal)
  ) +
  with_nonempty("\n[33mParser Generators:[0m\n",
    maybe_string("Happy", isc.happy) +
    maybe_string("Alex", isc.alex)
  )

fun adjust_contents(prev: source_contents, scf: pl_type) : source_contents =
  case+ scf of
    | haskell n => 
      let
        val sc = @{ rust = prev.rust
                  , haskell = prev.haskell + n
                  , ats = prev.ats
                  , python = prev.python
                  , vimscript = prev.vimscript
                  , elm = prev.elm
                  , idris = prev.idris
                  , madlang = prev.madlang
                  , tex = prev.tex
                  , markdown = prev.markdown
                  , yaml = prev.yaml
                  , toml = prev.toml
                  , cabal = prev.cabal
                  , happy = prev.happy
                  , alex = prev.alex
                  }
      in
        sc
      end
    | ats n =>
      let
        val sc = @{ rust = prev.rust
                  , haskell = prev.haskell
                  , ats = prev.ats + n
                  , python = prev.python
                  , vimscript = prev.vimscript
                  , elm = prev.elm 
                  , idris = prev.idris
                  , madlang = prev.madlang
                  , tex = prev.tex
                  , markdown = prev.markdown
                  , yaml = prev.yaml
                  , toml = prev.toml
                  , cabal = prev.cabal
                  , happy = prev.happy
                  , alex = prev.alex
                  }
      in
        sc
      end
    | rust n =>
      let
        val sc = @{ rust = prev.rust + n
                  , haskell = prev.haskell
                  , ats = prev.ats
                  , python = prev.python
                  , vimscript = prev.vimscript
                  , elm = prev.elm 
                  , idris = prev.idris
                  , madlang = prev.madlang
                  , tex = prev.tex
                  , markdown = prev.markdown
                  , yaml = prev.yaml
                  , toml = prev.toml
                  , cabal = prev.cabal
                  , happy = prev.happy
                  , alex = prev.alex
                  }
      in
        sc
      end
    | python n =>
      let
        val sc = @{ rust = prev.rust
                  , haskell = prev.haskell
                  , ats = prev.ats
                  , python = prev.python + n
                  , vimscript = prev.vimscript
                  , elm = prev.elm 
                  , idris = prev.idris
                  , madlang = prev.madlang
                  , tex = prev.tex
                  , markdown = prev.markdown
                  , yaml = prev.yaml
                  , toml = prev.toml
                  , cabal = prev.cabal
                  , happy = prev.happy
                  , alex = prev.alex
                  }
      in
        sc
      end
    | elm n =>
      let
        val sc = @{ rust = prev.rust
                  , haskell = prev.haskell
                  , ats = prev.ats
                  , python = prev.python
                  , vimscript = prev.vimscript
                  , elm = prev.elm + n
                  , idris = prev.idris
                  , madlang = prev.madlang
                  , tex = prev.tex
                  , markdown = prev.markdown
                  , yaml = prev.yaml
                  , toml = prev.toml
                  , cabal = prev.cabal
                  , happy = prev.happy
                  , alex = prev.alex
                  }
      in
        sc
      end
    | vimscript n =>
      let
        val sc = @{ rust = prev.rust
                  , haskell = prev.haskell
                  , ats = prev.ats
                  , python = prev.python
                  , vimscript = prev.vimscript + n
                  , elm = prev.elm
                  , idris = prev.idris
                  , madlang = prev.madlang
                  , tex = prev.tex
                  , markdown = prev.markdown
                  , yaml = prev.yaml
                  , toml = prev.toml
                  , cabal = prev.cabal
                  , happy = prev.happy
                  , alex = prev.alex
                  }
      in
        sc
      end
    | idris n =>
      let
        val sc = @{ rust = prev.rust
                  , haskell = prev.haskell
                  , ats = prev.ats
                  , python = prev.python
                  , vimscript = prev.vimscript
                  , elm = prev.elm
                  , idris = prev.idris + n
                  , madlang = prev.madlang
                  , tex = prev.tex
                  , markdown = prev.markdown
                  , yaml = prev.yaml
                  , toml = prev.toml
                  , cabal = prev.cabal
                  , happy = prev.happy
                  , alex = prev.alex
                  }
      in
        sc
      end
    | madlang n =>
      let
        val sc = @{ rust = prev.rust
                  , haskell = prev.haskell
                  , ats = prev.ats
                  , python = prev.python
                  , vimscript = prev.vimscript
                  , elm = prev.elm
                  , idris = prev.idris
                  , madlang = prev.madlang + n
                  , tex = prev.tex
                  , markdown = prev.markdown
                  , yaml = prev.yaml
                  , toml = prev.toml
                  , cabal = prev.cabal
                  , happy = prev.happy
                  , alex = prev.alex
                  }
      in
        sc
      end
    | tex n =>
      let
        val sc = @{ rust = prev.rust
                  , haskell = prev.haskell
                  , ats = prev.ats
                  , python = prev.python
                  , vimscript = prev.vimscript
                  , elm = prev.elm
                  , idris = prev.idris
                  , madlang = prev.madlang
                  , tex = prev.tex + n
                  , markdown = prev.markdown
                  , yaml = prev.yaml
                  , toml = prev.toml
                  , cabal = prev.cabal
                  , happy = prev.happy
                  , alex = prev.alex
                  }
      in
        sc
      end
    | markdown n =>
      let
        val sc = @{ rust = prev.rust
                  , haskell = prev.haskell
                  , ats = prev.ats
                  , python = prev.python
                  , vimscript = prev.vimscript
                  , elm = prev.elm
                  , idris = prev.idris
                  , madlang = prev.madlang
                  , tex = prev.tex
                  , markdown = prev.markdown + n
                  , yaml = prev.yaml
                  , toml = prev.toml
                  , cabal = prev.cabal
                  , happy = prev.happy
                  , alex = prev.alex
                  }
      in
        sc
      end
    | toml n =>
      let
        val sc = @{ rust = prev.rust
                  , haskell = prev.haskell
                  , ats = prev.ats
                  , python = prev.python
                  , vimscript = prev.vimscript
                  , elm = prev.elm
                  , idris = prev.idris
                  , madlang = prev.madlang
                  , tex = prev.tex
                  , markdown = prev.markdown
                  , yaml = prev.yaml
                  , toml = prev.toml + n
                  , cabal = prev.cabal
                  , happy = prev.happy
                  , alex = prev.alex
                  }
      in
        sc
      end
    | yaml n =>
      let
        val sc = @{ rust = prev.rust
                  , haskell = prev.haskell
                  , ats = prev.ats
                  , python = prev.python
                  , vimscript = prev.vimscript
                  , elm = prev.elm
                  , idris = prev.idris
                  , madlang = prev.madlang
                  , tex = prev.tex
                  , markdown = prev.markdown
                  , yaml = prev.yaml + n
                  , toml = prev.toml
                  , cabal = prev.cabal
                  , happy = prev.happy
                  , alex = prev.alex
                  }
      in
        sc
      end
    | cabal n =>
      let
        val sc = @{ rust = prev.rust
                  , haskell = prev.haskell
                  , ats = prev.ats
                  , python = prev.python
                  , vimscript = prev.vimscript
                  , elm = prev.elm
                  , idris = prev.idris
                  , madlang = prev.madlang
                  , tex = prev.tex
                  , markdown = prev.markdown
                  , yaml = prev.yaml
                  , toml = prev.toml
                  , cabal = prev.cabal + n
                  , happy = prev.happy
                  , alex = prev.alex
                  }
      in
        sc
      end
    | happy n =>
      let
        val sc = @{ rust = prev.rust
                  , haskell = prev.haskell
                  , ats = prev.ats
                  , python = prev.python
                  , vimscript = prev.vimscript
                  , elm = prev.elm
                  , idris = prev.idris
                  , madlang = prev.madlang
                  , tex = prev.tex
                  , markdown = prev.markdown
                  , yaml = prev.yaml
                  , toml = prev.toml
                  , cabal = prev.cabal
                  , happy = prev.happy + n
                  , alex = prev.alex
                  }
      in
        sc
      end
    | alex n =>
      let
        val sc = @{ rust = prev.rust
                  , haskell = prev.haskell
                  , ats = prev.ats
                  , python = prev.python
                  , vimscript = prev.vimscript
                  , elm = prev.elm
                  , idris = prev.idris
                  , madlang = prev.madlang
                  , tex = prev.tex
                  , markdown = prev.markdown
                  , yaml = prev.yaml
                  , toml = prev.toml
                  , cabal = prev.cabal
                  , happy = prev.happy
                  , alex = prev.alex + n
                  }
      in
        sc
      end
    | _ => prev

fun prune_extension(s: string): pl_type =
  let
    val (ext | str) = filename_get_ext(s)
    val match: string = 
      if strptr2ptr(str) > 0
        then
          $UN.strptr2string(str)
        else
          ""
    prval () = ext (str)
  in
    case match of
      | "hs" => haskell(line_count(s))
      | "hs-boot" => haskell(line_count(s))
      | "hsig" => haskell(line_count(s))
      | "rs" => rust(line_count(s))
      | "tex" => tex(line_count(s))
      | "md" => markdown(line_count(s))
      | "markdown" => markdown(line_count(s))
      | "ats" => ats(line_count(s))
      | "dats" => ats(line_count(s))
      | "hats" => ats(line_count(s))
      | "cats" => ats(line_count(s))
      | "py" => python(line_count(s))
      | "fut" => futhark(line_count(s))
      | "pl" => perl(line_count(s))
      | "agda" => agda(line_count(s))
      | "idr" => idris(line_count(s))
      | "v" => verilog(line_count(s))
      | "vhdl" => vhdl(line_count(s))
      | "vhd" => vhdl(line_count(s))
      | "go" => go(line_count(s))
      | "vim" => vimscript(line_count(s))
      | "ml" => ocaml(line_count(s))
      | "purs" => purescript(line_count(s))
      | "elm" => elm(line_count(s))
      | "mad" => madlang(line_count(s))
      | "toml" => toml(line_count(s))
      | "yaml" => yaml(line_count(s))
      | "cabal" => cabal(line_count(s))
      | "yml" => yaml(line_count(s))
      | "y" => happy(line_count(s))
      | "x" => alex(line_count(s))
      | _ => haskell(0)
  end

fun with_string(s: string, prev: source_contents): source_contents =
  let
    val pl = prune_extension(s)
  in
    adjust_contents(prev, pl)
  end

fnx traverse(ss: list0(string)) : source_contents =
  case ss of
    | list0_cons(x, xs) =>
      begin
        let
          val prev = traverse(xs)
        in
          with_string(x, prev)
        end
      end
    | _ => 
      begin
        let
          val isc = @{ rust = 0
                     , haskell = 0
                     , ats = 0
                     , python = 0
                     , vimscript = 0
                     , elm = 0
                     , idris = 0
                     , madlang = 0
                     , tex = 0
                     , markdown = 0
                     , yaml = 0
                     , toml = 0
                     , cabal = 0
                     , happy = 0
                     , alex = 0
                     } : source_contents
        in
          isc
        end
      end

fun starts_with(s: String) : char =
  if length(s) > 1 
    then string_head(s)
    else '.'

fun bad_dir(s: string) : bool =
  case s of
    | "." => true
    | ".." => true
    | ".pijul" => true
    | "_darcs" => true
    | ".git" => true
    | "target" => true
    | ".shake" => true
    | "dist-newstyle" => true
    | "dist" => true
    | ".psc-package" => true
    | ".pulp-cache" => true
    | "output" => true
    | "elm-stuff" => true
    | ".stack-work" => true
    | ".reco" => true
    | ".reco-work" => true
    | "forks" => true
    | "depends" => true
    | "nix" => true
    | _ => false

fun not_wrong(s: string) : bool =
  let 
    val fancy = $UN.cast{String}(s)
    val extra = test_file_isdir(s)
  in
    case starts_with(fancy) of
      | _ => extra = 0 // FIXME ignore hidden files?
  end

fun list0_empty{a:t0p}(x: list0(a)) : bool =
  case x of
    | list0_cons(_, _) => false
    | _ => true

fun flow_stream(s: string) : void =
  let
    val files = streamize_dirname_fname(s)
  in
    stream_vt_foreach_cloptr(files, lam x => print(x)) // FIXME figure out how tf to use folds.
  end

// FIXME this is a fucking disaster lol.
fnx dir_recursive(s: string): list0(string) =
    let
      val files = dirname_get_fnamelst(s)
      val subdirs: list0(string) = list0_filter(files, lam x => test_file_isdir(s + "/" + x) = 1 && not(bad_dir(x)))
    in
      if not(list0_empty(subdirs))
        then
          list0_concat(list0_cons(list0_map(list0_filter(files, lam x => not(bad_dir(x))), lam x => s + "/" + x), list0_map(subdirs, lam x => dir_recursive(s + "/" + x))))
        else
          list0_map(list0_filter(files, lam x => not(bad_dir(x))), lam x => s + "/" + x)
    end

fun get_dir_contents(s: string): list0(string) =
  let
    val files = dir_recursive(s)
  in
    list0_filter(files, lam x => not_wrong(x))
  end

fun should_help
  {n: int | n >= 1}
  {m: nat | m < n}
  ( argc: int n
  , argv: &(@[string][n])
  , current: int m ) : bool =
  let
    val arg = argv[current]
  in
    if current < argc - 1 then
      arg = "--help" || arg = "-h" || should_help(argc, argv, current + 1)
    else
      arg = "--help" || arg = "-h"
  end

fn help () = prerr "Usage: poly [OPTION] ... [DIRECTORY] ...

Count lines of source code and output a summary.
    -V, --version            show version information
    -h, --help               display this help and exit

When no directory is provided poly will execute in the
current directory.

Examples:
  poly ../        Summarize contents of parent directory.  
  
Bug reports and updates at
    nest.pijul.com/vamchale/polyglot\n"

implement main0 (argc, argv) =
  if argc > 1 then
    print(make_output(traverse(get_dir_contents(argv[1]))))
  else
    print(make_output(traverse(get_dir_contents("."))))
