#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
#include "share/HATS/atslib_staload_libats_libc.hats"
#include "prelude/DATS/filebas.dats"
#include "libats/ML/DATS/filebas_dirent.dats"
#include "libats/libc/DATS/dirent.dats"
#include "libats/ML/DATS/list0.dats"

%{^
#include "libats/libc/CATS/string.cats"
#include "prelude/CATS/filebas.cats"
%}

(* ****** ****** *)
staload "prelude/DATS/unsafe.dats"
staload "libats/ML/DATS/string.dats"
staload "libats/libc/SATS/stdio.sats"
staload "prelude/SATS/filebas.sats"
staload "src/filetype.sats"
staload "libats/ML/DATS/filebas.dats"
staload EXTRA = "libats/ML/SATS/filebas.sats"
(* ****** ****** *)

fun line_count(s: string): int =
  let
    val ref = fileref_open_opt(s, file_mode_r)
  in
    case+ ref of
      | ~Some_vt(x) => 
        begin
          let
            val viewstream: stream_vt(string) = $EXTRA.streamize_fileref_line(x)
            val n: int = stream_vt_length(viewstream) - 1
            val _ = fileref_close(x)
          in
            n
          end
        end
      | ~None_vt() => 0
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
    maybe_string("ATS", isc.ats) +
    maybe_string("Brainfuck", isc.brainfuck) +
    maybe_string("C", isc.c) +
    maybe_string("C Header", isc.header) +
    maybe_string("C++", isc.cpp) +
    maybe_string("COBOL", isc.cobol) +
    maybe_string("Elm", isc.elm) +
    maybe_string("Go", isc.go) +
    maybe_string("Haskell", isc.haskell) +
    maybe_string("Idris", isc.idris) +
    maybe_string("Madlang", isc.madlang) +
    maybe_string("Lua", isc.lua) +
    maybe_string("OCaml", isc.ocaml) +
    maybe_string("Perl", isc.perl) +
    maybe_string("Python", isc.python) +
    maybe_string("R", isc.r) +
    maybe_string("Ruby", isc.ruby) +
    maybe_string("Rust", isc.rust) +
    maybe_string("Sixten", isc.sixten) +
    maybe_string("TCL", isc.tcl) +
    maybe_string("Vimscript", isc.vimscript)
  ) +
  with_nonempty("\n[33mDocumentation:[0m\n",
    maybe_string("Markdown", isc.markdown) +
    maybe_string("TeX", isc.tex)
  ) +
  with_nonempty("\n[33mConfiguration:[0m\n",
    maybe_string("Cabal", isc.cabal) +
    maybe_string("Dhall", isc.dhall) +
    maybe_string("iPKG", isc.ipkg) +
    maybe_string("TOML", isc.toml) +
    maybe_string("YAML", isc.yaml)
  ) +
  with_nonempty("\n[33mShell:[0m\n",
    maybe_string("Ion", isc.ion) +
    maybe_string("Bash", isc.bash)
  ) +
  with_nonempty("\n[33mParser Generators:[0m\n",
    maybe_string("Alex", isc.alex) +
    maybe_string("Happy", isc.happy) +
    maybe_string("LALRPOP", isc.lalrpop)
  ) +
  with_nonempty("\n[33mWeb:[0m\n",
    maybe_string("CSS", isc.css) +
    maybe_string("HTML", isc.html)
  ) +
  with_nonempty("\n[33mHardware:[0m\n",
    maybe_string("Verilog", isc.verilog) +
    maybe_string("VHDL", isc.vhdl)
  ) +
  with_nonempty("\n[33mOther:[0m\n",
    maybe_string("Justfile", isc.justfile) +
    maybe_string("Madlang", isc.madlang) +
    maybe_string("Makefile", isc.makefile)
  )

// fun detect_happy( 

fun adjust_contents(prev: source_contents, scf: pl_type) : source_contents =
  let
    val sc_r = ref<source_contents>(prev)
  in
    case+ scf of
      | haskell n => 
        let
          val () = sc_r->haskell := prev.haskell + n
        in
          !sc_r
        end
      | ats n => 
        let
          val () = sc_r->ats := prev.ats + n
        in
          !sc_r
        end
      | rust n => 
        let
          val () = sc_r->rust := prev.rust + n
        in
          !sc_r
        end
      | markdown n => 
        let
          val () = sc_r->markdown := prev.markdown + n
        in
          !sc_r
        end
      | python n => 
        let
          val () = sc_r->python := prev.python + n
        in
          !sc_r
        end
      | vimscript n => 
        let
          val () = sc_r->vimscript := prev.vimscript + n
        in
          !sc_r
        end
      | yaml n => 
        let
          val () = sc_r->yaml := prev.yaml + n
        in
          !sc_r
        end
      | toml n => 
        let
          val () = sc_r->toml := prev.toml + n
        in
          !sc_r
        end
      | happy n => 
        let
          val () = sc_r->happy := prev.happy + n
        in
          !sc_r
        end
      | alex n => 
        let
          val () = sc_r->alex := prev.alex + n
        in
          !sc_r
        end
      | idris n => 
        let
          val () = sc_r->idris := prev.idris + n
        in
          !sc_r
        end
      | madlang n => 
        let
          val () = sc_r->madlang := prev.madlang + n
        in
          !sc_r
        end
      | elm n => 
        let
          val () = sc_r->elm := prev.elm + n
        in
          !sc_r
        end
      | c n => 
        let
          val () = sc_r->c := prev.c + n
        in
          !sc_r
        end
      | go n => 
        let
          val () = sc_r->go := prev.go + n
        in
          !sc_r
        end
      | cabal n => 
        let
          val () = sc_r->cabal := prev.cabal + n
        in
          !sc_r
        end
      | verilog n => 
        let
          val () = sc_r->verilog := prev.verilog + n
        in
          !sc_r
        end
      | vhdl n => 
        let
          val () = sc_r->vhdl := prev.vhdl + n
        in
          !sc_r
        end
      | html n => 
        let
          val () = sc_r->html := prev.html + n
        in
          !sc_r
        end
      | css n => 
        let
          val () = sc_r->css := prev.css + n
        in
          !sc_r
        end
      | purescript n => 
        let
          val () = sc_r->purescript := prev.purescript + n
        in
          !sc_r
        end
      | futhark n => 
        let
          val () = sc_r->futhark := prev.futhark + n
        in
          !sc_r
        end
      | brainfuck n => 
        let
          val () = sc_r->brainfuck := prev.brainfuck + n
        in
          !sc_r
        end
      | ruby n => 
        let
          val () = sc_r->ruby := prev.ruby + n
        in
          !sc_r
        end
      | julia n => 
        let
          val () = sc_r->julia := prev.julia + n
        in
          !sc_r
        end
      | tex n => 
        let
          val () = sc_r->tex := prev.tex + n
        in
          !sc_r
        end
      | perl n => 
        let
          val () = sc_r->perl := prev.perl + n
        in
          !sc_r
        end
      | ocaml n => 
        let
          val () = sc_r->ocaml := prev.ocaml + n
        in
          !sc_r
        end
      | agda n => 
        let
          val () = sc_r->agda := prev.agda + n
        in
          !sc_r
        end
      | cobol n => 
        let
          val () = sc_r->cobol := prev.cobol + n
        in
          !sc_r
        end
      | tcl n => 
        let
          val () = sc_r->tcl := prev.tcl + n
        in
          !sc_r
        end
      | r n => 
        let
          val () = sc_r->r := prev.r + n
        in
          !sc_r
        end
      | lua n => 
        let
          val () = sc_r->lua := prev.lua + n
        in
          !sc_r
        end
      | cpp n => 
        let
          val () = sc_r->cpp := prev.cpp + n
        in
          !sc_r
        end
      | lalrpop n => 
        let
          val () = sc_r->lalrpop := prev.lalrpop + n
        in
          !sc_r
        end
      | header n => 
        let
          val () = sc_r->header := prev.header + n
        in
          !sc_r
        end
      | sixten n => 
        let
          val () = sc_r->sixten := prev.sixten + n
        in
          !sc_r
        end
      | dhall n => 
        let
          val () = sc_r->dhall := prev.dhall + n
        in
          !sc_r
        end
      | ipkg n => 
        let
          val () = sc_r->ipkg := prev.ipkg + n
        in
          !sc_r
        end
      | justfile n => 
        let
          val () = sc_r->justfile := prev.justfile + n
        in
          !sc_r
        end
      | makefile n => 
        let
          val () = sc_r->makefile := prev.makefile + n
        in
          !sc_r
        end
      | ion n => 
        let
          val () = sc_r->ion := prev.ion + n
        in
          !sc_r
        end
      | bash n => 
        let
          val () = sc_r->bash := prev.bash + n
        in
          !sc_r
        end
      | unknown _ => prev
  end

fun check_shebang(s: string): pl_type =
  let
    val ref = fileref_open_opt(s, file_mode_r)
    val str: string =
      case+ ref of
        | ~Some_vt(x) => 
          let
            val s = strptr2string(fileref_get_line_string(x)) // type Strptr1
            val _ = fileref_close(x)
          in
            s
          end
        | ~None_vt() => ""
  in
    case str of
      | "#!/usr/bin/env ion" => ion(line_count(s))
      | "#!/usr/bin/env bash" => bash(line_count(s))
      | "#!/bin/bash" => bash(line_count(s))
      | "#!/usr/bin/env python" => python(line_count(s))
      | "#!/usr/bin/env python2" => python(line_count(s))
      | "#!/usr/bin/env python3" => python(line_count(s))
      | "#!/usr/bin/env perl" => perl(line_count(s))
      | "#!/usr/bin/env perl6" => perl(line_count(s))
      | "#!/usr/bin/perl" => perl(line_count(s))
      // | "#!/usr/bin/env node" 
      | _ => unknown
  end

fun match_filename(s: string): pl_type =
  let
    val (prf | str) = filename_get_base(s)
    val match = $UN.strptr2string(str)
    prval () = prf(str)
  in
    case match of
      | "Makefile" => makefile(line_count(s))
      | "makefile" => makefile(line_count(s))
      | "GNUmakefile" => makefile(line_count(s))
      | "Justfile" => justfile(line_count(s))
      | "justfile" => justfile(line_count(s))
      | _ => check_shebang(s)
  end

fun prune_extension(s: string): pl_type =
  let
    val (prf | str) = filename_get_ext(s)
    val match: string = 
      if strptr2ptr(str) > 0
        then
          $UN.strptr2string(str)
        else
          ""
    prval () = prf (str)
  in
    case match of
      | "hs" => haskell(line_count(s))
      | "hs-boot" => haskell(line_count(s))
      | "hsig" => haskell(line_count(s))
      | "rs" => rust(line_count(s))
      | "tex" => tex(line_count(s))
      | "md" => markdown(line_count(s))
      | "markdown" => markdown(line_count(s))
      | "dats" => ats(line_count(s))
      | "hats" => ats(line_count(s))
      | "cats" => ats(line_count(s))
      | "sats" => ats(line_count(s))
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
      | "go" => go(line_count(s))
      | "html" => html(line_count(s))
      | "htm" => html(line_count(s))
      | "css" => css(line_count(s))
      | "v" => verilog(line_count(s))
      | "vhdl" => vhdl(line_count(s))
      | "vhd" => vhdl(line_count(s))
      | "c" => c(line_count(s))
      | "b" => brainfuck(line_count(s))
      | "bf" => brainfuck(line_count(s))
      | "rb" => ruby(line_count(s))
      | "cob" => cobol(line_count(s))
      | "ml" => ocaml(line_count(s))
      | "tcl" => tcl(line_count(s))
      | "r" => r(line_count(s))
      | "lua" => lua(line_count(s))
      | "cpp" => cpp(line_count(s))
      | "cc" => cpp(line_count(s))
      | "lalrpop" => lalrpop(line_count(s))
      | "h" => header(line_count(s))
      | "vix" => sixten(line_count(s))
      | "dhall" => dhall(line_count(s))
      | "ipkg" => ipkg(line_count(s))
      | "mk" => makefile(line_count(s))
      | _ => match_filename(s)
  end

fun with_string(s: string, prev: source_contents): source_contents =
  let
    var pl = prune_extension(s)
  in
    adjust_contents(prev, pl)
  end

// TODO if we can do this on a stream it's nicer.
fnx traverse(ss: list0(string), initial: source_contents) : source_contents =
  case+ ss of
    | list0_cons(x, xs) =>
      begin
        let
          var prev = traverse(xs, initial)
        in
          with_string(x, prev)
        end
      end
    | _ => initial

fun starts_with(s: String) : char =
  if length(s) > 1 
    then string_head(s)
    else '.'

// TODO take an array of bad directories from CLI.
fun bad_dir(s: string, excludes: list0(string)) : bool =
  case s of
    | "." => true
    | ".." => true
    | ".pijul" => true
    | "_darcs" => true
    | ".hg" => true
    | ".git" => true
    | "target" => true // Rust, etc.
    | ".egg-info" => true
    | "nimcache" => true
    | ".shake" => true
    | "dist-newstyle" => true
    | "dist" => true
    | ".psc-package" => true
    | ".pulp-cache" => true
    | "output" => true // pulp
    | "bower_components" => true
    | "elm-stuff" => true
    | ".stack-work" => true
    | ".reco" => true
    | ".reco-work" => true
    | ".cabal-sandbox" => true
    | "node_modules" => true
    | ".lein-plugins" => true
    | ".sass-cache" => true
    | _ => list0_exists(excludes, lam x => x = s) // FIXME this is inefficient

fun not_wrong(s: string) : bool =
  let 
    val extra = test_file_isdir(s)
  in
    case extra of
      | 0 => true
      | _ => false
  end

fnx step_stream(acc: source_contents, s: string, excludes: list0(string)) : source_contents =
  if test_file_isdir(s) = 1 then
    flow_stream(s, acc, excludes)
  else
    adjust_contents(acc, prune_extension(s))

and flow_stream(s: string, init: source_contents, excludes: list0(string)) : source_contents =
  let
    val files = streamize_dirname_fname(s)
  in
    stream_vt_foldleft_cloptr( files
                             , init
                             , lam (acc, next) => if not(bad_dir(next, excludes)) then step_stream(acc, s + "/" + next, excludes) else acc
                             )
  end

fnx dir_recursive(s: string): list0(string) =
    let
      val files = dirname_get_fnamelst(s)
      val subdirs: list0(string) = list0_filter(files, lam x => test_file_isdir(s + "/" + x) = 1 && not(bad_dir(x, list0_nil)))
    in
      if not(list0_is_nil(subdirs))
        then
          list0_concat(list0_cons(list0_map(list0_filter(files, lam x => not(bad_dir(x, list0_nil))), lam x => s + "/" + x), list0_map(subdirs, lam x => dir_recursive(s + "/" + x))))
        else
          list0_map(list0_filter(files, lam x => not(bad_dir(x, list0_nil))), lam x => s + "/" + x)
    end

// TODO this is quite inefficient; a fold would be nice.
fun get_dir_contents(s: string): list0(string) =
  let
    val files = dir_recursive(s)
  in
    list0_filter(files, lam x => not_wrong(x))
  end

fnx detect_excludes
  {n: int | n >= 1}
  {m: nat | m < n}
  ( argc: int n
  , argv: !argv(n)
  , current: int m
  , skip_rest: bool
  ) : list0(string) =
  let
    val arg = argv[current]
  in
    if current < argc - 1 then
      if skip_rest then
        list0_cons(arg, detect_excludes(argc, argv, current + 1, false))
      else
        if arg = "--exclude" || arg = "-e" then
          detect_excludes(argc, argv, current + 1, true)
        else
          detect_excludes(argc, argv, current + 1, skip_rest)
    else
      if skip_rest then
        list0_cons(arg, list0_nil)
      else
        list0_nil
  end

fnx detect_flag
  {n: int | n >= 1}
  {m: nat | m < n}
  ( argc: int n
  , argv: !argv(n)
  , current: int m
  , long: string
  , short: string
  ) : bool =
  let
    val arg = argv[current]
  in
    if current < argc - 1 then
      arg = long || arg = short || detect_flag(argc, argv, current + 1, long, short)
    else
      arg = long || arg = short
  end

fun version(): void =
  println!("polygot version 0.1.0\nCopyright (c) 2017 Vanessa McHale")

fun help(): void = 
print("polyglot - Count lines of code quickly.

[36mUSAGE:[0m poly [DIRECTORY] ... [OPTION] ...

[36mFLAGS:[0m
    -V, --version            show version information
    -h, --help               display this help and exit
    -e, --exclude            exclude a directory

When no directory is provided poly will execute in the
current directory.

Bug reports and updates: nest.pijul.com/vamchale/polyglot\n")

implement main0 (argc, argv) =
  let 
    val should_help = detect_flag(argc, argv, 0, "--help", "-h")
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
               , go = 0
               , html = 0
               , css = 0
               , verilog = 0
               , vhdl = 0
               , c = 0
               , purescript = 0
               , futhark = 0
               , brainfuck = 0
               , ruby = 0
               , julia = 0
               , perl = 0
               , ocaml = 0
               , agda = 0
               , cobol = 0
               , tcl = 0
               , r = 0
               , lua = 0
               , cpp = 0
               , lalrpop = 0
               , header = 0
               , sixten = 0
               , dhall = 0
               , ipkg = 0
               , makefile = 0
               , justfile = 0
               , ion = 0
               , bash = 0
               } : source_contents
  in
    if not(should_help || detect_flag(argc, argv, 0, "--version", "-V")) then
      if argc = ~1 then
        print(make_output(traverse(get_dir_contents("."), isc))) // why is this necessary???
      else if argc > 1 then
        if argv[1] != "--excludes" && argv[1] != "-e" then
          print(make_output(step_stream(isc, argv[1], detect_excludes(argc, argv, 0, false))))
        else
          print(make_output(step_stream(isc, ".", detect_excludes(argc, argv, 0, false))))
      else
        print(make_output(step_stream(isc, ".", detect_excludes(argc, argv, 0, false))))
    else
      if should_help then
        help()
      else
        version()
  end
