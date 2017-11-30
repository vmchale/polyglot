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

fnx right_pad {k: int | k >= 0} .<k>. (s: string, n: int(k)) : string =
  case+ length(s) < n of
    | true when n > 0 => right_pad(s, n - 1) + " "
    | _ => s

fnx left_pad {k: int | k >= 0} .<k>. (s: string, n: int(k)) : string =
  case+ length(s) < n of
    | true when n > 0 => " " + left_pad(s, n - 1)
    | _ => s

fun maybe_string(s: string, n: int): string =
  if n > 0
    then
      s + ": " + tostring_int(n) + "\n"
    else
      ""

fun maybe_table(s: string, files: int, lines: int): string =
  if files > 0
    then
      right_pad(" " + s, 21) + left_pad(tostring_int(files), 5) + left_pad(tostring_int(lines), 13) + "            -            -            -\n"
  else
    ""

fun with_nonempty(s1: string, s2: string): string =
  if s2 != ""
    then
      s1 + s2
    else
      ""

fun sum_fields(sc: source_contents): file =
  let
    var f = @{ lines = sc.rust.lines +
                       sc.haskell.lines +
                       sc.ats.lines +
                       sc.python.lines +
                       sc.vimscript.lines +
                       sc.elm.lines +
                       sc.idris.lines +
                       sc.madlang.lines +
                       sc.tex.lines +
                       sc.markdown.lines +
                       sc.yaml.lines +
                       sc.toml.lines +
                       sc.cabal.lines +
                       sc.happy.lines +
                       sc.alex.lines +
                       sc.go.lines +
                       sc.html.lines +
                       sc.css.lines +
                       sc.verilog.lines +
                       sc.vhdl.lines +
                       sc.c.lines +
                       sc.purescript.lines +
                       sc.futhark.lines +
                       sc.brainfuck.lines +
                       sc.ruby.lines +
                       sc.julia.lines +
                       sc.perl.lines +
                       sc.ocaml.lines +
                       sc.agda.lines +
                       sc.cobol.lines +
                       sc.tcl.lines +
                       sc.r.lines +
                       sc.lua.lines +
                       sc.cpp.lines +
                       sc.lalrpop.lines +
                       sc.header.lines +
                       sc.sixten.lines +
                       sc.dhall.lines +
                       sc.ipkg.lines +
                       sc.makefile.lines +
                       sc.justfile.lines +
                       sc.ion.lines +
                       sc.bash.lines +
                       sc.hamlet.lines +
                       sc.cassius.lines +
                       sc.lucius.lines +
                       sc.julius.lines
             , files = sc.rust.files +
                       sc.haskell.files +
                       sc.ats.files +
                       sc.python.files +
                       sc.vimscript.files +
                       sc.elm.files +
                       sc.idris.files +
                       sc.madlang.files +
                       sc.tex.files +
                       sc.markdown.files +
                       sc.yaml.files +
                       sc.toml.files +
                       sc.cabal.files +
                       sc.happy.files +
                       sc.alex.files +
                       sc.go.files +
                       sc.html.files +
                       sc.css.files +
                       sc.verilog.files +
                       sc.vhdl.files +
                       sc.c.files +
                       sc.purescript.files +
                       sc.futhark.files +
                       sc.brainfuck.files +
                       sc.ruby.files +
                       sc.julia.files +
                       sc.perl.files +
                       sc.ocaml.files +
                       sc.agda.files +
                       sc.cobol.files +
                       sc.tcl.files +
                       sc.r.files +
                       sc.lua.files +
                       sc.cpp.files +
                       sc.lalrpop.files +
                       sc.header.files +
                       sc.sixten.files +
                       sc.dhall.files +
                       sc.ipkg.files +
                       sc.makefile.files +
                       sc.justfile.files +
                       sc.ion.files +
                       sc.bash.files +
                       sc.hamlet.files +
                       sc.cassius.files +
                       sc.lucius.files +
                       sc.julius.files
             }
  in
    f
  end

fun make_table(isc: source_contents): string =
  "-------------------------------------------------------------------------------\n [35mLanguage[0m            [35mFiles[0m        [35mLines[0m         [35mCode[0m     [35mComments[0m       [35mBlanks[0m\n-------------------------------------------------------------------------------\n" +
  maybe_table("Alex", isc.alex.files, isc.alex.lines) +
  maybe_table("Agda", isc.agda.files, isc.agda.lines) +
  maybe_table("ATS", isc.ats.files, isc.ats.lines) +
  maybe_table("Bash", isc.bash.files, isc.bash.lines) +
  maybe_table("Brainfuck", isc.brainfuck.files, isc.brainfuck.lines) +
  maybe_table("C", isc.c.files, isc.c.lines) +
  maybe_table("C Header", isc.header.files, isc.header.lines) +
  maybe_table("C++", isc.cpp.files, isc.cpp.lines) +
  maybe_table("Cabal", isc.cabal.files, isc.cabal.lines) +
  maybe_table("Cassius", isc.cassius.files, isc.cassius.lines) +
  maybe_table("COBOL", isc.cobol.files, isc.cobol.lines) +
  maybe_table("CSS", isc.css.files, isc.css.lines) +
  maybe_table("Dhall", isc.dhall.files, isc.dhall.lines) +
  maybe_table("Elm", isc.elm.files, isc.elm.lines) +
  maybe_table("Go", isc.go.files, isc.go.lines) +
  maybe_table("Hamlet", isc.hamlet.files, isc.hamlet.lines) +
  maybe_table("Happy", isc.happy.files, isc.happy.lines) +
  maybe_table("Haskell", isc.haskell.files, isc.haskell.lines) +
  maybe_table("HTML", isc.html.files, isc.html.lines) +
  maybe_table("Idris", isc.idris.files, isc.idris.lines) +
  maybe_table("iPKG", isc.ipkg.files, isc.ipkg.lines) +
  maybe_table("Ion", isc.ion.files, isc.ion.lines) +
  maybe_table("Julius", isc.julius.files, isc.julius.lines) +
  maybe_table("Julia", isc.julia.files, isc.julia.lines) +
  maybe_table("Justfile", isc.justfile.files, isc.justfile.lines) +
  maybe_table("LALRPOP", isc.lalrpop.files, isc.lalrpop.lines) +
  maybe_table("Lua", isc.lua.files, isc.lua.lines) +
  maybe_table("Lucius", isc.lucius.files, isc.lucius.lines) +
  maybe_table("Madlang", isc.madlang.files, isc.madlang.lines) +
  maybe_table("Makefile", isc.makefile.files, isc.makefile.lines) +
  maybe_table("Markdown", isc.markdown.files, isc.markdown.lines) +
  maybe_table("OCaml", isc.ocaml.files, isc.ocaml.lines) +
  maybe_table("Perl", isc.perl.files, isc.perl.lines) +
  maybe_table("Python", isc.python.files, isc.python.lines) +
  maybe_table("R", isc.r.files, isc.r.lines) +
  maybe_table("Ruby", isc.ruby.files, isc.ruby.lines) +
  maybe_table("Rust", isc.rust.files, isc.rust.lines) +
  maybe_table("Sixten", isc.sixten.files, isc.sixten.lines) +
  maybe_table("TCL", isc.tcl.files, isc.tcl.lines) +
  maybe_table("TeX", isc.tex.files, isc.tex.lines) +
  maybe_table("TOML", isc.toml.files, isc.toml.lines) +
  maybe_table("Verilog", isc.verilog.files, isc.verilog.lines) +
  maybe_table("VHDL", isc.vhdl.files, isc.vhdl.lines) +
  maybe_table("Vimscript", isc.vimscript.files, isc.vimscript.lines) +
  maybe_table("YAML", isc.yaml.files, isc.yaml.lines) +
  "-------------------------------------------------------------------------------\n" +
  maybe_table("Total", (sum_fields(isc)).files, (sum_fields(isc)).lines) +
  "-------------------------------------------------------------------------------\n"

fun make_output(isc: source_contents): string =
  with_nonempty("[33mProgramming Languages:[0m\n",
    maybe_string("Agda", isc.agda.lines) +
    maybe_string("ATS", isc.ats.lines) +
    maybe_string("Brainfuck", isc.brainfuck.lines) +
    maybe_string("C", isc.c.lines) +
    maybe_string("C Header", isc.header.lines) +
    maybe_string("C++", isc.cpp.lines) +
    maybe_string("COBOL", isc.cobol.lines) +
    maybe_string("Elm", isc.elm.lines) +
    maybe_string("Go", isc.go.lines) +
    maybe_string("Haskell", isc.haskell.lines) +
    maybe_string("Idris", isc.idris.lines) +
    maybe_string("Julia", isc.julia.lines) +
    maybe_string("Lua", isc.lua.lines) +
    maybe_string("OCaml", isc.ocaml.lines) +
    maybe_string("Perl", isc.perl.lines) +
    maybe_string("Python", isc.python.lines) +
    maybe_string("R", isc.r.lines) +
    maybe_string("Ruby", isc.ruby.lines) +
    maybe_string("Rust", isc.rust.lines) +
    maybe_string("Sixten", isc.sixten.lines) +
    maybe_string("TCL", isc.tcl.lines) +
    maybe_string("Vimscript", isc.vimscript.lines)
  ) +
  with_nonempty("\n[33mDocumentation:[0m\n",
    maybe_string("Markdown", isc.markdown.lines) +
    maybe_string("TeX", isc.tex.lines)
  ) +
  with_nonempty("\n[33mConfiguration:[0m\n",
    maybe_string("Cabal", isc.cabal.lines) +
    maybe_string("Dhall", isc.dhall.lines) +
    maybe_string("iPKG", isc.ipkg.lines) +
    maybe_string("TOML", isc.toml.lines) +
    maybe_string("YAML", isc.yaml.lines)
  ) +
  with_nonempty("\n[33mShell:[0m\n",
    maybe_string("Ion", isc.ion.lines) +
    maybe_string("Bash", isc.bash.lines)
  ) +
  with_nonempty("\n[33mParser Generators:[0m\n",
    maybe_string("Alex", isc.alex.lines) +
    maybe_string("Happy", isc.happy.lines) +
    maybe_string("LALRPOP", isc.lalrpop.lines)
  ) +
  with_nonempty("\n[33mWeb:[0m\n",
    maybe_string("Cassius", isc.cassius.lines) +
    maybe_string("CSS", isc.css.lines) +
    maybe_string("Hamlet", isc.hamlet.lines) +
    maybe_string("HTML", isc.html.lines) +
    maybe_string("Julius", isc.julius.lines) +
    maybe_string("Lucius", isc.lucius.lines)
  ) +
  with_nonempty("\n[33mHardware:[0m\n",
    maybe_string("Verilog", isc.verilog.lines) +
    maybe_string("VHDL", isc.vhdl.lines)
  ) +
  with_nonempty("\n[33mOther:[0m\n",
    maybe_string("Justfile", isc.justfile.lines) +
    maybe_string("Madlang", isc.madlang.lines) +
    maybe_string("Makefile", isc.makefile.lines)
  )

// fun detect_happy( 

fun add_results(x: file, y: file): file =
  let
    val next = @{ lines = x.lines + y.lines
                , files = x.files + y.files 
                }
  in
    next
  end

overload + with add_results

fun adjust_contents(prev: source_contents, scf: pl_type) : source_contents =
  let
    val sc_r = ref<source_contents>(prev)
  in
    case+ scf of
      | haskell n => 
        let
          val () = sc_r->haskell := prev.haskell + @{ lines = n, files = 1} // @{ lines = n, files = 1}
        in
          !sc_r
        end
      | ats n => 
        let
          val () = sc_r->ats := prev.ats + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | rust n => 
        let
          val () = sc_r->rust := prev.rust + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | markdown n => 
        let
          val () = sc_r->markdown := prev.markdown + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | python n => 
        let
          val () = sc_r->python := prev.python + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | vimscript n => 
        let
          val () = sc_r->vimscript := prev.vimscript + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | yaml n => 
        let
          val () = sc_r->yaml := prev.yaml + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | toml n => 
        let
          val () = sc_r->toml := prev.toml + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | happy n => 
        let
          val () = sc_r->happy := prev.happy + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | alex n => 
        let
          val () = sc_r->alex := prev.alex + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | idris n => 
        let
          val () = sc_r->idris := prev.idris + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | madlang n => 
        let
          val () = sc_r->madlang := prev.madlang + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | elm n => 
        let
          val () = sc_r->elm := prev.elm + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | c n => 
        let
          val () = sc_r->c := prev.c + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | go n => 
        let
          val () = sc_r->go := prev.go + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | cabal n => 
        let
          val () = sc_r->cabal := prev.cabal + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | verilog n => 
        let
          val () = sc_r->verilog := prev.verilog + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | vhdl n => 
        let
          val () = sc_r->vhdl := prev.vhdl + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | html n => 
        let
          val () = sc_r->html := prev.html + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | css n => 
        let
          val () = sc_r->css := prev.css + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | purescript n => 
        let
          val () = sc_r->purescript := prev.purescript + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | futhark n => 
        let
          val () = sc_r->futhark := prev.futhark + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | brainfuck n => 
        let
          val () = sc_r->brainfuck := prev.brainfuck + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | ruby n => 
        let
          val () = sc_r->ruby := prev.ruby + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | julia n => 
        let
          val () = sc_r->julia := prev.julia + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | tex n => 
        let
          val () = sc_r->tex := prev.tex + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | perl n => 
        let
          val () = sc_r->perl := prev.perl + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | ocaml n => 
        let
          val () = sc_r->ocaml := prev.ocaml + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | agda n => 
        let
          val () = sc_r->agda := prev.agda + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | cobol n => 
        let
          val () = sc_r->cobol := prev.cobol + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | tcl n => 
        let
          val () = sc_r->tcl := prev.tcl + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | r n => 
        let
          val () = sc_r->r := prev.r + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | lua n => 
        let
          val () = sc_r->lua := prev.lua + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | cpp n => 
        let
          val () = sc_r->cpp := prev.cpp + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | lalrpop n => 
        let
          val () = sc_r->lalrpop := prev.lalrpop + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | header n => 
        let
          val () = sc_r->header := prev.header + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | sixten n => 
        let
          val () = sc_r->sixten := prev.sixten + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | dhall n => 
        let
          val () = sc_r->dhall := prev.dhall + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | ipkg n => 
        let
          val () = sc_r->ipkg := prev.ipkg + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | justfile n => 
        let
          val () = sc_r->justfile := prev.justfile + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | makefile n => 
        let
          val () = sc_r->makefile := prev.makefile + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | ion n => 
        let
          val () = sc_r->ion := prev.ion + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | bash n => 
        let
          val () = sc_r->bash := prev.bash + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | hamlet n => 
        let
          val () = sc_r->hamlet := prev.hamlet + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | cassius n => 
        let
          val () = sc_r->cassius := prev.cassius + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | lucius n => 
        let
          val () = sc_r->lucius := prev.lucius + @{ lines = n, files = 1}
        in
          !sc_r
        end
      | julius n => 
        let
          val () = sc_r->julius := prev.julius + @{ lines = n, files = 1}
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
            val s = strptr2string(fileref_get_line_string(x))
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
      | "hamlet" => hamlet(line_count(s))
      | "cassius" => cassius(line_count(s))
      | "lucius" => cassius(line_count(s))
      | "julius" => julius(line_count(s))
      | "jl" => julius(line_count(s))
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

fun empty_file(): file =
  let
    var f = @{ files = 0
             , lines = 0
             } : file
  in
    f
  end

implement main0 (argc, argv) =
  let 
    var should_help = detect_flag(argc, argv, 0, "--help", "-h")
    var isc = @{ rust = empty_file()
               , haskell = empty_file()
               , ats = empty_file()
               , python = empty_file()
               , vimscript = empty_file()
               , elm = empty_file()
               , idris = empty_file()
               , madlang = empty_file()
               , tex = empty_file()
               , markdown = empty_file()
               , yaml = empty_file()
               , toml = empty_file()
               , cabal = empty_file()
               , happy = empty_file()
               , alex = empty_file()
               , go = empty_file()
               , html = empty_file()
               , css = empty_file()
               , verilog = empty_file()
               , vhdl = empty_file()
               , c = empty_file()
               , purescript = empty_file()
               , futhark = empty_file()
               , brainfuck = empty_file()
               , ruby = empty_file()
               , julia = empty_file()
               , perl = empty_file()
               , ocaml = empty_file()
               , agda = empty_file()
               , cobol = empty_file()
               , tcl = empty_file()
               , r = empty_file()
               , lua = empty_file()
               , cpp = empty_file()
               , lalrpop = empty_file()
               , header = empty_file()
               , sixten = empty_file()
               , dhall = empty_file()
               , ipkg = empty_file()
               , makefile = empty_file()
               , justfile = empty_file()
               , ion = empty_file()
               , bash = empty_file()
               , hamlet = empty_file()
               , cassius = empty_file()
               , lucius = empty_file()
               , julius = empty_file()
               } : source_contents
  in
    if not(should_help || detect_flag(argc, argv, 0, "--version", "-V")) then
      if argc = ~1 then
        print(make_output(traverse(get_dir_contents("."), isc))) // why is this necessary???
      else if argc > 1 then
        if argv[1] != "--excludes" && argv[1] != "-e" then
          if detect_flag(argc, argv, 0, "-t", "--table") then
            print(make_table(step_stream(isc, argv[1], detect_excludes(argc, argv, 0, false))))
          else
            print(make_output(step_stream(isc, argv[1], detect_excludes(argc, argv, 0, false))))
        else
          if detect_flag(argc, argv, 0, "-t", "--table") then
            print(make_table(step_stream(isc, ".", detect_excludes(argc, argv, 0, false))))
          else
            print(make_output(step_stream(isc, ".", detect_excludes(argc, argv, 0, false))))
      else
        if detect_flag(argc, argv, 0, "-t", "--table") then
          print(make_table(step_stream(isc, ".", detect_excludes(argc, argv, 0, false))))
        else
          print(make_output(step_stream(isc, ".", detect_excludes(argc, argv, 0, false))))
    else
      if should_help then
        help()
      else
        version()
  end
