#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
#include "share/HATS/atslib_staload_libats_libc.hats"
#include "prelude/DATS/filebas.dats"
#include "libats/ML/DATS/filebas_dirent.dats"
#include "libats/libc/DATS/dirent.dats" // causes problematic include "share/H/pats_atslib.h"
#include "libats/ML/DATS/list0.dats"
#include "libats/DATS/athread_posix.dats"

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
staload "libats/SATS/athread.sats"
staload EXTRA = "libats/ML/SATS/filebas.sats"
(* ****** ****** *)

// TODO think of parallelism/concurrency approach - MS queue wouldn't be exactly perfect.
// Conversely, channels could be slower than desired.
// Want: three workers traversing directories, one 

// Given a string representing a filepath, return an integer.
fun line_count(s: string): int =
  let
    var ref = fileref_open_opt(s, file_mode_r)
  in
    case ref of
      | ~Some_vt(x) => 
        begin
          let
            var viewstream: stream_vt(string) = $EXTRA.streamize_fileref_line(x)
            var n: int = stream_vt_length(viewstream) - 1
            val _ = fileref_close(x)
          in
            n
          end
        end
      | ~None_vt() => (println!("[33mWarning:[0m could not open file at " + s) ; 0)
  end

// Pad a string of bounded length on the right by adding spaces.
fnx right_pad { k: int | k >= 0 }{ m: int | m <= k } .<k>. (s: string(m), n: int(k)) : string =
  case+ length(s) < n of
    | true when n > 0 => right_pad(s, n - 1) + " "
    | _ => s

// Pad a string on the left by adding spaces.
fnx left_pad { k: int | k >= 0 } .<k>. (s : string, n : int(k)) : string =
  case+ length(s) < n of
    | true when n > 0 => " " + left_pad(s, n - 1)
    | _ => s

// helper function for make_output
fun maybe_string(s: string, n: int): string =
  if n > 0
    then
      s + ": " + tostring_int(n) + "\n"
    else
      ""

// helper function for make_table
fun maybe_table{ k : int | k >= 0 && k < 20 }(s: string(k), files: int, lines: int): string =
  if files > 0
    then
      " " + right_pad(s, 21) + left_pad(tostring_int(files), 5) + left_pad(tostring_int(lines), 13) + "           -            -            -\n"
  else
    ""

// helper function for make_output
fun with_nonempty(s1: string, s2: string): string =
  if s2 != ""
    then
      s1 + s2
    else
      ""

// helper function to make totals for tabular output.
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
                       sc.julius.lines +
                       sc.mercury.lines +
                       sc.yacc.lines +
                       sc.lex.lines +
                       sc.coq.lines +
                       sc.jupyter.lines +
                       sc.java.lines +
                       sc.scala.lines +
                       sc.erlang.lines +
                       sc.elixir.lines +
                       sc.pony.lines +
                       sc.clojure.lines +
                       sc.cabal_project.lines +
                       sc.assembly.lines +
                       sc.nix.lines +
                       sc.php.lines +
                       sc.javascript.lines +
                       sc.kotlin.lines +
                       sc.fsharp.lines +
                       sc.fortran.lines +
                       sc.swift.lines +
                       sc.csharp.lines +
                       sc.nim.lines +
                       sc.cpp_header.lines +
                       sc.elisp.lines +
                       sc.plaintext.lines +
                       sc.rakefile.lines +
                       sc.llvm.lines +
                       sc.autoconf.lines +
                       sc.batch.lines +
                       sc.powershell.lines +
                       sc.m4.lines
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
                       sc.julius.files +
                       sc.mercury.files +
                       sc.yacc.files +
                       sc.lex.files +
                       sc.coq.files +
                       sc.jupyter.files +
                       sc.java.files +
                       sc.scala.files +
                       sc.erlang.files +
                       sc.elixir.files +
                       sc.pony.files +
                       sc.clojure.files +
                       sc.cabal_project.files +
                       sc.assembly.files +
                       sc.nix.files +
                       sc.php.files +
                       sc.javascript.files +
                       sc.kotlin.files +
                       sc.fsharp.files +
                       sc.fortran.files +
                       sc.swift.files +
                       sc.csharp.files +
                       sc.nim.files +
                       sc.cpp_header.files +
                       sc.elisp.files +
                       sc.plaintext.files +
                       sc.rakefile.files +
                       sc.llvm.files +
                       sc.autoconf.files +
                       sc.batch.files +
                       sc.powershell.files +
                       sc.m4.files
             }
  in
    f
  end

// function to print tabular output at the end
fun make_table(isc: source_contents): string =
  "-------------------------------------------------------------------------------\n [35mLanguage[0m            [35mFiles[0m        [35mLines[0m         [35mCode[0m     [35mComments[0m       [35mBlanks[0m\n-------------------------------------------------------------------------------\n" +
  maybe_table("Alex", isc.alex.files, isc.alex.lines) +
  maybe_table("Agda", isc.agda.files, isc.agda.lines) +
  maybe_table("Assembly", isc.assembly.files, isc.assembly.lines) +
  maybe_table("ATS", isc.ats.files, isc.ats.lines) +
  maybe_table("Autoconf", isc.autoconf.files, isc.autoconf.lines) +
  maybe_table("Bash", isc.bash.files, isc.bash.lines) +
  maybe_table("Batch", isc.batch.files, isc.batch.lines) +
  maybe_table("Brainfuck", isc.brainfuck.files, isc.brainfuck.lines) +
  maybe_table("C", isc.c.files, isc.c.lines) +
  maybe_table("C Header", isc.header.files, isc.header.lines) +
  maybe_table("C++ cpp_header", isc.cpp_header.files, isc.cpp_header.lines) +
  maybe_table("C++", isc.cpp.files, isc.cpp.lines) +
  maybe_table("C#", isc.csharp.files, isc.csharp.lines) +
  maybe_table("Cabal", isc.cabal.files, isc.cabal.lines) +
  maybe_table("Cabal Project", isc.cabal_project.files, isc.cabal_project.lines) +
  maybe_table("Cassius", isc.cassius.files, isc.cassius.lines) +
  maybe_table("COBOL", isc.cobol.files, isc.cobol.lines) +
  maybe_table("Coq", isc.coq.files, isc.coq.lines) +
  maybe_table("CSS", isc.css.files, isc.css.lines) +
  maybe_table("Dhall", isc.dhall.files, isc.dhall.lines) +
  maybe_table("Elixir", isc.elixir.files, isc.elixir.lines) +
  maybe_table("Elm", isc.elm.files, isc.elm.lines) +
  maybe_table("Emacs Lisp", isc.elisp.files, isc.elisp.lines) +
  maybe_table("Erlang", isc.erlang.files, isc.erlang.lines) +
  maybe_table("F#", isc.fsharp.files, isc.fsharp.lines) +
  maybe_table("Fortran", isc.fortran.files, isc.fortran.lines) +
  maybe_table("Go", isc.go.files, isc.go.lines) +
  maybe_table("Hamlet", isc.hamlet.files, isc.hamlet.lines) +
  maybe_table("Happy", isc.happy.files, isc.happy.lines) +
  maybe_table("Haskell", isc.haskell.files, isc.haskell.lines) +
  maybe_table("HTML", isc.html.files, isc.html.lines) +
  maybe_table("Idris", isc.idris.files, isc.idris.lines) +
  maybe_table("iPKG", isc.ipkg.files, isc.ipkg.lines) +
  maybe_table("Ion", isc.ion.files, isc.ion.lines) +
  maybe_table("Java", isc.java.files, isc.java.lines) +
  maybe_table("JavaScript", isc.javascript.files, isc.javascript.lines) +
  maybe_table("Julius", isc.julius.files, isc.julius.lines) +
  maybe_table("Julia", isc.julia.files, isc.julia.lines) +
  maybe_table("Jupyter", isc.jupyter.files, isc.jupyter.lines) +
  maybe_table("Justfile", isc.justfile.files, isc.justfile.lines) +
  maybe_table("Kotlin", isc.kotlin.files, isc.kotlin.lines) +
  maybe_table("LALRPOP", isc.lalrpop.files, isc.lalrpop.lines) +
  maybe_table("Lex", isc.lex.files, isc.lex.lines) +
  maybe_table("LLVM", isc.llvm.files, isc.llvm.lines) +
  maybe_table("Lua", isc.lua.files, isc.lua.lines) +
  maybe_table("Lucius", isc.lucius.files, isc.lucius.lines) +
  maybe_table("M4", isc.m4.files, isc.m4.lines) +
  maybe_table("Madlang", isc.madlang.files, isc.madlang.lines) +
  maybe_table("Makefile", isc.makefile.files, isc.makefile.lines) +
  maybe_table("Markdown", isc.markdown.files, isc.markdown.lines) +
  maybe_table("Nim", isc.nim.files, isc.nim.lines) +
  maybe_table("Nix", isc.nix.files, isc.nix.lines) +
  maybe_table("OCaml", isc.ocaml.files, isc.ocaml.lines) +
  maybe_table("Perl", isc.perl.files, isc.perl.lines) +
  maybe_table("PHP", isc.php.files, isc.php.lines) +
  maybe_table("Plaintext", isc.plaintext.files, isc.plaintext.lines) +
  maybe_table("PowerShell", isc.powershell.files, isc.powershell.lines) +
  maybe_table("Pony", isc.pony.files, isc.pony.lines) +
  maybe_table("Python", isc.python.files, isc.python.lines) +
  maybe_table("PureScript", isc.purescript.files, isc.purescript.lines) +
  maybe_table("R", isc.r.files, isc.r.lines) +
  maybe_table("Rakefile", isc.rakefile.files, isc.rakefile.lines) +
  maybe_table("Ruby", isc.ruby.files, isc.ruby.lines) +
  maybe_table("Rust", isc.rust.files, isc.rust.lines) +
  maybe_table("Scala", isc.scala.files, isc.scala.lines) +
  maybe_table("Sixten", isc.sixten.files, isc.sixten.lines) +
  maybe_table("Swift", isc.swift.files, isc.swift.lines) +
  maybe_table("TCL", isc.tcl.files, isc.tcl.lines) +
  maybe_table("TeX", isc.tex.files, isc.tex.lines) +
  maybe_table("TOML", isc.toml.files, isc.toml.lines) +
  maybe_table("Verilog", isc.verilog.files, isc.verilog.lines) +
  maybe_table("VHDL", isc.vhdl.files, isc.vhdl.lines) +
  maybe_table("Vimscript", isc.vimscript.files, isc.vimscript.lines) +
  maybe_table("Yacc", isc.yacc.files, isc.yacc.lines) +
  maybe_table("YAML", isc.yaml.files, isc.yaml.lines) +
  "-------------------------------------------------------------------------------\n" +
  maybe_table("Total", (sum_fields(isc)).files, (sum_fields(isc)).lines) +
  "-------------------------------------------------------------------------------\n"

// Function to print output sorted by type of language.
fun make_output(isc: source_contents): string =
  with_nonempty("[33mProgramming Languages:[0m\n",
    maybe_string("Agda", isc.agda.lines) +
    maybe_string("ATS", isc.ats.lines) +
    maybe_string("Brainfuck", isc.brainfuck.lines) +
    maybe_string("C", isc.c.lines) +
    maybe_string("C Header", isc.header.lines) +
    maybe_string("C++", isc.cpp.lines) +
    maybe_string("C++ Header", isc.cpp_header.lines) +
    maybe_string("C#", isc.csharp.lines) +
    maybe_string("COBOL", isc.cobol.lines) +
    maybe_string("Coq", isc.coq.lines) +
    maybe_string("Elixir", isc.elixir.lines) +
    maybe_string("Elm", isc.elm.lines) +
    maybe_string("Erlang", isc.elm.lines) +
    maybe_string("F#", isc.fsharp.lines) +
    maybe_string("Fortran", isc.fortran.lines) +
    maybe_string("Go", isc.go.lines) +
    maybe_string("Haskell", isc.haskell.lines) +
    maybe_string("Idris", isc.idris.lines) +
    maybe_string("Kotline", isc.kotlin.lines) +
    maybe_string("Java", isc.java.lines) +
    maybe_string("Julia", isc.julia.lines) +
    maybe_string("Lua", isc.lua.lines) +
    maybe_string("Nim", isc.nim.lines) +
    maybe_string("OCaml", isc.ocaml.lines) +
    maybe_string("Perl", isc.perl.lines) +
    maybe_string("Pony", isc.pony.lines) +
    maybe_string("PureScript", isc.purescript.lines) +
    maybe_string("Python", isc.python.lines) +
    maybe_string("R", isc.r.lines) +
    maybe_string("Ruby", isc.ruby.lines) +
    maybe_string("Rust", isc.rust.lines) +
    maybe_string("Scala", isc.scala.lines) +
    maybe_string("Sixten", isc.sixten.lines) +
    maybe_string("Swift", isc.swift.lines) +
    maybe_string("TCL", isc.tcl.lines)
  ) +
  with_nonempty("\n[33mEditor Plugins:[0m\n",
    maybe_string("Emacs Lisp", isc.elisp.lines) +
    maybe_string("Vimscript", isc.vimscript.lines)
  ) +
  with_nonempty("\n[33mDocumentation:[0m\n",
    maybe_string("Markdown", isc.markdown.lines) +
    maybe_string("Plaintext", isc.plaintext.lines) +
    maybe_string("TeX", isc.tex.lines)
  ) +
  with_nonempty("\n[33mConfiguration:[0m\n",
    maybe_string("Cabal", isc.cabal.lines) +
    maybe_string("cabal Project", isc.cabal_project.lines) +
    maybe_string("Dhall", isc.dhall.lines) +
    maybe_string("iPKG", isc.ipkg.lines) +
    maybe_string("TOML", isc.toml.lines) +
    maybe_string("YAML", isc.yaml.lines)
  ) +
  with_nonempty("\n[33mShell:[0m\n",
    maybe_string("Bash", isc.bash.lines) +
    maybe_string("Batch", isc.batch.lines) +
    maybe_string("Ion", isc.ion.lines) +
    maybe_string("PowerShell", isc.powershell.lines)
  ) +
  with_nonempty("\n[33mParser Generators:[0m\n",
    maybe_string("Alex", isc.alex.lines) +
    maybe_string("Happy", isc.happy.lines) +
    maybe_string("LALRPOP", isc.lalrpop.lines) +
    maybe_string("Lex", isc.lex.lines) +
    maybe_string("Yacc", isc.yacc.lines)
  ) +
  with_nonempty("\n[33mWeb:[0m\n",
    maybe_string("Cassius", isc.cassius.lines) +
    maybe_string("CSS", isc.css.lines) +
    maybe_string("Hamlet", isc.hamlet.lines) +
    maybe_string("HTML", isc.html.lines) +
    maybe_string("JavaScript", isc.javascript.lines) +
    maybe_string("Julius", isc.julius.lines) +
    maybe_string("Lucius", isc.lucius.lines)
  ) +
  with_nonempty("\n[33mHardware:[0m\n",
    maybe_string("Verilog", isc.verilog.lines) +
    maybe_string("VHDL", isc.vhdl.lines)
  ) +
  with_nonempty("\n[33mOther:[0m\n",
    maybe_string("Autoconf", isc.autoconf.lines) +
    maybe_string("Jupyter", isc.jupyter.lines) +
    maybe_string("Justfile", isc.justfile.lines) +
    maybe_string("LLVM", isc.llvm.lines) +
    maybe_string("M4", isc.m4.lines) +
    maybe_string("Madlang", isc.madlang.lines) +
    maybe_string("Makefile", isc.makefile.lines) +
    maybe_string("Rakefile", isc.rakefile.lines)
  )

// monoidal addition for 'file' type.
fun add_results(x: file, y: file): file =
  let
    var next = @{ lines = x.lines + y.lines
                , files = x.files + y.files 
                }
  in
    next
  end

overload + with add_results

// This is the step function used when streaming directory contents. 
fun adjust_contents(prev: source_contents, scf: pl_type) : source_contents =
  let
    val sc_r = ref<source_contents>(prev)
    val _ =
      case+ scf of
        | ~haskell n => sc_r->haskell := prev.haskell + @{ lines = n, files = 1 }
        | ~ats n => sc_r->ats := prev.ats + @{ lines = n, files = 1}
        | ~rust n => sc_r->rust := prev.rust + @{ lines = n, files = 1}
        | ~markdown n => sc_r->markdown := prev.markdown + @{ lines = n, files = 1}
        | ~python n => sc_r->python := prev.python + @{ lines = n, files = 1}
        | ~vimscript n => sc_r->vimscript := prev.vimscript + @{ lines = n, files = 1}
        | ~yaml n => sc_r->yaml := prev.yaml + @{ lines = n, files = 1}
        | ~toml n => sc_r->toml := prev.toml + @{ lines = n, files = 1}
        | ~happy n => sc_r->happy := prev.happy + @{ lines = n, files = 1}
        | ~alex n => sc_r->alex := prev.alex + @{ lines = n, files = 1}
        | ~idris n => sc_r->idris := prev.idris + @{ lines = n, files = 1}
        | ~madlang n => sc_r->madlang := prev.madlang + @{ lines = n, files = 1}
        | ~elm n => sc_r->elm := prev.elm + @{ lines = n, files = 1}
        | ~c n => sc_r->c := prev.c + @{ lines = n, files = 1}
        | ~go n => sc_r->go := prev.go + @{ lines = n, files = 1}
        | ~cabal n => sc_r->cabal := prev.cabal + @{ lines = n, files = 1}
        | ~verilog n => sc_r->verilog := prev.verilog + @{ lines = n, files = 1}
        | ~vhdl n => sc_r->vhdl := prev.vhdl + @{ lines = n, files = 1}
        | ~html n => sc_r->html := prev.html + @{ lines = n, files = 1}
        | ~css n => sc_r->css := prev.css + @{ lines = n, files = 1}
        | ~purescript n => sc_r->purescript := prev.purescript + @{ lines = n, files = 1}
        | ~futhark n => sc_r->futhark := prev.futhark + @{ lines = n, files = 1}
        | ~brainfuck n => sc_r->brainfuck := prev.brainfuck + @{ lines = n, files = 1}
        | ~ruby n => sc_r->ruby := prev.ruby + @{ lines = n, files = 1}
        | ~julia n => sc_r->julia := prev.julia + @{ lines = n, files = 1}
        | ~tex n => sc_r->tex := prev.tex + @{ lines = n, files = 1}
        | ~perl n => sc_r->perl := prev.perl + @{ lines = n, files = 1}
        | ~ocaml n => sc_r->ocaml := prev.ocaml + @{ lines = n, files = 1}
        | ~agda n => sc_r->agda := prev.agda + @{ lines = n, files = 1}
        | ~cobol n => sc_r->cobol := prev.cobol + @{ lines = n, files = 1}
        | ~tcl n => sc_r->tcl := prev.tcl + @{ lines = n, files = 1}
        | ~r n => sc_r->r := prev.r + @{ lines = n, files = 1}
        | ~lua n => sc_r->lua := prev.lua + @{ lines = n, files = 1}
        | ~cpp n => sc_r->cpp := prev.cpp + @{ lines = n, files = 1}
        | ~lalrpop n => sc_r->lalrpop := prev.lalrpop + @{ lines = n, files = 1}
        | ~header n => sc_r->header := prev.header + @{ lines = n, files = 1}
        | ~sixten n => sc_r->sixten := prev.sixten + @{ lines = n, files = 1}
        | ~dhall n => sc_r->dhall := prev.dhall + @{ lines = n, files = 1}
        | ~ipkg n => sc_r->ipkg := prev.ipkg + @{ lines = n, files = 1}
        | ~justfile n => sc_r->justfile := prev.justfile + @{ lines = n, files = 1}
        | ~makefile n => sc_r->makefile := prev.makefile + @{ lines = n, files = 1}
        | ~ion n => sc_r->ion := prev.ion + @{ lines = n, files = 1}
        | ~bash n => sc_r->bash := prev.bash + @{ lines = n, files = 1}
        | ~hamlet n => sc_r->hamlet := prev.hamlet + @{ lines = n, files = 1}
        | ~cassius n => sc_r->cassius := prev.cassius + @{ lines = n, files = 1}
        | ~lucius n => sc_r->lucius := prev.lucius + @{ lines = n, files = 1}
        | ~julius n => sc_r->julius := prev.julius + @{ lines = n, files = 1}
        | ~mercury n => sc_r->mercury := prev.mercury + @{ lines = n, files = 1}
        | ~yacc n => sc_r->yacc := prev.yacc + @{ lines = n, files = 1}
        | ~lex n => sc_r->lex := prev.lex + @{ lines = n, files = 1}
        | ~coq n => sc_r->coq := prev.coq + @{ lines = n, files = 1}
        | ~jupyter n => sc_r->jupyter := prev.jupyter + @{ lines = n, files = 1}
        | ~java n => sc_r->java := prev.java + @{ lines = n, files = 1}
        | ~scala n => sc_r->scala := prev.scala + @{ lines = n, files = 1}
        | ~erlang n => sc_r->erlang := prev.erlang + @{ lines = n, files = 1}
        | ~elixir n => sc_r->elixir := prev.elixir + @{ lines = n, files = 1}
        | ~pony n => sc_r->pony := prev.pony + @{ lines = n, files = 1}
        | ~clojure n => sc_r->clojure := prev.clojure + @{ lines = n, files = 1}
        | ~cabal_project n => sc_r->cabal_project := prev.cabal_project + @{ lines = n, files = 1}
        | ~assembly n => sc_r->assembly := prev.assembly + @{ lines = n, files = 1}
        | ~nix n => sc_r->nix := prev.nix + @{ lines = n, files = 1}
        | ~php n => sc_r->php := prev.php + @{ lines = n, files = 1}
        | ~javascript n => sc_r->javascript := prev.javascript + @{ lines = n, files = 1}
        | ~kotlin n => sc_r->kotlin := prev.kotlin + @{ lines = n, files = 1}
        | ~fsharp n => sc_r->fsharp := prev.fsharp + @{ lines = n, files = 1}
        | ~fortran n => sc_r->fortran := prev.fortran + @{ lines = n, files = 1}
        | ~swift n => sc_r->swift := prev.swift + @{ lines = n, files = 1}
        | ~csharp n => sc_r->csharp := prev.csharp + @{ lines = n, files = 1}
        | ~nim n => sc_r->nim := prev.nim + @{ lines = n, files = 1}
        | ~cpp_header n => sc_r->cpp_header := prev.cpp_header + @{ lines = n, files = 1}
        | ~elisp n => sc_r->elisp := prev.elisp + @{ lines = n, files = 1}
        | ~plaintext n => sc_r->plaintext := prev.plaintext + @{ lines = n, files = 1}
        | ~rakefile n => sc_r->rakefile := prev.rakefile + @{ lines = n, files = 1}
        | ~llvm n => sc_r->llvm := prev.llvm + @{ lines = n, files = 1}
        | ~autoconf n => sc_r->autoconf := prev.autoconf + @{ lines = n, files = 1}
        | ~batch n => sc_r->batch := prev.batch + @{ lines = n, files = 1}
        | ~powershell n => sc_r->powershell := prev.powershell + @{ lines = n, files = 1}
        | ~m4 n => sc_r->m4 := prev.m4 + @{ lines = n, files = 1}
        | ~unknown _ => ()
  in
    !sc_r
  end

fun free_pl(pl: pl_type) : void =
  case+ pl of
    | ~unknown _ => ()
    | ~rust _ => ()
    | ~haskell _ => ()
    | ~perl _ => ()
    | ~lucius _ => ()
    | ~cassius _ => ()
    | ~hamlet _ => ()
    | ~julius _ => ()
    | ~bash _ => ()
    | ~coq _ => ()
    | ~justfile _ => ()
    | ~makefile _ => ()
    | ~yaml _ => ()
    | ~toml _ => ()
    | ~dhall _ => ()
    | ~ipkg _ => ()
    | ~ion _ => ()
    | ~mercury _ => ()
    | ~yacc _ => ()
    | ~lex _ => ()
    | ~r _ => ()
    | ~c _ => ()
    | ~cpp _ => ()
    | ~lua _ => ()
    | ~lalrpop _ => ()
    | ~header _ => ()
    | ~sixten _ => ()
    | ~java _ => ()
    | ~scala _ => ()
    | ~elixir _ => ()
    | ~erlang _ => ()
    | ~happy _ => ()
    | ~alex _ => ()
    | ~go _ => ()
    | ~html _ => ()
    | ~css _ => ()
    | ~brainfuck _ => ()
    | ~ruby _ => ()
    | ~julia _ => ()
    | ~elm _ => ()
    | ~purescript _ => ()
    | ~vimscript _ => ()
    | ~ocaml _ => ()
    | ~madlang _ => ()
    | ~agda _ => ()
    | ~idris _ => ()
    | ~futhark _ => ()
    | ~ats _ => ()
    | ~tex _ => ()
    | ~cabal _ => ()
    | ~cobol _ => ()
    | ~tcl _ => ()
    | ~verilog _ => ()
    | ~vhdl _ => ()
    | ~markdown _ => ()
    | ~python _ => ()
    | ~pony _ => ()
    | ~jupyter _ => ()
    | ~clojure _ => ()
    | ~cabal_project _ => ()
    | ~assembly _ => ()
    | ~nix _ => ()
    | ~php _ => ()
    | ~javascript _ => ()
    | ~kotlin _ => ()
    | ~fsharp _ => ()
    | ~fortran _ => ()
    | ~swift _ => ()
    | ~csharp _ => ()
    | ~nim _ => ()
    | ~cpp_header _ => ()
    | ~elisp _ => ()
    | ~plaintext _ => ()
    | ~rakefile _ => ()
    | ~llvm _ => ()
    | ~autoconf _ => ()
    | ~batch _ => ()
    | ~powershell _ => ()
    | ~m4 _ => ()

// match a particular word against a list of keywords
fun match_keywords { m : nat | m <= 10 } (keys: list(string, m), word: string) : bool =
  list_foldright_cloref(keys, lam (next, acc) =<cloref1> acc || eq_string_string(next, word), false) // next = word, false)

// helper function for check_keywords
fun step_keyword(size: int, pre: pl_type, word: string, ext: string) : pl_type =
  case+ pre of
    | unknown _ =>
      let
      in
        case+ ext of
          | "y" =>
            let 
              val _ = free_pl(pre)
              var happy_keywords = list_cons("module", list_nil())
            in
              if match_keywords(happy_keywords, word)
                then happy size
                else if
                  let 
                    var yacc_keywords = list_cons("struct", list_cons("char", list_cons("int", list_nil())))
                  in
                    match_keywords(yacc_keywords, word)
                  end
                    then yacc size
                else unknown
            end
          | "v" =>
            let 
              var _ = free_pl(pre)
              var verilog_keywords = list_cons("endmodule", list_cons("posedge", list_cons("edge", list_cons("always", list_cons("wire", list_nil())))))
            in 
              if match_keywords(verilog_keywords, word) 
                then verilog size 
                else if 
                  let 
                    var coq_keywords = list_cons("Qed", list_cons("Require", list_cons("Hypothesis", list_cons("Inductive", list_cons("Remark", list_cons("Lemma", list_cons("Proof", list_cons("Definition", list_cons("Theorem", list_nil())))))))))
                  in
                  match_keywords(coq_keywords, word)
                  end
                    then coq size
                else unknown end
          | "m" =>
            let 
              val _ = free_pl(pre) 
            in 
              mercury size
            end
          | _ => pre
      end
    | _ => pre

// Function to disambiguate extensions such as .v (Coq and Verilog) and .m
// (Mercury and Objective C). This should only be called when extensions are in
// conflict, as it reads the whole file.
fun check_keywords(s: string, size: int, ext: string) : pl_type =
  let
    var ref = fileref_open_opt(s, file_mode_r)
  in
    case+ ref of
      | ~Some_vt(x) =>
        let
          var init: pl_type = unknown
          var viewstream = $EXTRA.streamize_fileref_word(x)
          val result = stream_vt_foldleft_cloptr( viewstream
                                                , init
                                                , lam (acc, next) => step_keyword(size, acc, next, ext)
                                                )
          val _ = fileref_close(x)
        in
          result
        end
      | ~None_vt() => (println!("[33mWarning:[0m could not open file at " + s) ; unknown)
  end

// Check shebang on scripts.
//
// TODO flexible parser that drops spaces as appropriate
// TODO check magic number so as to avoid checking shebang of binary file
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
        | ~None_vt() => (println!("[33mWarning:[0m could not open file at " + s) ; "")
  in
    case+ str of
      | "#!/usr/bin/env ion" => ion(line_count(s))
      | "#!/usr/bin/env bash" => bash(line_count(s))
      | "#!/bin/bash" => bash(line_count(s))
      | "#!python" => python(line_count(s))
      | "#!python2" => python(line_count(s))
      | "#!python3" => python(line_count(s))
      | "#!/usr/bin/env python" => python(line_count(s))
      | "#!/usr/bin/env python2" => python(line_count(s))
      | "#!/usr/bin/env python3" => python(line_count(s))
      | "#!/usr/bin/env perl" => perl(line_count(s))
      | "#!/usr/bin/env perl6" => perl(line_count(s))
      | "#!/usr/bin/perl" => perl(line_count(s))
      | "#!/usr/bin/env stack" => haskell(line_count(s))
      | "#!/usr/bin/env runhaskell" => haskell(line_count(s))
      | "#!/usr/bin/env node" => javascript(line_count(s))
      | _ => unknown
  end

// Match based on filename (for makefiles, etc.)
fun match_filename(s: string): pl_type =
  let
    val (prf | str) = filename_get_base(s)
    val match = $UN.strptr2string(str)
    prval () = prf(str)
  in
    case+ match of
      | "Makefile" => makefile(line_count(s))
      | "Makefile.tc" => makefile(line_count(s))
      | "makefile" => makefile(line_count(s))
      | "GNUmakefile" => makefile(line_count(s))
      | "Justfile" => justfile(line_count(s))
      | "justfile" => justfile(line_count(s))
      | "Rakefile" => rakefile(line_count(s))
      | _ => check_shebang(s)
  end

// Match based on file extension (assuming the file name is passed in as an
// argument).
fun prune_extension(s: string, file_proper: string): pl_type =
  let
    val (prf | str) = filename_get_ext(file_proper)
    val match: string = 
      if strptr2ptr(str) > 0
        then
          $UN.strptr2string(str)
        else
          ""
    prval () = prf (str)
  in
    case+ match of
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
      | "v" => check_keywords(s, line_count(s), match)
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
      | "yaml" => yaml(line_count(s))
      | "y" => check_keywords(s, line_count(s), match)
      | "ypp" => yacc(line_count(s))
      | "x" => alex(line_count(s))
      | "l" => lex(line_count(s))
      | "lpp" => lex(line_count(s))
      | "go" => go(line_count(s))
      | "html" => html(line_count(s))
      | "htm" => html(line_count(s))
      | "css" => css(line_count(s))
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
      | "R" => r(line_count(s))
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
      | "jl" => julia(line_count(s))
      | "ion" => ion(line_count(s))
      | "bash" => bash(line_count(s))
      | "ipynb" => jupyter(line_count(s))
      | "java" => java(line_count(s))
      | "scala" => scala(line_count(s))
      | "erl" => erlang(line_count(s))
      | "hrl" => erlang(line_count(s))
      | "ex" => elixir(line_count(s))
      | "exs" => elixir(line_count(s))
      | "pony" => pony(line_count(s))
      | "clj" => clojure(line_count(s))
      | "s" => assembly(line_count(s))
      | "S" => assembly(line_count(s))
      | "nix" => nix(line_count(s))
      | "php" => php(line_count(s))
      | "local" => cabal_project(line_count(s))
      | "project" => cabal_project(line_count(s))
      | "js" => javascript(line_count(s))
      | "jsexe" => javascript(line_count(s))
      | "kt" => kotlin(line_count(s))
      | "kts" => kotlin(line_count(s))
      | "fs" => fsharp(line_count(s))
      | "f" => fortran(line_count(s))
      | "for" => fortran(line_count(s))
      | "f90" => fortran(line_count(s))
      | "swift" => swift(line_count(s))
      | "csharp" => csharp(line_count(s))
      | "nim" => nim(line_count(s))
      | "el" => elisp(line_count(s))
      | "txt" => plaintext(line_count(s))
      | "ll" => llvm(line_count(s))
      | "in" => autoconf(line_count(s))
      | "bat" => batch(line_count(s))
      | "ps1" => powershell(line_count(s))
      | "ac" => m4(line_count(s))
      | "" => match_filename(s)
      | "sh" => match_filename(s)
      | "yamllint" => match_filename(s)
      | _ => unknown
  end

// filter out directories containing artifacts
fun bad_dir(s: string, excludes: List0(string)) : bool =
  case+ s of
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
    | _ => list_exists_cloref(excludes, lam x => x = s)

fnx step_stream(acc: source_contents, full_name: string, file_proper: string, excludes: List0(string)) : source_contents =
  if test_file_isdir(full_name) != 0 then
    flow_stream(full_name, acc, excludes)
  else
    adjust_contents(acc, prune_extension(full_name, file_proper))

and flow_stream(s: string, init: source_contents, excludes: List0(string)) : source_contents =
  let
    var files = streamize_dirname_fname(s)
    var ffiles = stream_vt_filter_cloptr(files, lam x => not(bad_dir(x, excludes)))
  in
    stream_vt_foldleft_cloptr( ffiles
                             , init
                             , lam (acc, next) => step_stream(acc, s + "/" + next, next, excludes)
                             )
  end

fun map_stream(acc: source_contents, includes: List0(string), excludes: List0(string)) : source_contents =
  if length(includes) > 0 then
    list_foldleft_cloref(includes, acc, lam (acc, next) => step_stream(acc, next, next, excludes)) // TODO check uniqueness
  else
    step_stream(acc, ".", ".", excludes)

fun is_flag(s: string) : bool =
  string_is_prefix("-", s)

fun process_excludes(s: string, acc: command_line) : command_line =
  let
    val acc_r = ref<command_line>(acc)
    val () = if is_flag(s)
      then (println!("Error: flag " + s + " found where a directory name was expected") ; exit(0) ; ())
      else acc_r->excludes := list_cons(s, acc.excludes)
  in
    !acc_r
  end

fun process(s: string, acc: command_line, is_first: bool) : command_line =
  let
    val acc_r = ref<command_line>(acc)
    val () = 
      if is_flag(s)
        then
          case+ s of
            | "--help" => acc_r->help := true
            | "-h" => acc_r->help := true
            | "--table" => if not(acc.table) then
              acc_r->table := true
              else (println!("[31mError:[0m flag " + s + " cannot appear twice") ; exit(0) ; ())
            | "-t" => if not(acc.table) then
              acc_r->table := true
              else (println!("[31mError:[0m flag " + s + " cannot appear twice") ; exit(0) ; ())
            | "--version" => acc_r->version := true
            | "-V" => acc_r->version := true
            | "-e" => (println!("[31mError:[0m flag " + s + " must be followed by an argument") ; exit(0) ; ())
            | "--exclude" => (println!("[31mError:[0m flag " + s + " must be followed by an argument") ; exit(0) ; ())
            | _ => (println!("[31mError:[0m flag " + s + " not recognized") ; exit(0) ; ())
        else
          if not(is_first) then
            acc_r->includes := list_cons(s, acc.includes)
          else
            ()
  in
    !acc_r
  end

fnx get_cli
  { n : int | n >= 1 }
  { m : nat | m < n }
  .<n-m>.
  ( argc: int n
  , argv: !argv(n)
  , current: int m
  , prev_is_exclude: bool
  , acc: command_line
  ) : command_line =
  let
    var arg = argv[current]
  in
    if current < argc - 1 then
      if arg != "--exclude" && arg != "-e" then
        let 
          val c = get_cli(argc, argv, current + 1, false, acc)
        in
          if prev_is_exclude && current != 0 then
            process_excludes(arg, c)
          else if current != 0 then
            process(arg, c, current = 0)
          else
            c
        end
      else
        let 
          val c = get_cli(argc, argv, current + 1, true, acc)
        in
          c
        end
    else
      if prev_is_exclude then
        process_excludes(arg, acc)
      else
        process(arg, acc, current = 0)
  end

fun version(): void =
  println!("polygot version 0.3.6\nCopyright (c) 2017 Vanessa McHale")

fun help(): void = 
print("polyglot - Count lines of code quickly.

[36mUSAGE:[0m poly [DIRECTORY] ... [OPTION] ...

[36mFLAGS:[0m
    -V, --version            show version information
    -h, --help               display this help and exit
    -e, --exclude            exclude a directory
    -t, --table              display results in a table

When no directory is provided poly will execute in the
current directory.

Bug reports and updates: nest.pijul.com/vamchale/polyglot\n")

fun default_head(ls: List0(string)) : string =
  case+ ls of
    | list_cons(x, _) => x
    | list_nil() => "."

fun empty_file(): file =
  let
    var f = @{ files = 0
             , lines = 0
             } : file
  in
    f
  end

// >> means "transform proof??"

implement main0 (argc, argv) =
  let
    val cli = @{ version = false
               , help = false
               , table = false
               , excludes = list_nil()
               , includes = list_nil()
               } : command_line
    val parsed = get_cli(argc, argv, 0, false, cli)
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
               , mercury = empty_file()
               , yacc = empty_file()
               , lex = empty_file()
               , coq = empty_file()
               , jupyter = empty_file()
               , java = empty_file()
               , scala = empty_file()
               , erlang = empty_file()
               , elixir = empty_file()
               , pony = empty_file()
               , clojure = empty_file()
               , cabal_project = empty_file()
               , assembly = empty_file()
               , nix = empty_file()
               , php = empty_file()
               , javascript = empty_file()
               , kotlin = empty_file()
               , fsharp = empty_file()
               , fortran = empty_file()
               , swift = empty_file()
               , csharp = empty_file()
               , nim = empty_file()
               , cpp_header = empty_file()
               , elisp = empty_file()
               , plaintext = empty_file()
               , rakefile = empty_file()
               , llvm = empty_file()
               , autoconf = empty_file()
               , batch = empty_file()
               , powershell = empty_file()
               , m4 = empty_file()
               } : source_contents
  in
    if parsed.help
      then
        ( help() ; exit(0) )
    else if parsed.version
      then
        ( version() ; exit(0) )
    else
      let 
        val result = map_stream(isc, parsed.includes, parsed.excludes)
      in
        if parsed.table
          then
            print(make_table(result))
        else
          print(make_output(result))
      end
  end
