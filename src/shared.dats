#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
#include "share/HATS/atslib_staload_libats_libc.hats"
#include "src/cli.dats"
#include "src/count-loop.dats"

staload "libats/ML/DATS/filebas_dirent.dats"
staload "prelude/SATS/stream_vt.sats"
staload "libats/ML/DATS/list0.dats"
staload "libats/ML/DATS/string.dats"
staload "libats/ML/SATS/list0.sats"
staload "libats/ML/SATS/basis.sats"
staload "prelude/SATS/filebas.sats"
staload "src/filetype.sats"
staload "libats/ML/DATS/filebas.dats"
staload EXTRA = "libats/ML/SATS/filebas.sats"
staload "libats/libc/SATS/unistd.sats"

fn eq_pl_type(x : !pl_type, y : !pl_type) : bool =
  case- (x, y) of
    | (happy (_), happy (_)) => true
    | (yacc (_), yacc (_)) => true
    | (coq (_), coq (_)) => true
    | (verilog (_), verilog (_)) => true

overload = with eq_pl_type

fun to_file(s : string, pre : Option(string)) : file =
  let
    var is_comment = case+ pre of
      | Some (x) => string_is_prefix(x, s)
      | None => false
  in
    if s = "" then
      @{ lines = 1, blanks = 1, comments = 0, files = 0 }
    else
      if is_comment then
        @{ lines = 1, blanks = 0, comments = 1, files = 0 }
      else
        @{ lines = 1, blanks = 0, comments = 0, files = 0 }
  end

fun empty_file() : file =
  let
    var f = @{ files = 0, blanks = 0, comments = 0, lines = 0 } : file
  in
    f
  end

fn bad_file(s : string) : void =
  if s != "" then
    prerr!("\33[33mWarning:\33[0m could not open file at " + s + "\n")
  else
    ()

fnx right_pad { k : int | k >= 0 }{ m : int | m <= k } .<k>. (s : string(m), n : int(k)) :
  string =
  case+ length(s) < n of
    | true when n > 0 => right_pad(s, n - 1) + " "
    | _ => s

// Pad a string on the left by adding spaces.
fnx left_pad { k : int | k >= 0 } .<k>. (s : string, n : int(k)) : string =
  case+ length(s) < n of
    | true when n > 0 => " " + left_pad(s, n - 1)
    | _ => s

// helper function for make_table
fun maybe_table { k : int | k >= 0 && k < 20 }(s : string(k), f : file) : string =
  let
    var code = f.lines - f.comments - f.blanks
    var pad = right_pad(s, 21)
  in
    if f.files > 0 then
      " "
      + right_pad(s, 21)
      + left_pad(tostring_int(f.files), 5)
      + left_pad(tostring_int(f.lines), 12)
      + left_pad(tostring_int(code), 13)
      + left_pad(tostring_int(f.comments), 13)
      + left_pad(tostring_int(f.blanks), 13)
      + "\n"
    else
      ""
  end

// helper function to make totals for tabular output.
fun sum_fields(sc : source_contents) : file =
  let
    var f = @{ lines = sc.rust.lines
                     + sc.haskell.lines
                     + sc.ats.lines
                     + sc.python.lines
                     + sc.vimscript.lines
                     + sc.elm.lines
                     + sc.idris.lines
                     + sc.madlang.lines
                     + sc.tex.lines
                     + sc.markdown.lines
                     + sc.yaml.lines
                     + sc.toml.lines
                     + sc.cabal.lines
                     + sc.happy.lines
                     + sc.alex.lines
                     + sc.go.lines
                     + sc.html.lines
                     + sc.css.lines
                     + sc.verilog.lines
                     + sc.vhdl.lines
                     + sc.c.lines
                     + sc.purescript.lines
                     + sc.futhark.lines
                     + sc.brainfuck.lines
                     + sc.ruby.lines
                     + sc.julia.lines
                     + sc.perl.lines
                     + sc.ocaml.lines
                     + sc.agda.lines
                     + sc.cobol.lines
                     + sc.tcl.lines
                     + sc.r.lines
                     + sc.lua.lines
                     + sc.cpp.lines
                     + sc.lalrpop.lines
                     + sc.header.lines
                     + sc.sixten.lines
                     + sc.dhall.lines
                     + sc.ipkg.lines
                     + sc.makefile.lines
                     + sc.justfile.lines
                     + sc.ion.lines
                     + sc.bash.lines
                     + sc.hamlet.lines
                     + sc.cassius.lines
                     + sc.lucius.lines
                     + sc.julius.lines
                     + sc.mercury.lines
                     + sc.yacc.lines
                     + sc.lex.lines
                     + sc.coq.lines
                     + sc.jupyter.lines
                     + sc.java.lines
                     + sc.scala.lines
                     + sc.erlang.lines
                     + sc.elixir.lines
                     + sc.pony.lines
                     + sc.clojure.lines
                     + sc.cabal_project.lines
                     + sc.assembly.lines
                     + sc.nix.lines
                     + sc.php.lines
                     + sc.javascript.lines
                     + sc.kotlin.lines
                     + sc.fsharp.lines
                     + sc.fortran.lines
                     + sc.swift.lines
                     + sc.csharp.lines
                     + sc.nim.lines
                     + sc.cpp_header.lines
                     + sc.elisp.lines
                     + sc.plaintext.lines
                     + sc.rakefile.lines
                     + sc.llvm.lines
                     + sc.autoconf.lines
                     + sc.batch.lines
                     + sc.powershell.lines
                     + sc.m4.lines
                     + sc.objective_c.lines
                     + sc.automake.lines
             , blanks = sc.rust.blanks
                      + sc.haskell.blanks
                      + sc.ats.blanks
                      + sc.python.blanks
                      + sc.vimscript.blanks
                      + sc.elm.blanks
                      + sc.idris.blanks
                      + sc.madlang.blanks
                      + sc.tex.blanks
                      + sc.markdown.blanks
                      + sc.yaml.blanks
                      + sc.toml.blanks
                      + sc.cabal.blanks
                      + sc.happy.blanks
                      + sc.alex.blanks
                      + sc.go.blanks
                      + sc.html.blanks
                      + sc.css.blanks
                      + sc.verilog.blanks
                      + sc.vhdl.blanks
                      + sc.c.blanks
                      + sc.purescript.blanks
                      + sc.futhark.blanks
                      + sc.brainfuck.blanks
                      + sc.ruby.blanks
                      + sc.julia.blanks
                      + sc.perl.blanks
                      + sc.ocaml.blanks
                      + sc.agda.blanks
                      + sc.cobol.blanks
                      + sc.tcl.blanks
                      + sc.r.blanks
                      + sc.lua.blanks
                      + sc.cpp.blanks
                      + sc.lalrpop.blanks
                      + sc.header.blanks
                      + sc.sixten.blanks
                      + sc.dhall.blanks
                      + sc.ipkg.blanks
                      + sc.makefile.blanks
                      + sc.justfile.blanks
                      + sc.ion.blanks
                      + sc.bash.blanks
                      + sc.hamlet.blanks
                      + sc.cassius.blanks
                      + sc.lucius.blanks
                      + sc.julius.blanks
                      + sc.mercury.blanks
                      + sc.yacc.blanks
                      + sc.lex.blanks
                      + sc.coq.blanks
                      + sc.jupyter.blanks
                      + sc.java.blanks
                      + sc.scala.blanks
                      + sc.erlang.blanks
                      + sc.elixir.blanks
                      + sc.pony.blanks
                      + sc.clojure.blanks
                      + sc.cabal_project.blanks
                      + sc.assembly.blanks
                      + sc.nix.blanks
                      + sc.php.blanks
                      + sc.javascript.blanks
                      + sc.kotlin.blanks
                      + sc.fsharp.blanks
                      + sc.fortran.blanks
                      + sc.swift.blanks
                      + sc.csharp.blanks
                      + sc.nim.blanks
                      + sc.cpp_header.blanks
                      + sc.elisp.blanks
                      + sc.plaintext.blanks
                      + sc.rakefile.blanks
                      + sc.llvm.blanks
                      + sc.autoconf.blanks
                      + sc.batch.blanks
                      + sc.powershell.blanks
                      + sc.m4.blanks
                      + sc.objective_c.blanks
                      + sc.automake.blanks
             , comments = sc.rust.comments
                        + sc.haskell.comments
                        + sc.ats.comments
                        + sc.python.comments
                        + sc.vimscript.comments
                        + sc.elm.comments
                        + sc.idris.comments
                        + sc.madlang.comments
                        + sc.tex.comments
                        + sc.markdown.comments
                        + sc.yaml.comments
                        + sc.toml.comments
                        + sc.cabal.comments
                        + sc.happy.comments
                        + sc.alex.comments
                        + sc.go.comments
                        + sc.html.comments
                        + sc.css.comments
                        + sc.verilog.comments
                        + sc.vhdl.comments
                        + sc.c.comments
                        + sc.purescript.comments
                        + sc.futhark.comments
                        + sc.brainfuck.comments
                        + sc.ruby.comments
                        + sc.julia.comments
                        + sc.perl.comments
                        + sc.ocaml.comments
                        + sc.agda.comments
                        + sc.cobol.comments
                        + sc.tcl.comments
                        + sc.r.comments
                        + sc.lua.comments
                        + sc.cpp.comments
                        + sc.lalrpop.comments
                        + sc.header.comments
                        + sc.sixten.comments
                        + sc.dhall.comments
                        + sc.ipkg.comments
                        + sc.makefile.comments
                        + sc.justfile.comments
                        + sc.ion.comments
                        + sc.bash.comments
                        + sc.hamlet.comments
                        + sc.cassius.comments
                        + sc.lucius.comments
                        + sc.julius.comments
                        + sc.mercury.comments
                        + sc.yacc.comments
                        + sc.lex.comments
                        + sc.coq.comments
                        + sc.jupyter.comments
                        + sc.java.comments
                        + sc.scala.comments
                        + sc.erlang.comments
                        + sc.elixir.comments
                        + sc.pony.comments
                        + sc.clojure.comments
                        + sc.cabal_project.comments
                        + sc.assembly.comments
                        + sc.nix.comments
                        + sc.php.comments
                        + sc.javascript.comments
                        + sc.kotlin.comments
                        + sc.fsharp.comments
                        + sc.fortran.comments
                        + sc.swift.comments
                        + sc.csharp.comments
                        + sc.nim.comments
                        + sc.cpp_header.comments
                        + sc.elisp.comments
                        + sc.plaintext.comments
                        + sc.rakefile.comments
                        + sc.llvm.comments
                        + sc.autoconf.comments
                        + sc.batch.comments
                        + sc.powershell.comments
                        + sc.m4.comments
                        + sc.objective_c.comments
                        + sc.automake.comments
             , files = sc.rust.files
                     + sc.haskell.files
                     + sc.ats.files
                     + sc.python.files
                     + sc.vimscript.files
                     + sc.elm.files
                     + sc.idris.files
                     + sc.madlang.files
                     + sc.tex.files
                     + sc.markdown.files
                     + sc.yaml.files
                     + sc.toml.files
                     + sc.cabal.files
                     + sc.happy.files
                     + sc.alex.files
                     + sc.go.files
                     + sc.html.files
                     + sc.css.files
                     + sc.verilog.files
                     + sc.vhdl.files
                     + sc.c.files
                     + sc.purescript.files
                     + sc.futhark.files
                     + sc.brainfuck.files
                     + sc.ruby.files
                     + sc.julia.files
                     + sc.perl.files
                     + sc.ocaml.files
                     + sc.agda.files
                     + sc.cobol.files
                     + sc.tcl.files
                     + sc.r.files
                     + sc.lua.files
                     + sc.cpp.files
                     + sc.lalrpop.files
                     + sc.header.files
                     + sc.sixten.files
                     + sc.dhall.files
                     + sc.ipkg.files
                     + sc.makefile.files
                     + sc.justfile.files
                     + sc.ion.files
                     + sc.bash.files
                     + sc.hamlet.files
                     + sc.cassius.files
                     + sc.lucius.files
                     + sc.julius.files
                     + sc.mercury.files
                     + sc.yacc.files
                     + sc.lex.files
                     + sc.coq.files
                     + sc.jupyter.files
                     + sc.java.files
                     + sc.scala.files
                     + sc.erlang.files
                     + sc.elixir.files
                     + sc.pony.files
                     + sc.clojure.files
                     + sc.cabal_project.files
                     + sc.assembly.files
                     + sc.nix.files
                     + sc.php.files
                     + sc.javascript.files
                     + sc.kotlin.files
                     + sc.fsharp.files
                     + sc.fortran.files
                     + sc.swift.files
                     + sc.csharp.files
                     + sc.nim.files
                     + sc.cpp_header.files
                     + sc.elisp.files
                     + sc.plaintext.files
                     + sc.rakefile.files
                     + sc.llvm.files
                     + sc.autoconf.files
                     + sc.batch.files
                     + sc.powershell.files
                     + sc.m4.files
                     + sc.objective_c.files
                     + sc.automake.files
             }
  in
    f
  end

fn maybe_full(a : string, b : string, c : string) : string =
  if b != "" then
    a + b + c
  else
    ""

// function to print tabular output at the end
fun make_table(isc : source_contents) : string =
  let
    var a = "-------------------------------------------------------------------------------\n \33[35mLanguage\33[0m             \33[35mFiles\33[0m       \33[35mLines\33[0m         \33[35mCode\33[0m     \33[35mComments\33[0m       \33[35mBlanks\33[0m\n-------------------------------------------------------------------------------\n"
    var b: string = maybe_table("Alex", isc.alex)
    + maybe_table("Agda", isc.agda)
    + maybe_table("Assembly", isc.assembly)
    + maybe_table("ATS", isc.ats)
    + maybe_table("Autoconf", isc.autoconf)
    + maybe_table("Automake", isc.automake)
    + maybe_table("Bash", isc.bash)
    + maybe_table("Batch", isc.batch)
    + maybe_table("Brainfuck", isc.brainfuck)
    + maybe_table("C", isc.c)
    + maybe_table("Carp", isc.carp)
    + maybe_table("C--", isc.cmm)
    + maybe_table("C++ Header", isc.cpp_header)
    + maybe_table("C++", isc.cpp)
    + maybe_table("C#", isc.csharp)
    + maybe_table("C Header", isc.header)
    + maybe_table("Cabal", isc.cabal)
    + maybe_table("Cabal Project", isc.cabal_project)
    + maybe_table("Cassius", isc.cassius)
    + maybe_table("COBOL", isc.cobol)
    + maybe_table("Coq", isc.coq)
    + maybe_table("CSS", isc.css)
    + maybe_table("Dhall", isc.dhall)
    + maybe_table("Elixir", isc.elixir)
    + maybe_table("Elm", isc.elm)
    + maybe_table("Emacs Lisp", isc.elisp)
    + maybe_table("Erlang", isc.erlang)
    + maybe_table("F#", isc.fsharp)
    + maybe_table("Fortran", isc.fortran)
    + maybe_table("Go", isc.go)
    + maybe_table("Greencard", isc.greencard)
    + maybe_table("Hamlet", isc.hamlet)
    + maybe_table("Happy", isc.happy)
    + maybe_table("Haskell", isc.haskell)
    + maybe_table("HTML", isc.html)
    + maybe_table("Idris", isc.idris)
    + maybe_table("iPKG", isc.ipkg)
    + maybe_table("Ion", isc.ion)
    + maybe_table("Java", isc.java)
    + maybe_table("JavaScript", isc.javascript)
    + maybe_table("Julius", isc.julius)
    + maybe_table("Julia", isc.julia)
    + maybe_table("Jupyter", isc.jupyter)
    + maybe_table("Justfile", isc.justfile)
    + maybe_table("Kotlin", isc.kotlin)
    + maybe_table("LALRPOP", isc.lalrpop)
    + maybe_table("Lex", isc.lex)
    + maybe_table("LLVM", isc.llvm)
    + maybe_table("Lua", isc.lua)
    + maybe_table("Lucius", isc.lucius)
    + maybe_table("M4", isc.m4)
    + maybe_table("Madlang", isc.madlang)
    + maybe_table("Makefile", isc.makefile)
    + maybe_table("Margaret", isc.margaret)
    + maybe_table("Markdown", isc.markdown)
    + maybe_table("Mercury", isc.mercury)
    + maybe_table("Nim", isc.nim)
    + maybe_table("Nix", isc.nix)
    + maybe_table("Objective C", isc.objective_c)
    + maybe_table("OCaml", isc.ocaml)
    + maybe_table("Perl", isc.perl)
    + maybe_table("PHP", isc.php)
    + maybe_table("Plaintext", isc.plaintext)
    + maybe_table("PowerShell", isc.powershell)
    + maybe_table("Pony", isc.pony)
    + maybe_table("Python", isc.python)
    + maybe_table("PureScript", isc.purescript)
    + maybe_table("R", isc.r)
    + maybe_table("Rakefile", isc.rakefile)
    + maybe_table("Ruby", isc.ruby)
    + maybe_table("Rust", isc.rust)
    + maybe_table("Scala", isc.scala)
    + maybe_table("Shen", isc.shen)
    + maybe_table("Sixten", isc.sixten)
    + maybe_table("Swift", isc.swift)
    + maybe_table("TCL", isc.tcl)
    + maybe_table("TeX", isc.tex)
    + maybe_table("TOML", isc.toml)
    + maybe_table("Verilog", isc.verilog)
    + maybe_table("VHDL", isc.vhdl)
    + maybe_table("Vimscript", isc.vimscript)
    + maybe_table("Yacc", isc.yacc)
    + maybe_table("YAML", isc.yaml)
    var c = "-------------------------------------------------------------------------------\n"
    + maybe_table("Total", sum_fields(isc))
    + "-------------------------------------------------------------------------------\n"
  in
    maybe_full(a, b, c)
  end

// Function to print output sorted by type of language.
fun make_output(isc : source_contents) : string =
  let
    var maybe_string = lam@ (s : string, n : int) : string =>
      if n > 0 then
        s + ": " + tostring_int(n) + "\n"
      else
        ""
    var with_nonempty = lam@ (s1 : string, s2 : string) : string =>
      if s2 != "" then
        s1 + s2
      else
        ""
  in
    with_nonempty( "\33[33mProgramming Languages:\33[0m\n"
                 , maybe_string("Agda", isc.agda.lines)
                 + maybe_string("Assembly", isc.assembly.lines)
                 + maybe_string("ATS", isc.ats.lines)
                 + maybe_string("Brainfuck", isc.brainfuck.lines)
                 + maybe_string("C", isc.c.lines)
                 + maybe_string("Carp", isc.carp.lines)
                 + maybe_string("C++", isc.cpp.lines)
                 + maybe_string("C--", isc.cmm.lines)
                 + maybe_string("C++ Header", isc.cpp_header.lines)
                 + maybe_string("C#", isc.csharp.lines)
                 + maybe_string("C Header", isc.header.lines)
                 + maybe_string("COBOL", isc.cobol.lines)
                 + maybe_string("Coq", isc.coq.lines)
                 + maybe_string("Elixir", isc.elixir.lines)
                 + maybe_string("Elm", isc.elm.lines)
                 + maybe_string("Erlang", isc.erlang.lines)
                 + maybe_string("F#", isc.fsharp.lines)
                 + maybe_string("Fortran", isc.fortran.lines)
                 + maybe_string("Go", isc.go.lines)
                 + maybe_string("Haskell", isc.haskell.lines)
                 + maybe_string("Idris", isc.idris.lines)
                 + maybe_string("Kotline", isc.kotlin.lines)
                 + maybe_string("Java", isc.java.lines)
                 + maybe_string("Julia", isc.julia.lines)
                 + maybe_string("Lua", isc.lua.lines)
                 + maybe_string("Margaret", isc.margaret.lines)
                 + maybe_string("Mercury", isc.mercury.lines)
                 + maybe_string("Nim", isc.nim.lines)
                 + maybe_string("Objective C", isc.objective_c.lines)
                 + maybe_string("OCaml", isc.ocaml.lines)
                 + maybe_string("Perl", isc.perl.lines)
                 + maybe_string("Pony", isc.pony.lines)
                 + maybe_string("PureScript", isc.purescript.lines)
                 + maybe_string("Python", isc.python.lines)
                 + maybe_string("R", isc.r.lines)
                 + maybe_string("Ruby", isc.ruby.lines)
                 + maybe_string("Rust", isc.rust.lines)
                 + maybe_string("Scala", isc.scala.lines)
                 + maybe_string("Shen", isc.shen.lines)
                 + maybe_string("Sixten", isc.sixten.lines)
                 + maybe_string("Swift", isc.swift.lines)
                 + maybe_string("TCL", isc.tcl.lines)
                 )
    + with_nonempty( "\n\33[33mEditor Plugins:\33[0m\n"
                   , maybe_string("Emacs Lisp", isc.elisp.lines)
                   + maybe_string("Vimscript", isc.vimscript.lines)
                   )
    + with_nonempty( "\n\33[33mDocumentation:\33[0m\n"
                   , maybe_string("Markdown", isc.markdown.lines)
                   + maybe_string("Plaintext", isc.plaintext.lines)
                   + maybe_string("TeX", isc.tex.lines)
                   )
    + with_nonempty( "\n\33[33mConfiguration:\33[0m\n"
                   , maybe_string("Cabal", isc.cabal.lines)
                   + maybe_string("Cabal Project", isc.cabal_project.lines)
                   + maybe_string("Dhall", isc.dhall.lines)
                   + maybe_string("iPKG", isc.ipkg.lines)
                   + maybe_string("TOML", isc.toml.lines)
                   + maybe_string("YAML", isc.yaml.lines)
                   )
    + with_nonempty( "\n\33[33mShell:\33[0m\n"
                   , maybe_string("Bash", isc.bash.lines)
                   + maybe_string("Batch", isc.batch.lines)
                   + maybe_string("Ion", isc.ion.lines)
                   + maybe_string("PowerShell", isc.powershell.lines)
                   )
    + with_nonempty( "\n\33[33mParser Generators:\33[0m\n"
                   , maybe_string("Alex", isc.alex.lines)
                   + maybe_string("Happy", isc.happy.lines)
                   + maybe_string("LALRPOP", isc.lalrpop.lines)
                   + maybe_string("Lex", isc.lex.lines)
                   + maybe_string("Yacc", isc.yacc.lines)
                   )
    + with_nonempty( "\n\33[33mWeb:\33[0m\n"
                   , maybe_string("Cassius", isc.cassius.lines)
                   + maybe_string("CSS", isc.css.lines)
                   + maybe_string("Hamlet", isc.hamlet.lines)
                   + maybe_string("HTML", isc.html.lines)
                   + maybe_string("JavaScript", isc.javascript.lines)
                   + maybe_string("Julius", isc.julius.lines)
                   + maybe_string("Lucius", isc.lucius.lines)
                   )
    + with_nonempty( "\n\33[33mHardware:\33[0m\n"
                   , maybe_string("Verilog", isc.verilog.lines)
                   + maybe_string("VHDL", isc.vhdl.lines)
                   )
    + with_nonempty("\n\33[33mNotebooks:\33[0m\n", maybe_string("Jupyter", isc.jupyter.lines))
    + with_nonempty( "\n\33[33mOther:\33[0m\n"
                   , maybe_string("Autoconf", isc.autoconf.lines)
                   + maybe_string("Automake", isc.automake.lines)
                   + maybe_string("Greencard", isc.greencard.lines)
                   + maybe_string("Justfile", isc.justfile.lines)
                   + maybe_string("LLVM", isc.llvm.lines)
                   + maybe_string("M4", isc.m4.lines)
                   + maybe_string("Madlang", isc.madlang.lines)
                   + maybe_string("Makefile", isc.makefile.lines)
                   + maybe_string("Rakefile", isc.rakefile.lines)
                   )
  end

fun add_contents(x : source_contents, y : source_contents) : source_contents =
  let
    var next = @{ rust = x.rust + y.rust
                , haskell = x.haskell + y.haskell
                , ats = x.ats + y.ats
                , python = x.python + y.python
                , vimscript = x.vimscript + y.vimscript
                , elm = x.elm + y.elm
                , idris = x.idris + y.idris
                , madlang = x.madlang + y.madlang
                , tex = x.tex + y.tex
                , markdown = x.markdown + y.markdown
                , yaml = x.yaml + y.yaml
                , toml = x.toml + y.toml
                , cabal = x.cabal + y.cabal
                , happy = x.happy + y.happy
                , alex = x.alex + y.alex
                , go = x.go + y.go
                , html = x.html + y.html
                , css = x.css + y.css
                , verilog = x.verilog + y.verilog
                , vhdl = x.vhdl + y.vhdl
                , c = x.c + y.c
                , purescript = x.purescript + y.purescript
                , futhark = x.futhark + y.futhark
                , brainfuck = x.brainfuck + y.brainfuck
                , ruby = x.ruby + y.ruby
                , julia = x.julia + y.julia
                , perl = x.perl + y.perl
                , ocaml = x.ocaml + y.ocaml
                , agda = x.agda + y.agda
                , cobol = x.cobol + y.cobol
                , tcl = x.tcl + y.tcl
                , r = x.r + y.r
                , lua = x.lua + y.lua
                , cpp = x.cpp + y.cpp
                , lalrpop = x.lalrpop + y.lalrpop
                , header = x.header + y.header
                , sixten = x.sixten + y.sixten
                , dhall = x.dhall + y.dhall
                , ipkg = x.ipkg + y.ipkg
                , makefile = x.makefile + y.makefile
                , justfile = x.justfile + y.justfile
                , ion = x.ion + y.ion
                , bash = x.bash + y.bash
                , hamlet = x.hamlet + y.hamlet
                , cassius = x.cassius + y.cassius
                , lucius = x.lucius + y.lucius
                , julius = x.julius + y.julius
                , mercury = x.mercury + y.mercury
                , yacc = x.yacc + y.yacc
                , lex = x.lex + y.lex
                , coq = x.coq + y.coq
                , jupyter = x.jupyter + y.jupyter
                , java = x.java + y.java
                , scala = x.scala + y.scala
                , erlang = x.erlang + y.erlang
                , elixir = x.elixir + y.elixir
                , pony = x.pony + y.pony
                , clojure = x.clojure + y.clojure
                , cabal_project = x.cabal_project + y.cabal_project
                , assembly = x.assembly + y.assembly
                , nix = x.nix + y.nix
                , php = x.php + y.php
                , javascript = x.javascript + y.javascript
                , kotlin = x.kotlin + y.kotlin
                , fsharp = x.fsharp + y.fsharp
                , fortran = x.fortran + y.fortran
                , swift = x.swift + y.swift
                , csharp = x.csharp + y.csharp
                , nim = x.nim + y.nim
                , cpp_header = x.cpp_header + y.cpp_header
                , elisp = x.elisp + y.elisp
                , plaintext = x.plaintext + y.plaintext
                , rakefile = x.rakefile + y.rakefile
                , llvm = x.llvm + y.llvm
                , autoconf = x.autoconf + y.autoconf
                , batch = x.batch + y.batch
                , powershell = x.powershell + y.powershell
                , m4 = x.m4 + y.m4
                , objective_c = x.objective_c + y.objective_c
                , automake = x.automake + y.automake
                , margaret = x.margaret + y.margaret
                , carp = x.carp + y.carp
                , shen = x.shen + y.shen
                , greencard = x.greencard + y.greencard
                , cmm = x.cmm + y.cmm
                } : source_contents
  in
    next
  end

// This is the step function used when streaming directory contents. 
fun adjust_contents(prev : source_contents, scf : pl_type) : source_contents =
  let
    var sc_r = ref<source_contents>(prev)
    val _ = case+ scf of
      | ~haskell n => sc_r -> haskell := prev.haskell + n
      | ~ats n => sc_r -> ats := prev.ats + n
      | ~rust n => sc_r -> rust := prev.rust + n
      | ~markdown n => sc_r -> markdown := prev.markdown + n
      | ~python n => sc_r -> python := prev.python + n
      | ~vimscript n => sc_r -> vimscript := prev.vimscript + n
      | ~yaml n => sc_r -> yaml := prev.yaml + n
      | ~toml n => sc_r -> toml := prev.toml + n
      | ~happy n => sc_r -> happy := prev.happy + n
      | ~alex n => sc_r -> alex := prev.alex + n
      | ~idris n => sc_r -> idris := prev.idris + n
      | ~madlang n => sc_r -> madlang := prev.madlang + n
      | ~elm n => sc_r -> elm := prev.elm + n
      | ~c n => sc_r -> c := prev.c + n
      | ~go n => sc_r -> go := prev.go + n
      | ~cabal n => sc_r -> cabal := prev.cabal + n
      | ~verilog n => sc_r -> verilog := prev.verilog + n
      | ~vhdl n => sc_r -> vhdl := prev.vhdl + n
      | ~html n => sc_r -> html := prev.html + n
      | ~css n => sc_r -> css := prev.css + n
      | ~purescript n => sc_r -> purescript := prev.purescript + n
      | ~futhark n => sc_r -> futhark := prev.futhark + n
      | ~brainfuck n => sc_r -> brainfuck := prev.brainfuck + n
      | ~ruby n => sc_r -> ruby := prev.ruby + n
      | ~julia n => sc_r -> julia := prev.julia + n
      | ~tex n => sc_r -> tex := prev.tex + n
      | ~perl n => sc_r -> perl := prev.perl + n
      | ~ocaml n => sc_r -> ocaml := prev.ocaml + n
      | ~agda n => sc_r -> agda := prev.agda + n
      | ~cobol n => sc_r -> cobol := prev.cobol + n
      | ~tcl n => sc_r -> tcl := prev.tcl + n
      | ~r n => sc_r -> r := prev.r + n
      | ~lua n => sc_r -> lua := prev.lua + n
      | ~cpp n => sc_r -> cpp := prev.cpp + n
      | ~lalrpop n => sc_r -> lalrpop := prev.lalrpop + n
      | ~header n => sc_r -> header := prev.header + n
      | ~sixten n => sc_r -> sixten := prev.sixten + n
      | ~dhall n => sc_r -> dhall := prev.dhall + n
      | ~ipkg n => sc_r -> ipkg := prev.ipkg + n
      | ~justfile n => sc_r -> justfile := prev.justfile + n
      | ~makefile n => sc_r -> makefile := prev.makefile + n
      | ~ion n => sc_r -> ion := prev.ion + n
      | ~bash n => sc_r -> bash := prev.bash + n
      | ~hamlet n => sc_r -> hamlet := prev.hamlet + n
      | ~cassius n => sc_r -> cassius := prev.cassius + n
      | ~lucius n => sc_r -> lucius := prev.lucius + n
      | ~julius n => sc_r -> julius := prev.julius + n
      | ~mercury n => sc_r -> mercury := prev.mercury + n
      | ~yacc n => sc_r -> yacc := prev.yacc + n
      | ~lex n => sc_r -> lex := prev.lex + n
      | ~coq n => sc_r -> coq := prev.coq + n
      | ~jupyter n => sc_r -> jupyter := prev.jupyter + n
      | ~java n => sc_r -> java := prev.java + n
      | ~scala n => sc_r -> scala := prev.scala + n
      | ~erlang n => sc_r -> erlang := prev.erlang + n
      | ~elixir n => sc_r -> elixir := prev.elixir + n
      | ~pony n => sc_r -> pony := prev.pony + n
      | ~clojure n => sc_r -> clojure := prev.clojure + n
      | ~cabal_project n => sc_r -> cabal_project := prev.cabal_project + n
      | ~assembly n => sc_r -> assembly := prev.assembly + n
      | ~nix n => sc_r -> nix := prev.nix + n
      | ~php n => sc_r -> php := prev.php + n
      | ~javascript n => sc_r -> javascript := prev.javascript + n
      | ~kotlin n => sc_r -> kotlin := prev.kotlin + n
      | ~fsharp n => sc_r -> fsharp := prev.fsharp + n
      | ~fortran n => sc_r -> fortran := prev.fortran + n
      | ~swift n => sc_r -> swift := prev.swift + n
      | ~csharp n => sc_r -> csharp := prev.csharp + n
      | ~nim n => sc_r -> nim := prev.nim + n
      | ~cpp_header n => sc_r -> cpp_header := prev.cpp_header + n
      | ~elisp n => sc_r -> elisp := prev.elisp + n
      | ~plaintext n => sc_r -> plaintext := prev.plaintext + n
      | ~rakefile n => sc_r -> rakefile := prev.rakefile + n
      | ~llvm n => sc_r -> llvm := prev.llvm + n
      | ~autoconf n => sc_r -> autoconf := prev.autoconf + n
      | ~batch n => sc_r -> batch := prev.batch + n
      | ~powershell n => sc_r -> powershell := prev.powershell + n
      | ~m4 n => sc_r -> m4 := prev.m4 + n
      | ~objective_c n => sc_r -> objective_c := prev.objective_c + n
      | ~automake n => sc_r -> automake := prev.automake + n
      | ~margaret n => sc_r -> margaret := prev.margaret + n
      | ~carp n => sc_r -> carp := prev.carp + n
      | ~shen n => sc_r -> shen := prev.shen + n
      | ~greencard n => sc_r -> greencard := prev.greencard + n
      | ~cmm n => sc_r -> cmm := prev.cmm + n
      | ~unknown _ => ()
  in
    !sc_r
  end

fun free_pl(pl : pl_type) : void =
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
    | ~objective_c _ => ()
    | ~automake _ => ()
    | ~margaret _ => ()
    | ~carp _ => ()
    | ~shen _ => ()
    | ~greencard _ => ()
    | ~cmm _ => ()

fun match_keywords { m : nat | m <= 10 }(keys : list(string, m), word : string) : bool =
  list_foldright_cloref(keys, lam (next, acc) =<cloref1> acc || eq_string_string( next
                                                                                , word
                                                                                ), false)

// TODO use list_vt{int}(0, 1, 2, 3, 4) instead?
// helper function for check_keywords
fun step_keyword(size : file, pre : pl_type, word : string, ext : string) : pl_type =
  case+ pre of
    | unknown _ => let
      
    in
      case+ ext of
        | "y" => let
          val _ = free_pl(pre)
          var happy_keywords = list_cons("module", list_nil())
        in
          if match_keywords(happy_keywords, word) then
            happy(size)
          else
            if let
              var yacc_keywords = list_cons("struct", list_cons("char", list_cons("int", list_nil())))
            in
              match_keywords(yacc_keywords, word)
            end then
              yacc(size)
            else
              unknown
        end
        | "v" => let
          var _ = free_pl(pre)
          var verilog_keywords = list_cons( "endmodule"
                                          , list_cons( "posedge"
                                                     , list_cons( "edge"
                                                                , list_cons("always", list_cons("wire", list_nil()))
                                                                )
                                                     )
                                          )
        in
          if match_keywords( verilog_keywords
                           , word
                           ) then
            verilog(size)
          else
            if let
              var coq_keywords = list_cons( "Qed"
                                          , list_cons( "Require"
                                                     , list_cons( "Hypothesis"
                                                                , list_cons( "Inductive"
                                                                           , list_cons( "Remark"
                                                                                      , list_cons( "Lemma"
                                                                                                 , list_cons( "Proof"
                                                                                                            , list_cons( "Definition"
                                                                                                                       , list_cons( "Theorem"
                                                                                                                                  , list_nil()
                                                                                                                                  )
                                                                                                                       )
                                                                                                            )
                                                                                                 )
                                                                                      )
                                                                           )
                                                                )
                                                     )
                                          )
            in
              match_keywords(coq_keywords, word)
            end then
              coq(size)
            else
              unknown
        end
        | "m" => let
          val _ = free_pl(pre)
          var mercury_keywords = list_cons("module", list_cons("pred", list_cons("mode", list_nil())))
        in
          if match_keywords(mercury_keywords, word) then
            mercury(size)
          else
            if let
              var objective_c_keywords = list_cons( "nil"
                                                  , list_cons("nullable", list_cons("nonnull", list_nil()))
                                                  )
            in
              match_keywords(objective_c_keywords, word)
            end then
              objective_c(size)
            else
              unknown
        end
        | _ => pre
    end
    | _ => pre

// Function to disambiguate extensions such as .v (Coq and Verilog) and .m
// (Mercury and Objective C). This should only be called when extensions are in
// conflict, as it reads the whole file.
fun check_keywords(s : string, size : file, ext : string) : pl_type =
  let
    var ref = fileref_open_opt(s, file_mode_r)
  in
    case+ ref of
      | ~Some_vt (x) => let
        var init: pl_type = unknown
        var viewstream = $EXTRA.streamize_fileref_word(x)
        var result = stream_vt_foldleft_cloptr(viewstream, init, lam (acc, next) => step_keyword( size
                                                                                                , acc
                                                                                                , next
                                                                                                , ext
                                                                                                ))
        val _ = fileref_close(x)
      in
        result
      end
      | ~None_vt() => (bad_file(s) ; unknown)
  end

// Check shebang on scripts.
//
// TODO flexible parser that drops spaces as appropriate
// TODO check magic number so as to avoid checking shebang of binary file
fun check_shebang(s : string) : pl_type =
  let
    var ref = fileref_open_opt(s, file_mode_r)
    val str: string = case+ ref of
      | ~Some_vt (x) => let
        var s = strptr2string(fileref_get_line_string(x))
        val _ = fileref_close(x)
      in
        s
      end
      | ~None_vt() => (bad_file(s) ; "")
  in
    case+ str of
      | "#!/usr/bin/env ion" => ion(line_count(s, Some_vt("#")))
      | "#!/usr/bin/env bash" => bash(line_count(s, Some_vt("#")))
      | "#!/bin/bash" => bash(line_count(s, Some_vt("#")))
      | "#!python" => python(line_count(s, Some_vt("#")))
      | "#!python2" => python(line_count(s, Some_vt("#")))
      | "#!python3" => python(line_count(s, Some_vt("#")))
      | "#!/usr/bin/env python" => python(line_count(s, Some_vt("#")))
      | "#!/usr/bin/env python2" => python(line_count(s, Some_vt("#")))
      | "#!/usr/bin/env python3" => python(line_count(s, Some_vt("#")))
      | "#!/usr/bin/env perl" => perl(line_count(s, Some_vt("#")))
      | "#!/usr/bin/env perl6" => perl(line_count(s, Some_vt("#")))
      | "#!/usr/bin/perl" => perl(line_count(s, Some_vt("#")))
      | "#!/usr/bin/env stack" => haskell(line_count(s, Some_vt("--")))
      | "#!/usr/bin/env runhaskell" => haskell(line_count(s, Some_vt("--")))
      | "#!/usr/bin/env node" => javascript(line_count(s, None_vt))
      | _ => unknown
  end

// Match based on filename (for makefiles, etc.)
fun match_filename(s : string) : pl_type =
  let
    val (prf | str) = filename_get_base(s)
    var match = $UN.strptr2string(str)
    prval () = prf(str)
  in
    case+ match of
      | "Makefile" => makefile(line_count(s, Some_vt("#")))
      | "Makefile.tc" => makefile(line_count(s, Some_vt("#")))
      | "makefile" => makefile(line_count(s, Some_vt("#")))
      | "GNUmakefile" => makefile(line_count(s, Some_vt("#")))
      | "Justfile" => justfile(line_count(s, Some_vt("#")))
      | "justfile" => justfile(line_count(s, Some_vt("#")))
      | "Rakefile" => rakefile(line_count(s, None_vt))
      | "cabal.project.local" => cabal_project(line_count(s, Some_vt("--")))
      | _ => check_shebang(s)
  end

// Match based on file extension (assuming the file name is passed in as an
// argument).
fun prune_extension(s : string, file_proper : string) : pl_type =
  let
    val (prf | str) = filename_get_ext(s)
    val match: string = if strptr2ptr(str) > 0 then
      $UN.strptr2string(str)
    else
      ""
    prval () = prf(str)
  in
    case+ match of
      | "hs" => haskell(line_count(s, Some_vt("--")))
      | "hs-boot" => haskell(line_count(s, Some_vt("--")))
      | "hsig" => haskell(line_count(s, Some_vt("--")))
      | "gc" => greencard(line_count(s, Some_vt("--")))
      | "rs" => rust(line_count(s, Some_vt("//")))
      | "tex" => tex(line_count(s, Some_vt("%")))
      | "md" => markdown(line_count(s, None_vt))
      | "markdown" => markdown(line_count(s, None_vt))
      | "dats" => ats(line_count(s, Some_vt("//")))
      | "lats" => ats(line_count(s, Some_vt("//")))
      | "hats" => ats(line_count(s, Some_vt("//")))
      | "cats" => ats(line_count(s, Some_vt("//")))
      | "sats" => ats(line_count(s, Some_vt("//")))
      | "py" => python(line_count(s, None_vt))
      | "fut" => futhark(line_count(s, Some_vt("--")))
      | "pl" => perl(line_count(s, None_vt))
      | "agda" => agda(line_count(s, Some_vt("--")))
      | "idr" => idris(line_count(s, Some_vt("--")))
      | "v" => check_keywords(s, line_count(s, Some_vt("--")), match)
      | "m" => check_keywords(s, line_count(s, None_vt), match)
      | "vhdl" => vhdl(line_count(s, None_vt))
      | "vhd" => vhdl(line_count(s, None_vt))
      | "go" => go(line_count(s, Some_vt("//")))
      | "vim" => vimscript(line_count(s, None_vt))
      | "ml" => ocaml(line_count(s, None_vt))
      | "purs" => purescript(line_count(s, None_vt))
      | "elm" => elm(line_count(s, Some_vt("--")))
      | "mad" => madlang(line_count(s, Some_vt("#")))
      | "toml" => toml(line_count(s, Some_vt("#")))
      | "cabal" => cabal(line_count(s, Some_vt("--")))
      | "yml" => yaml(line_count(s, Some_vt("#")))
      | "yaml" => yaml(line_count(s, Some_vt("#")))
      | "y" => check_keywords(s, line_count(s, Some_vt("--")), match)
      | "ly" => happy(line_count(s, Some_vt("--")))
      | "yl" => happy(line_count(s, Some_vt("--")))
      | "ypp" => yacc(line_count(s, Some_vt("//")))
      | "x" => alex(line_count(s, Some_vt("--")))
      | "l" => lex(line_count(s, None_vt))
      | "lpp" => lex(line_count(s, None_vt))
      | "html" => html(line_count(s, None_vt))
      | "htm" => html(line_count(s, None_vt))
      | "css" => css(line_count(s, None_vt))
      | "vhdl" => vhdl(line_count(s, None_vt))
      | "vhd" => vhdl(line_count(s, None_vt))
      | "c" => c(line_count(s, Some_vt("//")))
      | "cmm" => cmm(line_count(s, Some_vt("//")))
      | "b" => brainfuck(line_count(s, None_vt))
      | "bf" => brainfuck(line_count(s, None_vt))
      | "rb" => ruby(line_count(s, None_vt))
      | "cob" => cobol(line_count(s, None_vt))
      | "cbl" => cobol(line_count(s, None_vt))
      | "cpy" => cobol(line_count(s, None_vt))
      | "ml" => ocaml(line_count(s, None_vt))
      | "tcl" => tcl(line_count(s, None_vt))
      | "r" => r(line_count(s, None_vt))
      | "R" => r(line_count(s, None_vt))
      | "lua" => lua(line_count(s, None_vt))
      | "cpp" => cpp(line_count(s, Some_vt("//")))
      | "cc" => cpp(line_count(s, Some_vt("//")))
      | "lalrpop" => lalrpop(line_count(s, Some_vt("//")))
      | "h" => header(line_count(s, None_vt))
      | "vix" => sixten(line_count(s, Some_vt("--")))
      | "dhall" => dhall(line_count(s, None_vt))
      | "ipkg" => ipkg(line_count(s, Some_vt("--")))
      | "mk" => makefile(line_count(s, Some_vt("#")))
      | "hamlet" => hamlet(line_count(s, None_vt))
      | "cassius" => cassius(line_count(s, None_vt))
      | "lucius" => cassius(line_count(s, None_vt))
      | "julius" => julius(line_count(s, None_vt))
      | "jl" => julia(line_count(s, None_vt))
      | "ion" => ion(line_count(s, Some_vt("#")))
      | "bash" => bash(line_count(s, Some_vt("#")))
      | "ipynb" => jupyter(line_count(s, None_vt))
      | "java" => java(line_count(s, None_vt))
      | "scala" => scala(line_count(s, None_vt))
      | "erl" => erlang(line_count(s, None_vt))
      | "hrl" => erlang(line_count(s, None_vt))
      | "ex" => elixir(line_count(s, None_vt))
      | "exs" => elixir(line_count(s, None_vt))
      | "pony" => pony(line_count(s, None_vt))
      | "clj" => clojure(line_count(s, None_vt))
      | "s" => assembly(line_count(s, Some_vt(";")))
      | "S" => assembly(line_count(s, Some_vt(";")))
      | "asm" => assembly(line_count(s, Some_vt(";")))
      | "nix" => nix(line_count(s, None_vt))
      | "php" => php(line_count(s, None_vt))
      | "local" => match_filename(s)
      | "project" => cabal_project(line_count(s, Some_vt("--")))
      | "js" => javascript(line_count(s, None_vt))
      | "jsexe" => javascript(line_count(s, None_vt))
      | "kt" => kotlin(line_count(s, None_vt))
      | "kts" => kotlin(line_count(s, None_vt))
      | "fs" => fsharp(line_count(s, None_vt))
      | "f" => fortran(line_count(s, None_vt))
      | "for" => fortran(line_count(s, None_vt))
      | "f90" => fortran(line_count(s, None_vt))
      | "f95" => fortran(line_count(s, None_vt))
      | "swift" => swift(line_count(s, None_vt))
      | "csharp" => csharp(line_count(s, None_vt))
      | "nim" => nim(line_count(s, None_vt))
      | "el" => elisp(line_count(s, None_vt))
      | "txt" => plaintext(line_count(s, None_vt))
      | "ll" => llvm(line_count(s, None_vt))
      | "in" => autoconf(line_count(s, Some_vt("#")))
      | "bat" => batch(line_count(s, None_vt))
      | "ps1" => powershell(line_count(s, None_vt))
      | "ac" => m4(line_count(s, None_vt))
      | "mm" => objective_c(line_count(s, Some_vt("//")))
      | "am" => automake(line_count(s, Some_vt("#")))
      | "mgt" => margaret(line_count(s, Some_vt("--")))
      | "carp" => carp(line_count(s, Some_vt(";")))
      | "" => match_filename(s)
      | "sh" => match_filename(s)
      | "yamllint" => match_filename(s)
      | _ => unknown
  end

// filter out directories containing artifacts
fun bad_dir(s : string, excludes : List0(string)) : bool =
  case+ s of
    | "." => true
    | ".." => true
    | ".pijul" => true
    | "_darcs" => true
    | ".hg" => true
    | ".git" => true
    | "target" => true
    | ".atspkg" => true
    | ".egg-info" => true
    | "nimcache" => true
    | ".shake" => true
    | "dist-newstyle" => true
    | "vendor" => true
    | "dist" => true
    | "gen" => true
    | ".psc-package" => true
    | ".pulp-cache" => true
    | "output" => true
    | "bower_components" => true
    | "elm-stuff" => true
    | ".stack-work" => true
    | ".reco" => true
    | ".reco-work" => true
    | ".cabal-sandbox" => true
    | "node_modules" => true
    | ".lein-plugins" => true
    | ".sass-cache" => true
    | _ => list_exists_cloref(excludes, lam x => x = s || x = s + "/")

fnx step_stream( acc : source_contents
               , full_name : string
               , file_proper : string
               , excludes : List0(string)
               ) : source_contents =
  if test_file_isdir(full_name) > 0 then
    flow_stream(full_name, acc, excludes)
  else
    adjust_contents(acc, prune_extension(full_name, file_proper))
and flow_stream(s : string, init : source_contents, excludes : List0(string)) :
  source_contents =
  let
    var files = $EXTRA.streamize_dirname_fname(s)
    var ffiles = stream_vt_filter_cloptr(files, lam x => not(bad_dir(x, excludes)))
  in
    if s != "." then
      stream_vt_foldleft_cloptr(ffiles, init, lam (acc, next) => step_stream( acc
                                                                            , s + "/" + next
                                                                            , next
                                                                            , excludes
                                                                            ))
    else
      stream_vt_foldleft_cloptr(ffiles, init, lam (acc, next) => step_stream( acc
                                                                            , next
                                                                            , next
                                                                            , excludes
                                                                            ))
  end

fun empty_contents() : source_contents =
  let
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
               , objective_c = empty_file()
               , automake = empty_file()
               , margaret = empty_file()
               , carp = empty_file()
               , shen = empty_file()
               , greencard = empty_file()
               , cmm = empty_file()
               } : source_contents
  in
    isc
  end

fn maybe_err(next : string) : void =
  (prerr("\33[31mError:\33[0m directory '" + next + "' does not exist\n") ; exit(1))

fun map_stream(acc : source_contents, includes : List0(string), excludes : List0(string)) :
  source_contents =
  list_foldleft_cloref(includes, acc, lam (acc, next) => if test_file_exists(next)
                      || test_file_isdir(next) < 0 || next = "" then
                        step_stream(acc, next, next, excludes)
                      else
                        (maybe_err(next) ; acc))

fun step_list(s : string, excludes : List0(string)) : List0(string) =
  let
    var files = $EXTRA.streamize_dirname_fname(s)
    var ffiles = stream_vt_filter_cloptr(files, lam x => not(bad_dir(x, excludes)
                                        && test_file_isdir(s + "/" + x) > 0))
    
    fun stream2list(x : stream_vt(string)) : List0(string) =
      case+ !x of
        | ~stream_vt_cons (x, xs) => list_cons(s + "/" + x, stream2list(xs))
        | ~stream_vt_nil() => list_nil
  in
    stream2list(ffiles)
  end

fun step_list_files(s : string, excludes : List0(string)) : List0(string) =
  let
    var files = $EXTRA.streamize_dirname_fname(s)
    var ffiles = stream_vt_filter_cloptr(files, lam x => not(bad_dir(x, excludes))
                                        && test_file_isdir(s + "/" + x) = 0)
    
    fun stream2list(x : stream_vt(string)) : List0(string) =
      case+ !x of
        | ~stream_vt_cons (x, xs) when s = "." => list_cons(x, stream2list(xs))
        | ~stream_vt_cons (x, xs) => list_cons(s + "/" + x, stream2list(xs))
        | ~stream_vt_nil() => list_nil
  in
    stream2list(ffiles)
  end

fun map_depth(xs : List0(string), excludes : List0(string)) : List0(string) =
  let
    fun loop(i : int, xs : List0(string), excludes : List0(string)) : List0(string) =
      let
        var xs0 = list0_filter(g0ofg1(xs), lam x => test_file_isdir(x) > 0)
      in
        case+ i of
          | 0 => g1ofg0(list0_mapjoin(xs0, lam x => if not(bad_dir(x, excludes)) then
                                       g0ofg1(step_list(x, excludes))
                                     else
                                       list0_nil))
          | _ => g1ofg0(list0_mapjoin(xs0, lam x => let
                                       var ys = step_list(x, excludes)
                                       var zs = step_list_files(x, excludes)
                                     in
                                       if not(bad_dir(x, excludes)) then
                                         g0ofg1(loop(i - 1, ys, excludes)) + g0ofg1(zs)
                                       else
                                         if x = "." && i = 3 then
                                           g0ofg1(loop(i - 1, ys, excludes)) + g0ofg1(zs)
                                         else
                                           list0_nil
                                     end))
      end
  in
    loop(3, xs, excludes)
  end