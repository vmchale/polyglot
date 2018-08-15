// This file contains various functions for printing generated output
staload "src/filetype.sats"
staload "libats/ML/SATS/string.sats"
staload UN = "prelude/SATS/unsafe.sats"

// Optimized right pad function. In the future, it could return string(k) but
// that would require a different string append function.
fun right_pad { k : int | k >= 0 }{ m : int | m <= k && m >= 0 } .<k>. (s : string(m), n : int(k)) : string =
  if length(s) < n then
    right_pad(s, n - 1) + " "
  else
    s

// Pad a string on the left by adding spaces.
fun left_pad { k : int | k >= 0 } .<k>. (s : string, n : int(k)) : string =
  case+ length(s) < n of
    | true when n > 0 => " " + left_pad(s, n - 1)
    | _ => s

fn maybe_full(a : string, b : string, c : string) : string =
  if b != "" then
    a + b + c
  else
    ""

// helper function for make_table
fn maybe_table { k : int | k >= 0 && k < 19 }(s : string(k), f : file) : string =
  let
    var code = f.lines - f.comments - f.blanks
  in
    if f.files > 0 then
      " "
      + right_pad(s, 18)
      + left_pad(tostring_int(f.files), 8)
      + left_pad(tostring_int(f.lines), 12)
      + left_pad(tostring_int(code), 13)
      + left_pad(tostring_int(f.comments), 13)
      + left_pad(tostring_int(f.blanks), 13)
      + "\n"
    else
      ""
  end

// helper function to make totals for tabular output.
fn sum_fields(sc : source_contents) : file =
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
                     + sc.dash.lines
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
                     + sc.margaret.lines
                     + sc.carp.lines
                     + sc.shen.lines
                     + sc.greencard.lines
                     + sc.cmm.lines
                     + sc.fluid.lines
                     + sc.plutus.lines
                     + sc.j.lines
                     + sc.blodwen.lines
                     + sc.crystal.lines
                     + sc.racket.lines
                     + sc.ada.lines
                     + sc.sml.lines
                     + sc.isabelle.lines
                     + sc.fstar.lines
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
                      + sc.dash.blanks
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
                      + sc.margaret.blanks
                      + sc.carp.blanks
                      + sc.shen.blanks
                      + sc.greencard.blanks
                      + sc.cmm.blanks
                      + sc.fluid.blanks
                      + sc.plutus.blanks
                      + sc.j.blanks
                      + sc.blodwen.blanks
                      + sc.crystal.blanks
                      + sc.racket.blanks
                      + sc.ada.blanks
                      + sc.sml.blanks
                      + sc.isabelle.blanks
                      + sc.fstar.blanks
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
                        + sc.dash.comments
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
                        + sc.margaret.comments
                        + sc.carp.comments
                        + sc.shen.comments
                        + sc.greencard.comments
                        + sc.cmm.comments
                        + sc.fluid.comments
                        + sc.plutus.comments
                        + sc.j.comments
                        + sc.blodwen.comments
                        + sc.crystal.comments
                        + sc.racket.comments
                        + sc.ada.comments
                        + sc.sml.comments
                        + sc.isabelle.comments
                        + sc.fstar.comments
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
                     + sc.dash.files
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
                     + sc.margaret.files
                     + sc.carp.files
                     + sc.shen.files
                     + sc.greencard.files
                     + sc.cmm.files
                     + sc.fluid.files
                     + sc.plutus.files
                     + sc.j.files
                     + sc.blodwen.files
                     + sc.crystal.files
                     + sc.racket.files
                     + sc.ada.files
                     + sc.sml.files
                     + sc.isabelle.files
                     + sc.fstar.files
             }
  in
    f
  end

// function to print tabular output at the end
fn make_table(isc : source_contents, colorize : bool) : string =
  let
    var a = if colorize then
      "-------------------------------------------------------------------------------\n \33[35mLanguage\33[0m             \33[35mFiles\33[0m       \33[35mLines\33[0m         \33[35mCode\33[0m     \33[35mComments\33[0m       \33[35mBlanks\33[0m\n-------------------------------------------------------------------------------\n"
    else
      "-------------------------------------------------------------------------------\n Language             Files       Lines         Code     Comments       Blanks\n-------------------------------------------------------------------------------\n"
    var b: string = maybe_table("Ada", isc.ada)
    + maybe_table("Alex", isc.alex)
    + maybe_table("Agda", isc.agda)
    + maybe_table("Assembly", isc.assembly)
    + maybe_table("ATS", isc.ats)
    + maybe_table("Autoconf", isc.autoconf)
    + maybe_table("Automake", isc.automake)
    + maybe_table("Bash", isc.bash)
    + maybe_table("Batch", isc.batch)
    + maybe_table("blodwen", isc.blodwen)
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
    + maybe_table("Crystal", isc.crystal)
    + maybe_table("CSS", isc.css)
    + maybe_table("Dash", isc.dash)
    + maybe_table("Dhall", isc.dhall)
    + maybe_table("Elixir", isc.elixir)
    + maybe_table("Elm", isc.elm)
    + maybe_table("Emacs Lisp", isc.elisp)
    + maybe_table("Erlang", isc.erlang)
    + maybe_table("F#", isc.fsharp)
    + maybe_table("F*", isc.fstar)
    + maybe_table("FLTK Data", isc.fluid)
    + maybe_table("Fortran", isc.fortran)
    + maybe_table("Futhark", isc.futhark)
    + maybe_table("Go", isc.go)
    + maybe_table("Greencard", isc.greencard)
    + maybe_table("Hamlet", isc.hamlet)
    + maybe_table("Happy", isc.happy)
    + maybe_table("Haskell", isc.haskell)
    + maybe_table("HTML", isc.html)
    + maybe_table("Idris", isc.idris)
    + maybe_table("iPKG", isc.ipkg)
    + maybe_table("Ion", isc.ion)
    + maybe_table("Isabelle", isc.isabelle)
    + maybe_table("J", isc.j)
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
    + maybe_table("Plutus", isc.plutus)
    + maybe_table("PowerShell", isc.powershell)
    + maybe_table("Pony", isc.pony)
    + maybe_table("Python", isc.python)
    + maybe_table("PureScript", isc.purescript)
    + maybe_table("R", isc.r)
    + maybe_table("Racket", isc.racket)
    + maybe_table("Rakefile", isc.rakefile)
    + maybe_table("Ruby", isc.ruby)
    + maybe_table("Rust", isc.rust)
    + maybe_table("Scala", isc.scala)
    + maybe_table("Shen", isc.shen)
    + maybe_table("Sixten", isc.sixten)
    + maybe_table("Standard ML", isc.sml)
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
fn make_output(isc : source_contents, color : bool) : string =
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
    var pl_string = if color then
      "\33[33mProgramming Languages:\33[0m\n"
    else
      "Programming Languages:\n"
    var editor_string = if color then
      "\n\33[33mEditor Plugins:\33[0m\n"
    else
      "\nEditor Plugins:\n"
    var doc_string = if color then
      "\n\33[33mDocumentation:\33[0m\n"
    else
      "\nDocumentation:\n"
    var cfg_string = if color then
      "\n\33[33mConfiguration:\33[0m\n"
    else
      "\nConfiguration:\n"
    var sh_string = if color then
      "\n\33[33mShell:\33[0m\n"
    else
      "\nShell:\n"
    var parser_string = if color then
      "\n\33[33mParser Generators:\33[0m\n"
    else
      "\nParser Generators:\n"
    var web_string = if color then
      "\n\33[33mWeb:\33[0m\n"
    else
      "\nWeb\n:"
    var hw_string = if color then
      "\n\33[33mHardware:\33[0m\n"
    else
      "\nHardware:\n"
    var gui_string = if color then
      "\n\33[33mGUIs:\33[0m\n"
    else
      "\nGUIs:\n"
    var nb_string = if color then
      "\n\33[33mNotebooks:\33[0m\n"
    else
      "\nNotebooks:\n"
    var contract_string = if color then
      "\n\33[33mContract Languages:\33[0m\n"
    else
      "\nContract Languages:\n"
    var other = if color then
      "\n\33[33mOther:\33[0m\n"
    else
      "\nOther:\n"
    var thm_string = if color then
      "\n\33[33mTheorem Provers:\33[0m\n"
    else
      "\nTheorem Provers\n"
  in
    with_nonempty( pl_string
                 , maybe_string("Ada", isc.ada.lines)
                 + maybe_string("Agda", isc.agda.lines)
                 + maybe_string("Assembly", isc.assembly.lines)
                 + maybe_string("ATS", isc.ats.lines)
                 + maybe_string("blodwen", isc.blodwen.lines)
                 + maybe_string("Brainfuck", isc.brainfuck.lines)
                 + maybe_string("C", isc.c.lines)
                 + maybe_string("Carp", isc.carp.lines)
                 + maybe_string("C++", isc.cpp.lines)
                 + maybe_string("C--", isc.cmm.lines)
                 + maybe_string("C++ Header", isc.cpp_header.lines)
                 + maybe_string("C#", isc.csharp.lines)
                 + maybe_string("C Header", isc.header.lines)
                 + maybe_string("COBOL", isc.cobol.lines)
                 + maybe_string("Crystal", isc.crystal.lines)
                 + maybe_string("Elixir", isc.elixir.lines)
                 + maybe_string("Elm", isc.elm.lines)
                 + maybe_string("Erlang", isc.erlang.lines)
                 + maybe_string("F#", isc.fsharp.lines)
                 + maybe_string("Fortran", isc.fortran.lines)
                 + maybe_string("Futhark", isc.futhark.lines)
                 + maybe_string("Go", isc.go.lines)
                 + maybe_string("Haskell", isc.haskell.lines)
                 + maybe_string("Idris", isc.idris.lines)
                 + maybe_string("Kotline", isc.kotlin.lines)
                 + maybe_string("J", isc.j.lines)
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
                 + maybe_string("Racket", isc.racket.lines)
                 + maybe_string("Ruby", isc.ruby.lines)
                 + maybe_string("Rust", isc.rust.lines)
                 + maybe_string("Scala", isc.scala.lines)
                 + maybe_string("Shen", isc.shen.lines)
                 + maybe_string("Sixten", isc.sixten.lines)
                 + maybe_string("Standard ML", isc.sml.lines)
                 + maybe_string("Swift", isc.swift.lines)
                 + maybe_string("TCL", isc.tcl.lines)
                 )
    + with_nonempty( editor_string
                   , maybe_string("Emacs Lisp", isc.elisp.lines)
                   + maybe_string("Vimscript", isc.vimscript.lines)
                   )
    + with_nonempty( doc_string
                   , maybe_string("Markdown", isc.markdown.lines)
                   + maybe_string("Plaintext", isc.plaintext.lines)
                   + maybe_string("TeX", isc.tex.lines)
                   )
    + with_nonempty( cfg_string
                   , maybe_string("Cabal", isc.cabal.lines)
                   + maybe_string("Cabal Project", isc.cabal_project.lines)
                   + maybe_string("Dhall", isc.dhall.lines)
                   + maybe_string("iPKG", isc.ipkg.lines)
                   + maybe_string("TOML", isc.toml.lines)
                   + maybe_string("YAML", isc.yaml.lines)
                   )
    + with_nonempty( sh_string
                   , maybe_string("Bash", isc.bash.lines)
                   + maybe_string("Batch", isc.batch.lines)
                   + maybe_string("Dash", isc.dash.lines)
                   + maybe_string("Ion", isc.ion.lines)
                   + maybe_string("PowerShell", isc.powershell.lines)
                   )
    + with_nonempty( parser_string
                   , maybe_string("Alex", isc.alex.lines)
                   + maybe_string("Happy", isc.happy.lines)
                   + maybe_string("LALRPOP", isc.lalrpop.lines)
                   + maybe_string("Lex", isc.lex.lines)
                   + maybe_string("Yacc", isc.yacc.lines)
                   )
    + with_nonempty( web_string
                   , maybe_string("Cassius", isc.cassius.lines)
                   + maybe_string("CSS", isc.css.lines)
                   + maybe_string("Hamlet", isc.hamlet.lines)
                   + maybe_string("HTML", isc.html.lines)
                   + maybe_string("JavaScript", isc.javascript.lines)
                   + maybe_string("Julius", isc.julius.lines)
                   + maybe_string("Lucius", isc.lucius.lines)
                   )
    + with_nonempty(hw_string, maybe_string("Verilog", isc.verilog.lines) + maybe_string("VHDL", isc.vhdl.lines))
    + with_nonempty(gui_string, maybe_string("FLTK Data", isc.fluid.lines))
    + with_nonempty(nb_string, maybe_string("Jupyter", isc.jupyter.lines))
    + with_nonempty(contract_string, maybe_string("Plutus", isc.plutus.lines))
    + with_nonempty( thm_string
                   , maybe_string("Coq", isc.coq.lines)
                   + maybe_string("F*", isc.fstar.lines)
                   + maybe_string("Isabelle", isc.isabelle.lines)
                   )
    + with_nonempty( other
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
