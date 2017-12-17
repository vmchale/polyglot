#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
#include "share/HATS/atslib_staload_libats_libc.hats"
#include "prelude/DATS/filebas.dats"
#include "libats/ML/DATS/filebas_dirent.dats"
#include "libats/libc/DATS/dirent.dats"
#include "src/concurrency.dats"
#include "libats/DATS/athread_posix.dats"
#include "prelude/DATS/stream_vt.dats"

%{^
#include "libats/libc/CATS/string.cats"
#include "prelude/CATS/filebas.cats"
%}

staload "prelude/SATS/stream_vt.sats"
staload "libats/ML/DATS/list0.dats"
staload "libats/SATS/athread.sats"
staload "libats/ML/DATS/string.dats"
staload "libats/libc/SATS/stdio.sats"
staload "prelude/SATS/filebas.sats"
staload "src/filetype.sats"
staload "libats/ML/DATS/filebas.dats"
staload EXTRA = "libats/ML/SATS/filebas.sats"
staload "libats/libc/SATS/unistd.sats"
staload "libats/DATS/athread.dats"

fun to_file(s: string, pre: Option(string)) : file =
  let
    val is_comment =
      case+ pre of
        | Some(x) => string_is_prefix(x, s)
        | None => false
  in
    if s = "" then
      @{ lines = 1, blanks = 1, comments = 0, files = 0 }
    else if is_comment then
      @{ lines = 1, blanks = 0, comments = 1, files = 0 }
    else
      @{ lines = 1, blanks = 0, comments = 0, files = 0 }
  end

fun empty_file(): file =
  let
    var f = @{ files = 0
             , blanks = 0
             , comments = 0
             , lines = 0
             } : file
  in
    f
  end

fun acc_file(): file =
  let
    var f = @{ files = 1
             , blanks = 0
             , comments = 0
             , lines = ~1
             } : file
  in
    f
  end

// monoidal addition for 'file' type
fun add_results(x: file, y: file): file =
  let
    var next = @{ lines = x.lines + y.lines
                , blanks = x.blanks + y.blanks 
                , comments = x.comments + y.comments 
                , files = x.files + y.files 
                }
  in
    next
  end

overload + with add_results

// Given a string representing a filepath, return an integer.
fun line_count(s: string, pre: Option(string)): file =
  let
    var ref = fileref_open_opt(s, file_mode_r)
  in
    case ref of
      | ~Some_vt(x) => 
        begin
          let
            // TODO count all relevant data (should be relatively easy)
            var viewstream: stream_vt(string) = $EXTRA.streamize_fileref_line(x)
            val n: file = stream_vt_foldleft_cloptr(viewstream, acc_file(), lam (acc, f) =<cloptr1> acc + to_file(f, pre))
            val _ = fileref_close(x)
          in
            n
          end
        end
      | ~None_vt() => (println!("[33mWarning:[0m could not open file at " + s) ; to_file(s, None))
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
fun maybe_table{ k : int | k >= 0 && k < 20 }(s: string(k), f: file) : string =
  let
    var code = f.lines - f.comments - f.blanks
  in
    if f.files > 0
      then
        " " + right_pad(s, 21) + left_pad(tostring_int(f.files), 5) + left_pad(tostring_int(f.lines), 12) + left_pad(tostring_int(code), 13) + left_pad(tostring_int(f.comments), 13) + left_pad(tostring_int(f.blanks), 13) + "\n"
    else
      ""
  end

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
                       sc.m4.lines +
                       sc.objective_c.lines +
                       sc.automake.lines
             , blanks = sc.rust.blanks +
                       sc.haskell.blanks +
                       sc.ats.blanks +
                       sc.python.blanks +
                       sc.vimscript.blanks +
                       sc.elm.blanks +
                       sc.idris.blanks +
                       sc.madlang.blanks +
                       sc.tex.blanks +
                       sc.markdown.blanks +
                       sc.yaml.blanks +
                       sc.toml.blanks +
                       sc.cabal.blanks +
                       sc.happy.blanks +
                       sc.alex.blanks +
                       sc.go.blanks +
                       sc.html.blanks +
                       sc.css.blanks +
                       sc.verilog.blanks +
                       sc.vhdl.blanks +
                       sc.c.blanks +
                       sc.purescript.blanks +
                       sc.futhark.blanks +
                       sc.brainfuck.blanks +
                       sc.ruby.blanks +
                       sc.julia.blanks +
                       sc.perl.blanks +
                       sc.ocaml.blanks +
                       sc.agda.blanks +
                       sc.cobol.blanks +
                       sc.tcl.blanks +
                       sc.r.blanks +
                       sc.lua.blanks +
                       sc.cpp.blanks +
                       sc.lalrpop.blanks +
                       sc.header.blanks +
                       sc.sixten.blanks +
                       sc.dhall.blanks +
                       sc.ipkg.blanks +
                       sc.makefile.blanks +
                       sc.justfile.blanks +
                       sc.ion.blanks +
                       sc.bash.blanks +
                       sc.hamlet.blanks +
                       sc.cassius.blanks +
                       sc.lucius.blanks +
                       sc.julius.blanks +
                       sc.mercury.blanks +
                       sc.yacc.blanks +
                       sc.lex.blanks +
                       sc.coq.blanks +
                       sc.jupyter.blanks +
                       sc.java.blanks +
                       sc.scala.blanks +
                       sc.erlang.blanks +
                       sc.elixir.blanks +
                       sc.pony.blanks +
                       sc.clojure.blanks +
                       sc.cabal_project.blanks +
                       sc.assembly.blanks +
                       sc.nix.blanks +
                       sc.php.blanks +
                       sc.javascript.blanks +
                       sc.kotlin.blanks +
                       sc.fsharp.blanks +
                       sc.fortran.blanks +
                       sc.swift.blanks +
                       sc.csharp.blanks +
                       sc.nim.blanks +
                       sc.cpp_header.blanks +
                       sc.elisp.blanks +
                       sc.plaintext.blanks +
                       sc.rakefile.blanks +
                       sc.llvm.blanks +
                       sc.autoconf.blanks +
                       sc.batch.blanks +
                       sc.powershell.blanks +
                       sc.m4.blanks +
                       sc.objective_c.blanks +
                       sc.automake.blanks
             , comments = sc.rust.comments +
                       sc.haskell.comments +
                       sc.ats.comments +
                       sc.python.comments +
                       sc.vimscript.comments +
                       sc.elm.comments +
                       sc.idris.comments +
                       sc.madlang.comments +
                       sc.tex.comments +
                       sc.markdown.comments +
                       sc.yaml.comments +
                       sc.toml.comments +
                       sc.cabal.comments +
                       sc.happy.comments +
                       sc.alex.comments +
                       sc.go.comments +
                       sc.html.comments +
                       sc.css.comments +
                       sc.verilog.comments +
                       sc.vhdl.comments +
                       sc.c.comments +
                       sc.purescript.comments +
                       sc.futhark.comments +
                       sc.brainfuck.comments +
                       sc.ruby.comments +
                       sc.julia.comments +
                       sc.perl.comments +
                       sc.ocaml.comments +
                       sc.agda.comments +
                       sc.cobol.comments +
                       sc.tcl.comments +
                       sc.r.comments +
                       sc.lua.comments +
                       sc.cpp.comments +
                       sc.lalrpop.comments +
                       sc.header.comments +
                       sc.sixten.comments +
                       sc.dhall.comments +
                       sc.ipkg.comments +
                       sc.makefile.comments +
                       sc.justfile.comments +
                       sc.ion.comments +
                       sc.bash.comments +
                       sc.hamlet.comments +
                       sc.cassius.comments +
                       sc.lucius.comments +
                       sc.julius.comments +
                       sc.mercury.comments +
                       sc.yacc.comments +
                       sc.lex.comments +
                       sc.coq.comments +
                       sc.jupyter.comments +
                       sc.java.comments +
                       sc.scala.comments +
                       sc.erlang.comments +
                       sc.elixir.comments +
                       sc.pony.comments +
                       sc.clojure.comments +
                       sc.cabal_project.comments +
                       sc.assembly.comments +
                       sc.nix.comments +
                       sc.php.comments +
                       sc.javascript.comments +
                       sc.kotlin.comments +
                       sc.fsharp.comments +
                       sc.fortran.comments +
                       sc.swift.comments +
                       sc.csharp.comments +
                       sc.nim.comments +
                       sc.cpp_header.comments +
                       sc.elisp.comments +
                       sc.plaintext.comments +
                       sc.rakefile.comments +
                       sc.llvm.comments +
                       sc.autoconf.comments +
                       sc.batch.comments +
                       sc.powershell.comments +
                       sc.m4.comments +
                       sc.objective_c.comments +
                       sc.automake.comments
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
                       sc.m4.files +
                       sc.objective_c.files +
                       sc.automake.files
             }
  in
    f
  end

// function to print tabular output at the end
fun make_table(isc: source_contents): string =
  "-------------------------------------------------------------------------------\n [35mLanguage[0m            [35mFiles[0m        [35mLines[0m         [35mCode[0m     [35mComments[0m       [35mBlanks[0m\n-------------------------------------------------------------------------------\n" +
  maybe_table("Alex", isc.alex) +
  maybe_table("Agda", isc.agda) +
  maybe_table("Assembly", isc.assembly) +
  maybe_table("ATS", isc.ats) +
  maybe_table("Autoconf", isc.autoconf) +
  maybe_table("Automake", isc.automake) +
  maybe_table("Bash", isc.bash) +
  maybe_table("Batch", isc.batch) +
  maybe_table("Brainfuck", isc.brainfuck) +
  maybe_table("C", isc.c) +
  maybe_table("C Header", isc.header) +
  maybe_table("C++ cpp_header", isc.cpp_header) +
  maybe_table("C++", isc.cpp) +
  maybe_table("C#", isc.csharp) +
  maybe_table("Cabal", isc.cabal) +
  maybe_table("Cabal Project", isc.cabal_project) +
  maybe_table("Cassius", isc.cassius) +
  maybe_table("COBOL", isc.cobol) +
  maybe_table("Coq", isc.coq) +
  maybe_table("CSS", isc.css) +
  maybe_table("Dhall", isc.dhall) +
  maybe_table("Elixir", isc.elixir) +
  maybe_table("Elm", isc.elm) +
  maybe_table("Emacs Lisp", isc.elisp) +
  maybe_table("Erlang", isc.erlang) +
  maybe_table("F#", isc.fsharp) +
  maybe_table("Fortran", isc.fortran) +
  maybe_table("Go", isc.go) +
  maybe_table("Hamlet", isc.hamlet) +
  maybe_table("Happy", isc.happy) +
  maybe_table("Haskell", isc.haskell) +
  maybe_table("HTML", isc.html) +
  maybe_table("Idris", isc.idris) +
  maybe_table("iPKG", isc.ipkg) +
  maybe_table("Ion", isc.ion) +
  maybe_table("Java", isc.java) +
  maybe_table("JavaScript", isc.javascript) +
  maybe_table("Julius", isc.julius) +
  maybe_table("Julia", isc.julia) +
  maybe_table("Jupyter", isc.jupyter) +
  maybe_table("Justfile", isc.justfile) +
  maybe_table("Kotlin", isc.kotlin) +
  maybe_table("LALRPOP", isc.lalrpop) +
  maybe_table("Lex", isc.lex) +
  maybe_table("LLVM", isc.llvm) +
  maybe_table("Lua", isc.lua) +
  maybe_table("Lucius", isc.lucius) +
  maybe_table("M4", isc.m4) +
  maybe_table("Madlang", isc.madlang) +
  maybe_table("Makefile", isc.makefile) +
  maybe_table("Margaret", isc.margaret) +
  maybe_table("Markdown", isc.markdown) +
  maybe_table("Mercury", isc.mercury) +
  maybe_table("Nim", isc.nim) +
  maybe_table("Nix", isc.nix) +
  maybe_table("Objective C", isc.objective_c) +
  maybe_table("OCaml", isc.ocaml) +
  maybe_table("Perl", isc.perl) +
  maybe_table("PHP", isc.php) +
  maybe_table("Plaintext", isc.plaintext) +
  maybe_table("PowerShell", isc.powershell) +
  maybe_table("Pony", isc.pony) +
  maybe_table("Python", isc.python) +
  maybe_table("PureScript", isc.purescript) +
  maybe_table("R", isc.r) +
  maybe_table("Rakefile", isc.rakefile) +
  maybe_table("Ruby", isc.ruby) +
  maybe_table("Rust", isc.rust) +
  maybe_table("Scala", isc.scala) +
  maybe_table("Sixten", isc.sixten) +
  maybe_table("Swift", isc.swift) +
  maybe_table("TCL", isc.tcl) +
  maybe_table("TeX", isc.tex) +
  maybe_table("TOML", isc.toml) +
  maybe_table("Verilog", isc.verilog) +
  maybe_table("VHDL", isc.vhdl) +
  maybe_table("Vimscript", isc.vimscript) +
  maybe_table("Yacc", isc.yacc) +
  maybe_table("YAML", isc.yaml) +
  "-------------------------------------------------------------------------------\n" +
  maybe_table("Total", (sum_fields(isc))) +
  "-------------------------------------------------------------------------------\n"

// Function to print output sorted by type of language.
fun make_output(isc: source_contents): string =
  with_nonempty("[33mProgramming Languages:[0m\n",
    maybe_string("Agda", isc.agda.lines) +
    maybe_string("Assembly", isc.assembly.lines) +
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
    maybe_string("Erlang", isc.erlang.lines) +
    maybe_string("F#", isc.fsharp.lines) +
    maybe_string("Fortran", isc.fortran.lines) +
    maybe_string("Go", isc.go.lines) +
    maybe_string("Haskell", isc.haskell.lines) +
    maybe_string("Idris", isc.idris.lines) +
    maybe_string("Kotline", isc.kotlin.lines) +
    maybe_string("Java", isc.java.lines) +
    maybe_string("Julia", isc.julia.lines) +
    maybe_string("Lua", isc.lua.lines) +
    maybe_string("Margaret", isc.margaret.lines) +
    maybe_string("Mercury", isc.mercury.lines) +
    maybe_string("Nim", isc.nim.lines) +
    maybe_string("Objective C", isc.objective_c.lines) +
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
    maybe_string("Cabal Project", isc.cabal_project.lines) +
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
  with_nonempty("\n[33mNotebooks:[0m\n",
    maybe_string("Jupyter", isc.jupyter.lines)
  ) +
  with_nonempty("\n[33mOther:[0m\n",
    maybe_string("Autoconf", isc.autoconf.lines) +
    maybe_string("Automake", isc.automake.lines) +
    maybe_string("Justfile", isc.justfile.lines) +
    maybe_string("LLVM", isc.llvm.lines) +
    maybe_string("M4", isc.m4.lines) +
    maybe_string("Madlang", isc.madlang.lines) +
    maybe_string("Makefile", isc.makefile.lines) +
    maybe_string("Rakefile", isc.rakefile.lines)
  )

fun add_contents(x: source_contents, y: source_contents) : source_contents =
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
                } : source_contents
  in
    next
  end

// This is the step function used when streaming directory contents. 
fun adjust_contents(prev: source_contents, scf: pl_type) : source_contents =
  let
    val sc_r = ref<source_contents>(prev)
    val _ =
      case+ scf of
        | ~haskell n => sc_r->haskell := prev.haskell + n
        | ~ats n => sc_r->ats := prev.ats + n
        | ~rust n => sc_r->rust := prev.rust + n
        | ~markdown n => sc_r->markdown := prev.markdown + n
        | ~python n => sc_r->python := prev.python + n
        | ~vimscript n => sc_r->vimscript := prev.vimscript + n
        | ~yaml n => sc_r->yaml := prev.yaml + n
        | ~toml n => sc_r->toml := prev.toml + n
        | ~happy n => sc_r->happy := prev.happy + n
        | ~alex n => sc_r->alex := prev.alex + n
        | ~idris n => sc_r->idris := prev.idris + n
        | ~madlang n => sc_r->madlang := prev.madlang + n
        | ~elm n => sc_r->elm := prev.elm + n
        | ~c n => sc_r->c := prev.c + n
        | ~go n => sc_r->go := prev.go + n
        | ~cabal n => sc_r->cabal := prev.cabal + n
        | ~verilog n => sc_r->verilog := prev.verilog + n
        | ~vhdl n => sc_r->vhdl := prev.vhdl + n
        | ~html n => sc_r->html := prev.html + n
        | ~css n => sc_r->css := prev.css + n
        | ~purescript n => sc_r->purescript := prev.purescript + n
        | ~futhark n => sc_r->futhark := prev.futhark + n
        | ~brainfuck n => sc_r->brainfuck := prev.brainfuck + n
        | ~ruby n => sc_r->ruby := prev.ruby + n
        | ~julia n => sc_r->julia := prev.julia + n
        | ~tex n => sc_r->tex := prev.tex + n
        | ~perl n => sc_r->perl := prev.perl + n
        | ~ocaml n => sc_r->ocaml := prev.ocaml + n
        | ~agda n => sc_r->agda := prev.agda + n
        | ~cobol n => sc_r->cobol := prev.cobol + n
        | ~tcl n => sc_r->tcl := prev.tcl + n
        | ~r n => sc_r->r := prev.r + n
        | ~lua n => sc_r->lua := prev.lua + n
        | ~cpp n => sc_r->cpp := prev.cpp + n
        | ~lalrpop n => sc_r->lalrpop := prev.lalrpop + n
        | ~header n => sc_r->header := prev.header + n
        | ~sixten n => sc_r->sixten := prev.sixten + n
        | ~dhall n => sc_r->dhall := prev.dhall + n
        | ~ipkg n => sc_r->ipkg := prev.ipkg + n
        | ~justfile n => sc_r->justfile := prev.justfile + n
        | ~makefile n => sc_r->makefile := prev.makefile + n
        | ~ion n => sc_r->ion := prev.ion + n
        | ~bash n => sc_r->bash := prev.bash + n
        | ~hamlet n => sc_r->hamlet := prev.hamlet + n
        | ~cassius n => sc_r->cassius := prev.cassius + n
        | ~lucius n => sc_r->lucius := prev.lucius + n
        | ~julius n => sc_r->julius := prev.julius + n
        | ~mercury n => sc_r->mercury := prev.mercury + n
        | ~yacc n => sc_r->yacc := prev.yacc + n
        | ~lex n => sc_r->lex := prev.lex + n
        | ~coq n => sc_r->coq := prev.coq + n
        | ~jupyter n => sc_r->jupyter := prev.jupyter + n
        | ~java n => sc_r->java := prev.java + n
        | ~scala n => sc_r->scala := prev.scala + n
        | ~erlang n => sc_r->erlang := prev.erlang + n
        | ~elixir n => sc_r->elixir := prev.elixir + n
        | ~pony n => sc_r->pony := prev.pony + n
        | ~clojure n => sc_r->clojure := prev.clojure + n
        | ~cabal_project n => sc_r->cabal_project := prev.cabal_project + n
        | ~assembly n => sc_r->assembly := prev.assembly + n
        | ~nix n => sc_r->nix := prev.nix + n
        | ~php n => sc_r->php := prev.php + n
        | ~javascript n => sc_r->javascript := prev.javascript + n
        | ~kotlin n => sc_r->kotlin := prev.kotlin + n
        | ~fsharp n => sc_r->fsharp := prev.fsharp + n
        | ~fortran n => sc_r->fortran := prev.fortran + n
        | ~swift n => sc_r->swift := prev.swift + n
        | ~csharp n => sc_r->csharp := prev.csharp + n
        | ~nim n => sc_r->nim := prev.nim + n
        | ~cpp_header n => sc_r->cpp_header := prev.cpp_header + n
        | ~elisp n => sc_r->elisp := prev.elisp + n
        | ~plaintext n => sc_r->plaintext := prev.plaintext + n
        | ~rakefile n => sc_r->rakefile := prev.rakefile + n
        | ~llvm n => sc_r->llvm := prev.llvm + n
        | ~autoconf n => sc_r->autoconf := prev.autoconf + n
        | ~batch n => sc_r->batch := prev.batch + n
        | ~powershell n => sc_r->powershell := prev.powershell + n
        | ~m4 n => sc_r->m4 := prev.m4 + n
        | ~objective_c n => sc_r->objective_c := prev.objective_c + n
        | ~automake n => sc_r->automake := prev.automake + n
        | ~margaret n => sc_r->margaret := prev.margaret + n
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
    | ~objective_c _ => ()
    | ~automake _ => ()
    | ~margaret _ => ()

// match a particular word against a list of keywords
fun match_keywords { m : nat | m <= 10 } (keys: list(string, m), word: string) : bool =
  list_foldright_cloref(keys, lam (next, acc) =<cloref1> acc || eq_string_string(next, word), false) // next = word, false)

// helper function for check_keywords
fun step_keyword(size: file, pre: pl_type, word: string, ext: string) : pl_type =
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
              var mercury_keywords = list_cons("module", list_cons("pred", list_cons("mode", list_nil())))
            in
              if match_keywords(mercury_keywords, word)
              then mercury size
              else if
                let
                  var objective_c_keywords = list_cons("nil", list_cons("nullable", list_cons("nonnull", list_nil())))
                in
                  match_keywords(objective_c_keywords, word)
                end
                  then
                    objective_c size
              else unknown
            end
          | _ => pre
      end
    | _ => pre

// Function to disambiguate extensions such as .v (Coq and Verilog) and .m
// (Mercury and Objective C). This should only be called when extensions are in
// conflict, as it reads the whole file.
fun check_keywords(s: string, size: file, ext: string) : pl_type =
  let
    var ref = fileref_open_opt(s, file_mode_r)
  in
    case+ ref of
      | ~Some_vt(x) =>
        let
          var init: pl_type = unknown
          var viewstream = $EXTRA.streamize_fileref_word(x) // TODO count lines simultaneously
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
      | "#!/usr/bin/env ion" => ion(line_count(s, Some("#")))
      | "#!/usr/bin/env bash" => bash(line_count(s, Some("#")))
      | "#!/bin/bash" => bash(line_count(s, Some("#")))
      | "#!python" => python(line_count(s, Some("#")))
      | "#!python2" => python(line_count(s, Some("#")))
      | "#!python3" => python(line_count(s, Some("#")))
      | "#!/usr/bin/env python" => python(line_count(s, Some("#")))
      | "#!/usr/bin/env python2" => python(line_count(s, Some("#")))
      | "#!/usr/bin/env python3" => python(line_count(s, Some("#")))
      | "#!/usr/bin/env perl" => perl(line_count(s, Some("#")))
      | "#!/usr/bin/env perl6" => perl(line_count(s, Some("#")))
      | "#!/usr/bin/perl" => perl(line_count(s, Some("#")))
      | "#!/usr/bin/env stack" => haskell(line_count(s, Some("--")))
      | "#!/usr/bin/env runhaskell" => haskell(line_count(s, Some("--")))
      | "#!/usr/bin/env node" => javascript(line_count(s, None))
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
      | "Makefile" => makefile(line_count(s, Some("#")))
      | "Makefile.tc" => makefile(line_count(s, Some("#")))
      | "makefile" => makefile(line_count(s, Some("#")))
      | "GNUmakefile" => makefile(line_count(s, Some("#")))
      | "Justfile" => justfile(line_count(s, Some("#")))
      | "justfile" => justfile(line_count(s, Some("#")))
      | "Rakefile" => rakefile(line_count(s, None))
      | "cabal.project.local" => cabal_project(line_count(s, Some("--")))
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
      | "hs" => haskell(line_count(s, Some("--")))
      | "hs-boot" => haskell(line_count(s, Some("--")))
      | "hsig" => haskell(line_count(s, Some("--"))) // TODO consider haskell signature type?
      | "rs" => rust(line_count(s, Some("//")))
      | "tex" => tex(line_count(s, None))
      | "md" => markdown(line_count(s, None))
      | "markdown" => markdown(line_count(s, None))
      | "dats" => ats(line_count(s, Some("//")))
      | "hats" => ats(line_count(s, Some("//")))
      | "cats" => ats(line_count(s, Some("//")))
      | "sats" => ats(line_count(s, Some("//")))
      | "py" => python(line_count(s, None))
      | "fut" => futhark(line_count(s, Some("--")))
      | "pl" => perl(line_count(s, None))
      | "agda" => agda(line_count(s, Some("--")))
      | "idr" => idris(line_count(s, Some("--")))
      | "v" => check_keywords(s, line_count(s, Some("--")), match)
      | "m" => check_keywords(s, line_count(s, None), match)
      | "vhdl" => vhdl(line_count(s, None))
      | "vhd" => vhdl(line_count(s, None))
      | "go" => go(line_count(s, Some("//")))
      | "vim" => vimscript(line_count(s, None))
      | "ml" => ocaml(line_count(s, None))
      | "purs" => purescript(line_count(s, None))
      | "elm" => elm(line_count(s, Some("--")))
      | "mad" => madlang(line_count(s, Some("#")))
      | "toml" => toml(line_count(s, Some("#")))
      | "cabal" => cabal(line_count(s, Some("--")))
      | "yml" => yaml(line_count(s, Some("#")))
      | "yaml" => yaml(line_count(s, Some("#")))
      | "y" => check_keywords(s, line_count(s, None), match)
      | "ypp" => yacc(line_count(s, Some("//")))
      | "x" => alex(line_count(s, Some("--")))
      | "l" => lex(line_count(s, None))
      | "lpp" => lex(line_count(s, None))
      | "html" => html(line_count(s, None))
      | "htm" => html(line_count(s, None))
      | "css" => css(line_count(s, None))
      | "vhdl" => vhdl(line_count(s, None))
      | "vhd" => vhdl(line_count(s, None))
      | "c" => c(line_count(s, Some("//")))
      | "b" => brainfuck(line_count(s, None))
      | "bf" => brainfuck(line_count(s, None))
      | "rb" => ruby(line_count(s, None))
      | "cob" => cobol(line_count(s, None))
      | "cbl" => cobol(line_count(s, None))
      | "cpy" => cobol(line_count(s, None)) // TODO no separation of programs/copybooks yet.
      | "ml" => ocaml(line_count(s, None))
      | "tcl" => tcl(line_count(s, None))
      | "r" => r(line_count(s, None))
      | "R" => r(line_count(s, None))
      | "lua" => lua(line_count(s, None))
      | "cpp" => cpp(line_count(s, Some("//")))
      | "cc" => cpp(line_count(s, Some("//")))
      | "lalrpop" => lalrpop(line_count(s, Some("//")))
      | "h" => header(line_count(s, None))
      | "vix" => sixten(line_count(s, Some("--")))
      | "dhall" => dhall(line_count(s, None))
      | "ipkg" => ipkg(line_count(s, Some("--")))
      | "mk" => makefile(line_count(s, Some("#")))
      | "hamlet" => hamlet(line_count(s, None))
      | "cassius" => cassius(line_count(s, None))
      | "lucius" => cassius(line_count(s, None))
      | "julius" => julius(line_count(s, None))
      | "jl" => julia(line_count(s, None))
      | "ion" => ion(line_count(s, Some("#")))
      | "bash" => bash(line_count(s, Some("#")))
      | "ipynb" => jupyter(line_count(s, None))
      | "java" => java(line_count(s, None))
      | "scala" => scala(line_count(s, None))
      | "erl" => erlang(line_count(s, None))
      | "hrl" => erlang(line_count(s, None))
      | "ex" => elixir(line_count(s, None))
      | "exs" => elixir(line_count(s, None))
      | "pony" => pony(line_count(s, None))
      | "clj" => clojure(line_count(s, None))
      | "s" => assembly(line_count(s, Some(";")))
      | "S" => assembly(line_count(s, Some(";")))
      | "asm" => assembly(line_count(s, Some(";")))
      | "nix" => nix(line_count(s, None))
      | "php" => php(line_count(s, None))
      | "local" => match_filename(s)
      | "project" => cabal_project(line_count(s, Some("--")))
      | "js" => javascript(line_count(s, None))
      | "jsexe" => javascript(line_count(s, None))
      | "kt" => kotlin(line_count(s, None))
      | "kts" => kotlin(line_count(s, None))
      | "fs" => fsharp(line_count(s, None))
      | "f" => fortran(line_count(s, None))
      | "for" => fortran(line_count(s, None))
      | "f90" => fortran(line_count(s, None))
      | "f95" => fortran(line_count(s, None))
      | "swift" => swift(line_count(s, None))
      | "csharp" => csharp(line_count(s, None))
      | "nim" => nim(line_count(s, None))
      | "el" => elisp(line_count(s, None))
      | "txt" => plaintext(line_count(s, None))
      | "ll" => llvm(line_count(s, None))
      | "in" => autoconf(line_count(s, Some("#")))
      | "bat" => batch(line_count(s, None))
      | "ps1" => powershell(line_count(s, None))
      | "ac" => m4(line_count(s, None))
      | "mm" => objective_c(line_count(s, Some("//")))
      | "am" => automake(line_count(s, Some("#")))
      | "mgt" => margaret(line_count(s, Some("--")))
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
    | _ => list_exists_cloref(excludes, lam x => x = s || x = s + "/")

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
               } : source_contents
  in
    isc
  end

fun map_stream(acc: source_contents, includes: List0(string), excludes: List0(string)) : source_contents =
  list_foldleft_cloref(includes, acc, lam (acc, next) => if test_file_exists(next) || next = "" then step_stream(acc, next, next, excludes) else (prerr("[31mError:[0m directory '" + next + "' does not exist\n") ; exit(1) ; acc))

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
            | "--no-table" => if not(acc.no_table) then
              acc_r->no_table := true
              else (println!("[31mError:[0m flag " + s + " cannot appear twice") ; exit(0) ; ())
            | "-t" => if not(acc.no_table) then
              acc_r->no_table := true
              else (println!("[31mError:[0m flag " + s + " cannot appear twice") ; exit(0) ; ())
            | "--parallel" => acc_r->parallel := true
            | "-p" => acc_r->parallel := true
            | "--version" => acc_r->version := true
            | "-V" => acc_r->version := true
            | "-e" => (println!("[31mError:[0m flag " + s + " must be followed by an argument") ; exit(0) ; ())
            | "--exclude" => (println!("[31mError:[0m flag " + s + " must be followed by an argument") ; exit(0) ; ())
            | _ => (println!("[31mError:[0m flag '" + s + "' not recognized") ; exit(0) ; ())
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
  println!("polygot version 0.3.10\nCopyright (c) 2017 Vanessa McHale")

fun help(): void = 
print("polyglot - Count lines of code quickly.

[36mUSAGE:[0m poly [DIRECTORY] ... [OPTION] ...

[36mFLAGS:[0m
    -V, --version            show version information
    -h, --help               display this help and exit
    -e, --exclude            exclude a directory
    -p, --parallel           execute in parallel
    -t, --no-table           display results in alternate format

When no directory is provided poly will execute in the
current directory.

Bug reports and updates: nest.pijul.com/vamchale/polyglot\n")

fun head(xs: List0(string)) : string =
  case+ xs of
    | list_cons(x, xs) => x + ", " + head(xs)
    | list_nil() => ""

// TODO channel to draw work? e.g. take a channel of strings, return a channel of source_contents
fun work(excludes: List0(string), send: channel(List0(string)), chan: channel(source_contents)) : void =
  {

    val- (n) = channel_remove(send)
    var x = map_stream(empty_contents(), n, excludes)
    val () = channel_insert(chan, x)
    val- ~None_vt() = channel_unref(chan)

    // TODO make this end when it receives a Done()?
    val- () = case channel_unref<List0(string)>(send) of
      | ~None_vt() => ()
      | ~Some_vt(snd) => (queue_free<List0(string)>(snd))

  }

// Function returning the number of CPU cores.
extern fun ncpu() : int

%{^
#include <unistd.h>
int ncpu() {
  return sysconf(_SC_NPROCESSORS_ONLN);
}
%}

#define NCPU 4

fun apportion(includes: List0(string)) : (List0(string), List0(string)) = 
  let
    var n = length(includes) / 2
    val (p, q) = list_split_at(includes, n)
  in
    (list_vt2t(p), q)
  end

// TODO maybe make a parallel fold?
fun threads(includes: List0(string), excludes: List0(string)) : source_contents =
  let
    // channel containing outputs
    val chan = channel_make<source_contents>(2)
    val chan2 = channel_ref(chan)
    val chan3 = channel_ref(chan)

    // channel containing inputs
    val send1 = channel_make<List0(string)>(1)
    val send2 = channel_make<List0(string)>(1)
    val send_r1 = channel_ref(send1)
    val send_r2 = channel_ref(send2)
    
    var new_includes = 
      if length(includes) > 0 then
        includes
      else
        list_cons(".", list_nil())
    // insert inputs into channel.
    val (fst, snd) = apportion(new_includes)
    val _ = channel_insert(send1, fst)
    val _ = channel_insert(send2, snd)

    // create threads to do work
    val t2 = athread_create_cloptr_exn(llam () => work(excludes, send_r1, chan2))
    val t3 = athread_create_cloptr_exn(llam () => work(excludes, send_r2, chan3))

    // free sending channel
    val- ~None_vt() = channel_unref(send1)
    val- ~None_vt() = channel_unref(send2)

    // take outputs from each thread
    val- (n) = channel_remove(chan)
    val- (m) = channel_remove(chan)

    // only one μs so not really a problem.
    val () = ignoret(usleep(1u))

    // wait until workers are done
    val () = while(channel_refcount(chan) >= 2) ()

    // print total
    val r = add_contents(n, m)

    // free sending channel
    val- ~Some_vt(que) = channel_unref<source_contents>(chan)

    // free the queue
    val () = queue_free<source_contents>(que)

  in
    r
  end

implement main0 (argc, argv) =
  let
    val cli = @{ version = false
               , help = false
               , no_table = false
               , parallel = false
               , excludes = list_nil()
               , includes = list_nil()
               } : command_line
    val parsed = get_cli(argc, argv, 0, false, cli)
  in
    if parsed.help
      then
        ( help() ; exit(0) )
    else if parsed.version
      then
        ( version() ; exit(0) )
    else
      let
        val result = 
          if parsed.parallel then
            threads(parsed.includes, parsed.excludes)
          else if length(parsed.includes) > 0 then
            map_stream(empty_contents(), parsed.includes, parsed.excludes)
          else
            map_stream(empty_contents(), list_cons(".", list_nil()), parsed.excludes)
      in
        if parsed.no_table
          then
            print(make_output(result))
        else
          print(make_table(result))
      end
  end
