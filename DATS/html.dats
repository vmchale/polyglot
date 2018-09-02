staload "SATS/filetype.sats"
staload "SATS/error.sats"
staload "libats/ML/SATS/string.sats"
staload "libats/libc/SATS/stdio.sats"

fn maybe_html_table(s : string, f : file) : string =
  let
    fn th(s : string) : string =
      "<th><div class=\"cell\">" + s + "</th></div>"
    
    var code = f.lines - f.comments - f.blanks
  in
    if f.files > 0 then
      "<tr>"
      + th(s)
      + th(tostring_int(f.files))
      + th(tostring_int(f.lines))
      + th(tostring_int(code))
      + th(tostring_int(f.comments))
      + th(tostring_int(f.blanks))
      + "</tr>"
    else
      ""
  end

fn make_html(isc : source_contents) : string =
  let
    var header = "<head>
<style>
table {
    border-collapse: collapse;
    width: 60%
}

td, th {
    border: 1px solid #a6a6a6;
    text-align: left;
    padding: 8px
}

div.cell {
    font-weight: 100
}

body {
    color: #222;
    font-family: \"Palatino Linotype\", \"Book Antiqua\", Palatino, Georgia, serif
}

tr:nth-child(even) {
    background-color: #dddddd
}
</style></head><body><table><tr><th>Language</th><th>Files</th><th>Lines</th><th>Code</th><th>Comments</th><th>Blanks</th></tr>"
    var content = maybe_html_table("Ada", isc.ada)
    + maybe_html_table("Agda", isc.agda)
    + maybe_html_table("Alex", isc.alex)
    + maybe_html_table("Apex", isc.apex)
    + maybe_html_table("Assembly", isc.assembly)
    + maybe_html_table("ATS", isc.ats)
    + maybe_html_table("Awk", isc.awk)
    + maybe_html_table("Autoconf", isc.autoconf)
    + maybe_html_table("Automake", isc.automake)
    + maybe_html_table("Bash", isc.bash)
    + maybe_html_table("Batch", isc.batch)
    + maybe_html_table("Blodwen", isc.blodwen)
    + maybe_html_table("Brainfuck", isc.brainfuck)
    + maybe_html_table("C", isc.c)
    + maybe_html_table("Carp", isc.carp)
    + maybe_html_table("C--", isc.cmm)
    + maybe_html_table("C++ Header", isc.cpp_header)
    + maybe_html_table("C++", isc.cpp)
    + maybe_html_table("C#", isc.csharp)
    + maybe_html_table("C Header", isc.header)
    + maybe_html_table("Cabal", isc.cabal)
    + maybe_html_table("Cabal Project", isc.cabal_project)
    + maybe_html_table("Cassius", isc.cassius)
    + maybe_html_table("Chapel", isc.chapel)
    + maybe_html_table("Clean", isc.clean)
    + maybe_html_table("COBOL", isc.cobol)
    + maybe_html_table("CoffeeScript", isc.coffeescript)
    + maybe_html_table("Cogent", isc.cogent)
    + maybe_html_table("Coq", isc.coq)
    + maybe_html_table("Crystal", isc.crystal)
    + maybe_html_table("CSS", isc.css)
    + maybe_html_table("D", isc.d)
    + maybe_html_table("Dart", isc.dart)
    + maybe_html_table("Dash", isc.dash)
    + maybe_html_table("Dhall", isc.dhall)
    + maybe_html_table("Egison", isc.egison)
    + maybe_html_table("Eiffel", isc.eiffel)
    + maybe_html_table("Elixir", isc.elixir)
    + maybe_html_table("Elm", isc.elm)
    + maybe_html_table("Emacs Lisp", isc.elisp)
    + maybe_html_table("Erlang", isc.erlang)
    + maybe_html_table("F#", isc.fsharp)
    + maybe_html_table("F*", isc.fstar)
    + maybe_html_table("Factor", isc.factor)
    + maybe_html_table("Felix", isc.felix)
    + maybe_html_table("Fish", isc.fish)
    + maybe_html_table("FLTK Data", isc.fluid)
    + maybe_html_table("Fortran", isc.fortran)
    + maybe_html_table("Frege", isc.frege)
    + maybe_html_table("Futhark", isc.futhark)
    + maybe_html_table("Go", isc.go)
    + maybe_html_table("Greencard", isc.greencard)
    + maybe_html_table("Hamlet", isc.hamlet)
    + maybe_html_table("Happy", isc.happy)
    + maybe_html_table("Haskell", isc.haskell)
    + maybe_html_table("Haxe", isc.haxe)
    + maybe_html_table("HTML", isc.html)
    + maybe_html_table("Idris", isc.idris)
    + maybe_html_table("iPKG", isc.ipkg)
    + maybe_html_table("Ion", isc.ion)
    + maybe_html_table("Isabelle", isc.isabelle)
    + maybe_html_table("J", isc.j)
    + maybe_html_table("Jai", isc.jai)
    + maybe_html_table("Java", isc.java)
    + maybe_html_table("JavaScript", isc.javascript)
    + maybe_html_table("Julius", isc.julius)
    + maybe_html_table("Julia", isc.julia)
    + maybe_html_table("Jupyter", isc.jupyter)
    + maybe_html_table("Justfile", isc.justfile)
    + maybe_html_table("K", isc.k)
    + maybe_html_table("Kotlin", isc.kotlin)
    + maybe_html_table("LALRPOP", isc.lalrpop)
    + maybe_html_table("Lex", isc.lex)
    + maybe_html_table("LLVM", isc.llvm)
    + maybe_html_table("Lua", isc.lua)
    + maybe_html_table("Lucius", isc.lucius)
    + maybe_html_table("M4", isc.m4)
    + maybe_html_table("Madlang", isc.madlang)
    + maybe_html_table("Makefile", isc.makefile)
    + maybe_html_table("Margaret", isc.margaret)
    + maybe_html_table("Markdown", isc.markdown)
    + maybe_html_table("Mercury", isc.mercury)
    + maybe_html_table("Nim", isc.nim)
    + maybe_html_table("Nix", isc.nix)
    + maybe_html_table("Nu", isc.nu)
    + maybe_html_table("Objective C", isc.objective_c)
    + maybe_html_table("OCaml", isc.ocaml)
    + maybe_html_table("Oz", isc.oz)
    + maybe_html_table("Pascal", isc.pascal)
    + maybe_html_table("Perl", isc.perl)
    + maybe_html_table("PHP", isc.php)
    + maybe_html_table("Plaintext", isc.plaintext)
    + maybe_html_table("Plutus", isc.plutus)
    + maybe_html_table("PowerShell", isc.powershell)
    + maybe_html_table("Pony", isc.pony)
    + maybe_html_table("Python", isc.python)
    + maybe_html_table("PureScript", isc.purescript)
    + maybe_html_table("Q#", isc.qsharp)
    + maybe_html_table("R", isc.r)
    + maybe_html_table("Racket", isc.racket)
    + maybe_html_table("Ragel", isc.ragel)
    + maybe_html_table("Rakefile", isc.rakefile)
    + maybe_html_table("Red", isc.red)
    + maybe_html_table("Ruby", isc.ruby)
    + maybe_html_table("Rust", isc.rust)
    + maybe_html_table("SAS", isc.sas)
    + maybe_html_table("Scala", isc.scala)
    + maybe_html_table("Scheme", isc.scheme)
    + maybe_html_table("Sed", isc.sed)
    + maybe_html_table("Shen", isc.shen)
    + maybe_html_table("Sixten", isc.sixten)
    + maybe_html_table("Solidity", isc.solidity)
    + maybe_html_table("SQL", isc.sql)
    + maybe_html_table("Standard ML", isc.sml)
    + maybe_html_table("Swift", isc.swift)
    + maybe_html_table("TCL", isc.tcl)
    + maybe_html_table("TeX", isc.tex)
    + maybe_html_table("Thrift", isc.thrift)
    + maybe_html_table("TLA+", isc.tla)
    + maybe_html_table("TOML", isc.toml)
    + maybe_html_table("TypeScript", isc.typescript)
    + maybe_html_table("Vala", isc.vala)
    + maybe_html_table("Verilog", isc.verilog)
    + maybe_html_table("VHDL", isc.vhdl)
    + maybe_html_table("Vimscript", isc.vimscript)
    + maybe_html_table("Visual Basic", isc.vb)
    + maybe_html_table("Volt", isc.volt)
    + maybe_html_table("Yacc", isc.yacc)
    + maybe_html_table("YAML", isc.yaml)
    + maybe_html_table("XML", isc.xml)
    + maybe_html_table("Zig", isc.zig)
    + maybe_html_table("Zimpl", isc.zimpl)
    var footer = "</table></body>"
  in
    header + content + footer
  end

fn write_report(isc : source_contents, path : string) : void =
  let
    val ref = fileref_open_opt(path, file_mode_w)
    val () = case+ ref of
      | ~Some_vt (x) => let
        val to_write = make_html(isc)
        val () = fputs0_exn(to_write, x)
        val () = fileref_close(x)
      in end
      | ~None_vt() => bad_file(path)
  in end
