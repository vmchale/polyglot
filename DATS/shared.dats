#include "DATS/cli.dats"
#include "DATS/count-loop.dats"
#include "DATS/print.dats"

staload "libats/ML/SATS/string.sats"
staload "SATS/filetype.sats"
staload "libats/ML/SATS/atspre.sats"
staload "libats/ML/SATS/list0.sats"
staload "libats/ML/SATS/basis.sats"
staload EXTRA = "libats/ML/SATS/filebas.sats"
staload "libats/ML/DATS/filebas_dirent.dats"
staload "libats/libc/SATS/unistd.sats"
staload _ = "libats/ML/DATS/atspre.dats"
staload _ = "libats/ML/DATS/list0.dats"
staload _ = "libats/ML/DATS/filebas.dats"
staload "SATS/print.sats"

#define nil list_nil
#define :: list_cons

val empty_file = let
  var f = @{ files = 0, blanks = 0, comments = 0, lines = 0 } : file
in
  f
end

fn to_file(s : string, pre : Option(string)) : file =
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

fn add_contents(x : source_contents, y : source_contents) : source_contents =
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
                , sml = x.sml + y.sml
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
                , dash = x.dash + y.dash
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
                , carp = x.carp + y.carp
                , shen = x.shen + y.shen
                , greencard = x.greencard + y.greencard
                , cmm = x.cmm + y.cmm
                , fluid = x.fluid + y.fluid
                , plutus = x.plutus + y.plutus
                , j = x.j + y.j
                , blodwen = x.blodwen + y.blodwen
                , crystal = x.crystal + y.crystal
                , racket = x.racket + y.racket
                , ada = x.ada + y.ada
                , isabelle = x.isabelle + y.isabelle
                , fstar = x.fstar + y.fstar
                , d = x.d + y.d
                , factor = x.factor + y.factor
                , scheme = x.scheme + y.scheme
                , chapel = x.chapel + y.chapel
                , pascal = x.pascal + y.pascal
                , ragel = x.ragel + y.ragel
                , xml = x.xml + y.xml
                , awk = x.awk + y.awk
                , sed = x.sed + y.sed
                , k = x.k + y.k
                , typescript = x.typescript + y.typescript
                , coffeescript = x.coffeescript + y.coffeescript
                , red = x.red + y.red
                , fish = x.fish + y.fish
                , vb = x.vb + y.vb
                , frege = x.frege + y.frege
                , dart = x.dart + y.dart
                , solidity = x.solidity + y.solidity
                , egison = x.egison + y.egison
                , zig = x.zig + y.zig
                , sql = x.sql + y.sql
                , felix = x.felix + y.felix
                , qsharp = x.qsharp + y.qsharp
                , oz = x.oz + y.oz
                , jai = x.jai + y.jai
                , zimpl = x.zimpl + y.zimpl
                , volt = x.volt + y.volt
                , cogent = x.cogent + y.cogent
                , clean = x.clean + y.clean
                , thrift = x.thrift + y.thrift
                , vala = x.vala + y.vala
                , apex = x.apex + y.apex
                , sas = x.sas + y.sas
                , nu = x.nu + y.nu
                , haxe = x.haxe + y.haxe
                , eiffel = x.eiffel + y.eiffel
                , tla = x.tla + y.tla
                , lean = x.lean + y.lean
                , io = x.io + y.io
                , squirrel = x.squirrel + y.squirrel
                , agdalib = x.agdalib + y.agdalib
                , cedille = x.cedille + y.cedille
                , raml = x.raml + y.raml
                , scribble = x.scribble + y.scribble
                , bibtex = x.bibtex + y.bibtex
                , csv = x.csv + y.csv
                } : source_contents
  in
    next
  end

overload + with add_contents

// This is the step function used when streaming directory contents. 
fn adjust_contents(sc_r : &source_contents >> source_contents, scf : pl_type) : void =
  let
    val () = case+ scf of
      | ~haskell n => sc_r.haskell := sc_r.haskell + n
      | ~ats n => sc_r.ats := sc_r.ats + n
      | ~rust n => sc_r.rust := sc_r.rust + n
      | ~markdown n => sc_r.markdown := sc_r.markdown + n
      | ~python n => sc_r.python := sc_r.python + n
      | ~vimscript n => sc_r.vimscript := sc_r.vimscript + n
      | ~yaml n => sc_r.yaml := sc_r.yaml + n
      | ~toml n => sc_r.toml := sc_r.toml + n
      | ~happy n => sc_r.happy := sc_r.happy + n
      | ~alex n => sc_r.alex := sc_r.alex + n
      | ~idris n => sc_r.idris := sc_r.idris + n
      | ~madlang n => sc_r.madlang := sc_r.madlang + n
      | ~elm n => sc_r.elm := sc_r.elm + n
      | ~c n => sc_r.c := sc_r.c + n
      | ~go n => sc_r.go := sc_r.go + n
      | ~cabal n => sc_r.cabal := sc_r.cabal + n
      | ~verilog n => sc_r.verilog := sc_r.verilog + n
      | ~vhdl n => sc_r.vhdl := sc_r.vhdl + n
      | ~html n => sc_r.html := sc_r.html + n
      | ~css n => sc_r.css := sc_r.css + n
      | ~purescript n => sc_r.purescript := sc_r.purescript + n
      | ~futhark n => sc_r.futhark := sc_r.futhark + n
      | ~brainfuck n => sc_r.brainfuck := sc_r.brainfuck + n
      | ~ruby n => sc_r.ruby := sc_r.ruby + n
      | ~julia n => sc_r.julia := sc_r.julia + n
      | ~tex n => sc_r.tex := sc_r.tex + n
      | ~perl n => sc_r.perl := sc_r.perl + n
      | ~sml n => sc_r.sml := sc_r.sml + n
      | ~ocaml n => sc_r.ocaml := sc_r.ocaml + n
      | ~agda n => sc_r.agda := sc_r.agda + n
      | ~cobol n => sc_r.cobol := sc_r.cobol + n
      | ~tcl n => sc_r.tcl := sc_r.tcl + n
      | ~r n => sc_r.r := sc_r.r + n
      | ~lua n => sc_r.lua := sc_r.lua + n
      | ~cpp n => sc_r.cpp := sc_r.cpp + n
      | ~lalrpop n => sc_r.lalrpop := sc_r.lalrpop + n
      | ~header n => sc_r.header := sc_r.header + n
      | ~sixten n => sc_r.sixten := sc_r.sixten + n
      | ~dhall n => sc_r.dhall := sc_r.dhall + n
      | ~ipkg n => sc_r.ipkg := sc_r.ipkg + n
      | ~justfile n => sc_r.justfile := sc_r.justfile + n
      | ~makefile n => sc_r.makefile := sc_r.makefile + n
      | ~ion n => sc_r.ion := sc_r.ion + n
      | ~bash n => sc_r.bash := sc_r.bash + n
      | ~dash n => sc_r.dash := sc_r.dash + n
      | ~hamlet n => sc_r.hamlet := sc_r.hamlet + n
      | ~cassius n => sc_r.cassius := sc_r.cassius + n
      | ~lucius n => sc_r.lucius := sc_r.lucius + n
      | ~julius n => sc_r.julius := sc_r.julius + n
      | ~mercury n => sc_r.mercury := sc_r.mercury + n
      | ~yacc n => sc_r.yacc := sc_r.yacc + n
      | ~lex n => sc_r.lex := sc_r.lex + n
      | ~coq n => sc_r.coq := sc_r.coq + n
      | ~jupyter n => sc_r.jupyter := sc_r.jupyter + n
      | ~java n => sc_r.java := sc_r.java + n
      | ~scala n => sc_r.scala := sc_r.scala + n
      | ~erlang n => sc_r.erlang := sc_r.erlang + n
      | ~elixir n => sc_r.elixir := sc_r.elixir + n
      | ~pony n => sc_r.pony := sc_r.pony + n
      | ~clojure n => sc_r.clojure := sc_r.clojure + n
      | ~cabal_project n => sc_r.cabal_project := sc_r.cabal_project + n
      | ~assembly n => sc_r.assembly := sc_r.assembly + n
      | ~nix n => sc_r.nix := sc_r.nix + n
      | ~php n => sc_r.php := sc_r.php + n
      | ~javascript n => sc_r.javascript := sc_r.javascript + n
      | ~kotlin n => sc_r.kotlin := sc_r.kotlin + n
      | ~fsharp n => sc_r.fsharp := sc_r.fsharp + n
      | ~fortran n => sc_r.fortran := sc_r.fortran + n
      | ~swift n => sc_r.swift := sc_r.swift + n
      | ~csharp n => sc_r.csharp := sc_r.csharp + n
      | ~nim n => sc_r.nim := sc_r.nim + n
      | ~cpp_header n => sc_r.cpp_header := sc_r.cpp_header + n
      | ~elisp n => sc_r.elisp := sc_r.elisp + n
      | ~plaintext n => sc_r.plaintext := sc_r.plaintext + n
      | ~rakefile n => sc_r.rakefile := sc_r.rakefile + n
      | ~llvm n => sc_r.llvm := sc_r.llvm + n
      | ~autoconf n => sc_r.autoconf := sc_r.autoconf + n
      | ~batch n => sc_r.batch := sc_r.batch + n
      | ~powershell n => sc_r.powershell := sc_r.powershell + n
      | ~m4 n => sc_r.m4 := sc_r.m4 + n
      | ~objective_c n => sc_r.objective_c := sc_r.objective_c + n
      | ~automake n => sc_r.automake := sc_r.automake + n
      | ~carp n => sc_r.carp := sc_r.carp + n
      | ~shen n => sc_r.shen := sc_r.shen + n
      | ~greencard n => sc_r.greencard := sc_r.greencard + n
      | ~cmm n => sc_r.cmm := sc_r.cmm + n
      | ~fluid n => sc_r.fluid := sc_r.fluid + n
      | ~plutus n => sc_r.plutus := sc_r.plutus + n
      | ~j n => sc_r.j := sc_r.j + n
      | ~blodwen n => sc_r.blodwen := sc_r.blodwen + n
      | ~crystal n => sc_r.crystal := sc_r.crystal + n
      | ~racket n => sc_r.racket := sc_r.racket + n
      | ~ada n => sc_r.ada := sc_r.ada + n
      | ~isabelle n => sc_r.isabelle := sc_r.isabelle + n
      | ~fstar n => sc_r.fstar := sc_r.fstar + n
      | ~d n => sc_r.d := sc_r.d + n
      | ~factor n => sc_r.factor := sc_r.factor + n
      | ~scheme n => sc_r.scheme := sc_r.scheme + n
      | ~chapel n => sc_r.chapel := sc_r.chapel + n
      | ~pascal n => sc_r.pascal := sc_r.pascal + n
      | ~ragel n => sc_r.ragel := sc_r.ragel + n
      | ~xml n => sc_r.xml := sc_r.xml + n
      | ~awk n => sc_r.awk := sc_r.awk + n
      | ~sed n => sc_r.sed := sc_r.sed + n
      | ~k n => sc_r.k := sc_r.k + n
      | ~typescript n => sc_r.typescript := sc_r.typescript + n
      | ~coffeescript n => sc_r.coffeescript := sc_r.coffeescript + n
      | ~red n => sc_r.red := sc_r.red + n
      | ~fish n => sc_r.fish := sc_r.fish + n
      | ~vb n => sc_r.vb := sc_r.vb + n
      | ~frege n => sc_r.frege := sc_r.frege + n
      | ~dart n => sc_r.dart := sc_r.dart + n
      | ~solidity n => sc_r.solidity := sc_r.solidity + n
      | ~egison n => sc_r.egison := sc_r.egison + n
      | ~zig n => sc_r.zig := sc_r.zig + n
      | ~sql n => sc_r.sql := sc_r.sql + n
      | ~felix n => sc_r.felix := sc_r.felix + n
      | ~qsharp n => sc_r.qsharp := sc_r.qsharp + n
      | ~oz n => sc_r.oz := sc_r.oz + n
      | ~jai n => sc_r.jai := sc_r.jai + n
      | ~zimpl n => sc_r.zimpl := sc_r.zimpl + n
      | ~volt n => sc_r.volt := sc_r.volt + n
      | ~cogent n => sc_r.cogent := sc_r.cogent + n
      | ~clean n => sc_r.clean := sc_r.clean + n
      | ~thrift n => sc_r.thrift := sc_r.thrift + n
      | ~vala n => sc_r.vala := sc_r.vala + n
      | ~apex n => sc_r.apex := sc_r.apex + n
      | ~sas n => sc_r.sas := sc_r.sas + n
      | ~nu n => sc_r.nu := sc_r.nu + n
      | ~haxe n => sc_r.haxe := sc_r.haxe + n
      | ~eiffel n => sc_r.eiffel := sc_r.eiffel + n
      | ~tla n => sc_r.tla := sc_r.tla + n
      | ~lean n => sc_r.lean := sc_r.lean + n
      | ~io n => sc_r.io := sc_r.io + n
      | ~squirrel n => sc_r.squirrel := sc_r.squirrel + n
      | ~agdalib n => sc_r.agdalib := sc_r.agdalib + n
      | ~cedille n => sc_r.cedille := sc_r.cedille + n
      | ~raml n => sc_r.raml := sc_r.raml + n
      | ~scribble n => sc_r.scribble := sc_r.scribble + n
      | ~bibtex n => sc_r.bibtex := sc_r.bibtex + n
      | ~csv n => sc_r.csv := sc_r.csv + n
      | ~unknown _ => ()
  in
    ()
  end

fun match_keywords { m : nat | m <= 10 }(keys : list(string, m), word : string) : bool =
  list_foldright_cloref(keys, lam (next, acc) =<cloref1> acc || eq_string_string(next, word), false)

// helper function for check_keywords
fn step_keyword(size : file, pre : pl_type, word : string, ext : string) : pl_type =
  case+ pre of
    | unknown _ => 
      begin
        case+ ext of
          | "y" => let
            val _ = free(pre)
            var happy_keywords = "module" :: "import" :: nil
          in
            ifcase
              | match_keywords(happy_keywords, word) => happy(size)
              | let
                var yacc_keywords = "struct" :: "char" :: "int" :: nil
              in
                match_keywords(yacc_keywords, word)
              end => yacc(size)
              | _ => unknown
          end
          | "v" => let
            var _ = free(pre)
            var verilog_keywords = "endmodule" :: "posedge" :: "edge" :: "always" :: "wire" :: nil
          in
            ifcase
              | match_keywords(verilog_keywords, word) => verilog(size)
              | let
                var coq_keywords = "Qed"
                :: "Require"
                :: "Hypothesis"
                :: "Inductive" :: "Remark" :: "Lemma" :: "Proof" :: "Definition" :: "Theorem" :: nil
              in
                match_keywords(coq_keywords, word)
              end => coq(size)
              | _ => unknown
          end
          | "m" => let
            val _ = free(pre)
            var mercury_keywords = "module" :: "pred" :: nil
          in
            ifcase
              | match_keywords(mercury_keywords, word) => mercury(size)
              | let
                var objective_c_keywords = "unsigned" :: "nil" :: "nullable" :: "nonnull" :: nil
              in
                match_keywords(objective_c_keywords, word)
              end => objective_c(size)
              | _ => unknown
          end
          | _ => pre
      end
    | _ => pre

// Function to disambiguate extensions such as .v (Coq and Verilog) and .m
// (Mercury and Objective C). This should only be called when extensions are in
// conflict, as it reads the whole file.
fn check_keywords(s : string, size : file, ext : string) : pl_type =
  let
    var ref = fileref_open_opt(s, file_mode_r)
  in
    case+ ref of
      | ~Some_vt (x) => let
        var init: pl_type = unknown
        var viewstream = $EXTRA.streamize_fileref_word(x)
        var result = stream_vt_foldleft_cloptr(viewstream, init, lam (acc, next) => step_keyword(size, acc, next, ext))
        val _ = fileref_close(x)
      in
        result
      end
      | ~None_vt() => (bad_file(s) ; unknown)
  end

fn freadc {l:addr}(pf : !bytes_v(l, BUFSZ) | inp : !FILEptr1, p : ptr(l), c : char) : size_t =
  let
    extern
    castfn as_fileref(x : !FILEptr1) :<> FILEref
    
    var n = $extfcall(size_t, "fread", p, sizeof<char>, BUFSZ - 1, as_fileref(inp))
    val () = $UN.ptr0_set<char>(ptr_add<char>(p, n), c)
  in
    n
  end

// Check shebang on scripts.
//
// TODO flexible parser that drops spaces as appropriate
// TODO check magic number so as to avoid checking shebang of binary file
// FIXME: instead of using fileref_get_line_string, read into a buffer!
// 26 chars?
fn check_shebang(s : string) : pl_type =
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
      | "#!/usr/bin/env sh" => dash(line_count(s, Some_vt("#")))
      | "#!/bin/bash" => bash(line_count(s, Some_vt("#")))
      | "#!/bin/sh" => dash(line_count(s, Some_vt("#")))
      | "#!/bin/sed" => sed(line_count(s, Some_vt("#")))
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
      | "#!/usr/bin/env fish" => fish(line_count(s, Some_vt("#")))
      | "#!/usr/bin/Rscript" => r(line_count(s, None_vt))
      | _ => unknown
  end

// Match based on filename (for makefiles, etc.)
fn match_filename(s : string) : pl_type =
  let
    val (prf | str) = filename_get_base(s)
    var match = $UN.strptr2string(str)
    prval () = prf(str)
  in
    case+ match of
      | "Makefile" => makefile(line_count(s, Some_vt("#")))
      | "Makefile.tc" => makefile(line_count(s, Some_vt("#")))
      | "Makefile.common" => makefile(line_count(s, Some_vt("#")))
      | "Makefile.common_c" => makefile(line_count(s, Some_vt("#")))
      | "makefile" => makefile(line_count(s, Some_vt("#")))
      | "GNUmakefile" => makefile(line_count(s, Some_vt("#")))
      | "Justfile" => justfile(line_count(s, Some_vt("#")))
      | "justfile" => justfile(line_count(s, Some_vt("#")))
      | "Rakefile" => rakefile(line_count(s, None_vt))
      | "cabal.project.local" => cabal_project(line_count(s, Some_vt("--")))
      | "Nukefile" => nu(line_count(s, Some_vt(";")))
      | _ => check_shebang(s)
  end

// Match based on file extension (assuming the file name is passed in as an
// argument).
fn prune_extension(s : string, file_proper : string) : pl_type =
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
      | "cpphs" => haskell(line_count(s, Some_vt("--")))
      | "hsc" => haskell(line_count(s, Some_vt("--")))
      | "chs" => haskell(line_count(s, Some_vt("--")))
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
      | "cats" => c(line_count(s, Some_vt("//")))
      | "sats" => ats(line_count(s, Some_vt("//")))
      | "py" => python(line_count(s, Some_vt("#")))
      | "fut" => futhark(line_count(s, Some_vt("--")))
      | "pl" => perl(line_count(s, None_vt))
      | "agda" => agda(line_count(s, Some_vt("--")))
      | "idr" => idris(line_count(s, Some_vt("--")))
      | "blod" => blodwen(line_count(s, Some_vt("--")))
      | "v" => check_keywords(s, line_count(s, Some_vt("--")), match)
      | "m" => check_keywords(s, line_count(s, None_vt), match)
      | "vhdl" => vhdl(line_count(s, None_vt))
      | "vhd" => vhdl(line_count(s, None_vt))
      | "go" => go(line_count(s, Some_vt("//")))
      | "vim" => vimscript(line_count(s, Some_vt("\"")))
      | "sml" => sml(line_count(s, None_vt))
      | "sig" => sml(line_count(s, None_vt))
      | "fun" => sml(line_count(s, None_vt))
      | "ml" => ocaml(line_count(s, None_vt))
      | "mli" => ocaml(line_count(s, None_vt))
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
      | "lx" => alex(line_count(s, Some_vt("--")))
      | "l" => lex(line_count(s, None_vt))
      | "lpp" => lex(line_count(s, None_vt))
      | "html" => html(line_count(s, None_vt))
      | "htm" => html(line_count(s, None_vt))
      | "css" => css(line_count(s, None_vt))
      | "vhdl" => vhdl(line_count(s, None_vt))
      | "vhd" => vhdl(line_count(s, None_vt))
      | "c" => c(line_count(s, Some_vt("//")))
      | "C" => cpp(line_count(s, Some_vt("//")))
      | "cmm" => cmm(line_count(s, Some_vt("//")))
      | "b" => brainfuck(line_count(s, None_vt))
      | "bf" => brainfuck(line_count(s, None_vt))
      | "rb" => ruby(line_count(s, None_vt))
      | "cob" => cobol(line_count(s, Some_vt("*>")))
      | "cbl" => cobol(line_count(s, Some_vt("*>")))
      | "cpy" => cobol(line_count(s, Some_vt("*>")))
      | "tcl" => tcl(line_count(s, Some_vt("#")))
      | "fl" => fluid(line_count(s, Some_vt("#")))
      | "r" => r(line_count(s, None_vt))
      | "R" => r(line_count(s, None_vt))
      | "lua" => lua(line_count(s, Some_vt("--")))
      | "cpp" => cpp(line_count(s, Some_vt("//")))
      | "ino" => cpp(line_count(s, Some_vt("//")))
      | "cc" => cpp(line_count(s, Some_vt("//")))
      | "lalrpop" => lalrpop(line_count(s, Some_vt("//")))
      | "h" => header(line_count(s, None_vt))
      | "vix" => sixten(line_count(s, Some_vt("--")))
      | "dhall" => dhall(line_count(s, Some_vt("--")))
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
      | "java" => java(line_count(s, Some_vt("//")))
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
      | "js" => javascript(line_count(s, Some_vt("//")))
      | "jsexe" => javascript(line_count(s, Some_vt("//")))
      | "kt" => kotlin(line_count(s, None_vt))
      | "kts" => kotlin(line_count(s, None_vt))
      | "fs" => fsharp(line_count(s, None_vt))
      | "f" => fortran(line_count(s, Some_vt("*")))
      | "F" => fortran(line_count(s, Some_vt("*")))
      | "for" => fortran(line_count(s, Some_vt("*")))
      | "f90" => fortran(line_count(s, Some_vt("*")))
      | "f95" => fortran(line_count(s, Some_vt("*")))
      | "swift" => swift(line_count(s, None_vt))
      | "csharp" => csharp(line_count(s, Some_vt("//")))
      | "cs" => csharp(line_count(s, Some_vt("//")))
      | "nim" => nim(line_count(s, None_vt))
      | "el" => elisp(line_count(s, Some_vt(";")))
      | "txt" => plaintext(line_count(s, None_vt))
      | "ll" => llvm(line_count(s, None_vt))
      | "in" => autoconf(line_count(s, Some_vt("#")))
      | "bat" => batch(line_count(s, None_vt))
      | "ps1" => powershell(line_count(s, None_vt))
      | "ac" => m4(line_count(s, None_vt))
      | "mm" => objective_c(line_count(s, Some_vt("//")))
      | "am" => automake(line_count(s, Some_vt("#")))
      | "carp" => carp(line_count(s, Some_vt(";")))
      | "pls" => plutus(line_count(s, None_vt))
      | "ijs" => j(line_count(s, Some_vt("NB")))
      | "cr" => crystal(line_count(s, Some_vt("#")))
      | "rkt" => racket(line_count(s, Some_vt("#")))
      | "adb" => ada(line_count(s, Some_vt("--")))
      | "ads" => ada(line_count(s, Some_vt("--")))
      | "thy" => isabelle(line_count(s, None_vt))
      | "fst" => fstar(line_count(s, Some_vt("//")))
      | "d" => d(line_count(s, Some_vt("//")))
      | "factor" => factor(line_count(s, Some_vt("!")))
      | "scm" => scheme(line_count(s, Some_vt(";")))
      | "ss" => scheme(line_count(s, Some_vt(";")))
      | "chpl" => chapel(line_count(s, Some_vt("//")))
      | "pas" => pascal(line_count(s, Some_vt("//")))
      | "rl" => ragel(line_count(s, None_vt))
      | "xml" => xml(line_count(s, None_vt))
      | "awk" => awk(line_count(s, Some_vt("#")))
      | "sed" => sed(line_count(s, Some_vt("#")))
      | "k" => k(line_count(s, Some_vt("/")))
      | "ts" => typescript(line_count(s, Some_vt("//")))
      | "coffee" => coffeescript(line_count(s, Some_vt("//")))
      | "red" => red(line_count(s, Some_vt(";")))
      | "vb" => vb(line_count(s, Some_vt("'")))
      | "frege" => frege(line_count(s, Some_vt("--")))
      | "dart" => dart(line_count(s, Some_vt("//")))
      | "sol" => solidity(line_count(s, Some_vt("//")))
      | "egi" => egison(line_count(s, Some_vt(";")))
      | "zig" => zig(line_count(s, Some_vt("//")))
      | "sql" => sql(line_count(s, Some_vt("--")))
      | "flx" => felix(line_count(s, Some_vt("//")))
      | "qs" => qsharp(line_count(s, Some_vt("//")))
      | "fish" => fish(line_count(s, Some_vt("#")))
      | "oz" => oz(line_count(s, Some_vt("%")))
      | "jai" => jai(line_count(s, Some_vt("//")))
      | "zpl" => zimpl(line_count(s, Some_vt("#")))
      | "volt" => volt(line_count(s, Some_vt("//")))
      | "cogent" => cogent(line_count(s, Some_vt("--")))
      | "icl" => clean(line_count(s, Some_vt("//")))
      | "dcl" => clean(line_count(s, Some_vt("//")))
      | "thrift" => thrift(line_count(s, Some_vt("//")))
      | "vala" => vala(line_count(s, Some_vt("//")))
      | "cls" => apex(line_count(s, Some_vt("//")))
      | "sas" => sas(line_count(s, Some_vt("%*")))
      | "nu" => nu(line_count(s, Some_vt(";")))
      | "hx" => haxe(line_count(s, Some_vt("//")))
      | "e" => eiffel(line_count(s, Some_vt("--")))
      | "tla" => tla(line_count(s, Some_vt("\\*")))
      | "lean" => lean(line_count(s, Some_vt("--")))
      | "io" => io(line_count(s, Some_vt("#")))
      | "nut" => squirrel(line_count(s, Some_vt("//")))
      | "agda-lib" => agda(line_count(s, None_vt))
      | "ced" => cedille(line_count(s, Some_vt("--")))
      | "raml" => raml(line_count(s, None_vt()))
      | "scrbl" => scribble(line_count(s, Some_vt(";")))
      | "bib" => bibtex(line_count(s, None_vt()))
      | "csv" => csv(line_count(s, None_vt()))
      | "" => match_filename(s)
      | "sh" => match_filename(s)
      | "yamllint" => match_filename(s)
      | _ => unknown
  end

// filter out directories containing artifacts
fn bad_dir(s : string, excludes : List0(string)) : bool =
  case+ s of
    | "." => true
    | ".." => true
    | ".pijul" => true
    | "_darcs" => true
    | ".git" => true
    | ".hg" => true
    | "build" => true
    | "target" => true
    | ".atspkg" => true
    | "nimcache" => true
    | "dist-newstyle" => true
    | "dist" => true
    | ".psc-package" => true
    | ".pulp-cache" => true
    | "bower_components" => true
    | "elm-stuff" => true
    | ".stack-work" => true
    | ".cabal-sandbox" => true
    | "node_modules" => true
    | ".egg-info" => true
    | ".lein-plugins" => true
    | ".sass-cache" => true
    | ".pyre" => true
    | _ => let
      val s0 = s + "/"
    in
      list_exists_cloref(excludes, lam x => x = s || x = s0)
    end

fnx step_stream( acc : &source_contents >> source_contents
               , full_name : string
               , file_proper : string
               , excludes : List0(string)
               , verbose : bool
               ) : void =
  if test_file_isdir(full_name) > 0 then
    flow_stream(full_name, acc, excludes, verbose)
  else
    let
      val ft = prune_extension(full_name, file_proper)
      val () = if verbose then
        print!(print_file(ft, full_name))
      else
        ()
      val () = adjust_contents(acc, ft)
    in end
and flow_stream(s : string, init : &source_contents >> source_contents, excludes : List0(string), verbose : bool) :
  void =
  let
    var files = $EXTRA.streamize_dirname_fname(s)
    var ffiles = stream_vt_filter_cloptr(files, lam x => not(bad_dir(x, excludes)))
  in
    if s != "." then
      let
        fun loop( ffiles : stream_vt(string)
                , init : &source_contents >> source_contents
                , f : (&source_contents >> source_contents, string, string, List0(string), bool) -> void
                ) : void =
          case+ !ffiles of
            | ~stream_vt_cons (next, nexts) => {
              val () = f(init, s + "/" + next, next, excludes, verbose)
              val () = loop(nexts, init, f)
            }
            | ~stream_vt_nil() => ()
      in
        loop(ffiles, init, step_stream)
      end
    else
      let
        fun loop( ffiles : stream_vt(string)
                , init : &source_contents >> source_contents
                , f : (&source_contents >> source_contents, string, string, List0(string), bool) -> void
                ) : void =
          case+ !ffiles of
            | ~stream_vt_cons (next, nexts) => {
              val () = f(init, next, next, excludes, verbose)
              val () = loop(nexts, init, f)
            }
            | ~stream_vt_nil() => ()
      in
        loop(ffiles, init, step_stream)
      end
  end

fn empty_contents() : source_contents =
  let
    var isc = @{ rust = empty_file
               , haskell = empty_file
               , ats = empty_file
               , python = empty_file
               , vimscript = empty_file
               , elm = empty_file
               , idris = empty_file
               , madlang = empty_file
               , tex = empty_file
               , markdown = empty_file
               , yaml = empty_file
               , toml = empty_file
               , cabal = empty_file
               , happy = empty_file
               , alex = empty_file
               , go = empty_file
               , html = empty_file
               , css = empty_file
               , verilog = empty_file
               , vhdl = empty_file
               , c = empty_file
               , purescript = empty_file
               , futhark = empty_file
               , brainfuck = empty_file
               , ruby = empty_file
               , julia = empty_file
               , perl = empty_file
               , sml = empty_file
               , ocaml = empty_file
               , agda = empty_file
               , cobol = empty_file
               , tcl = empty_file
               , r = empty_file
               , lua = empty_file
               , cpp = empty_file
               , lalrpop = empty_file
               , header = empty_file
               , sixten = empty_file
               , dhall = empty_file
               , ipkg = empty_file
               , makefile = empty_file
               , justfile = empty_file
               , ion = empty_file
               , bash = empty_file
               , dash = empty_file
               , hamlet = empty_file
               , cassius = empty_file
               , lucius = empty_file
               , julius = empty_file
               , mercury = empty_file
               , yacc = empty_file
               , lex = empty_file
               , coq = empty_file
               , jupyter = empty_file
               , java = empty_file
               , scala = empty_file
               , erlang = empty_file
               , elixir = empty_file
               , pony = empty_file
               , clojure = empty_file
               , cabal_project = empty_file
               , assembly = empty_file
               , nix = empty_file
               , php = empty_file
               , javascript = empty_file
               , kotlin = empty_file
               , fsharp = empty_file
               , fortran = empty_file
               , swift = empty_file
               , csharp = empty_file
               , nim = empty_file
               , cpp_header = empty_file
               , elisp = empty_file
               , plaintext = empty_file
               , rakefile = empty_file
               , llvm = empty_file
               , autoconf = empty_file
               , batch = empty_file
               , powershell = empty_file
               , m4 = empty_file
               , objective_c = empty_file
               , automake = empty_file
               , carp = empty_file
               , shen = empty_file
               , greencard = empty_file
               , cmm = empty_file
               , fluid = empty_file
               , plutus = empty_file
               , j = empty_file
               , blodwen = empty_file
               , crystal = empty_file
               , racket = empty_file
               , ada = empty_file
               , isabelle = empty_file
               , fstar = empty_file
               , d = empty_file
               , factor = empty_file
               , scheme = empty_file
               , chapel = empty_file
               , pascal = empty_file
               , ragel = empty_file
               , xml = empty_file
               , awk = empty_file
               , sed = empty_file
               , k = empty_file
               , typescript = empty_file
               , coffeescript = empty_file
               , red = empty_file
               , fish = empty_file
               , vb = empty_file
               , frege = empty_file
               , dart = empty_file
               , solidity = empty_file
               , egison = empty_file
               , zig = empty_file
               , sql = empty_file
               , felix = empty_file
               , qsharp = empty_file
               , oz = empty_file
               , jai = empty_file
               , zimpl = empty_file
               , volt = empty_file
               , cogent = empty_file
               , clean = empty_file
               , thrift = empty_file
               , vala = empty_file
               , apex = empty_file
               , sas = empty_file
               , nu = empty_file
               , haxe = empty_file
               , eiffel = empty_file
               , tla = empty_file
               , lean = empty_file
               , io = empty_file
               , squirrel = empty_file
               , agdalib = empty_file
               , cedille = empty_file
               , raml = empty_file
               , scribble = empty_file
               , bibtex = empty_file
               , csv = empty_file
               } : source_contents
  in
    isc
  end

fn map_stream( acc : &source_contents >> source_contents
             , includes : List0(string)
             , excludes : List0(string)
             , verbose : bool
             ) : void =
  let
    fun loop(includes : List0(string), acc : &source_contents >> source_contents) : void =
      case+ includes of
        | list_cons (next, nexts) => if test_file_exists(next) || test_file_isdir(next) < 0 || next = "" then
          (step_stream(acc, next, next, excludes, verbose) ; loop(nexts, acc))
        else
          maybe_err(next)
        | list_nil() => ()
  in
    loop(includes, acc)
  end

fn step_list(s : string, excludes : List0(string)) : List0(string) =
  let
    var files = $EXTRA.streamize_dirname_fname(s)
    var ffiles = stream_vt_filter_cloptr(files, lam x => not(bad_dir(x, excludes) && test_file_isdir(s + "/" + x) > 0))
    
    fun stream2list(x : stream_vt(string)) : List0(string) =
      case+ !x of
        | ~stream_vt_cons (x, xs) => list_cons(s + "/" + x, stream2list(xs))
        | ~stream_vt_nil() => list_nil
  in
    stream2list(ffiles)
  end

fn step_list_files(s : string, excludes : List0(string)) : List0(string) =
  let
    var files = $EXTRA.streamize_dirname_fname(s)
    var ffiles = stream_vt_filter_cloptr(files, lam x => not(bad_dir(x, excludes)) && test_file_isdir(s + "/" + x) = 0)
    
    fun stream2list(x : stream_vt(string)) : List0(string) =
      case+ !x of
        | ~stream_vt_cons (x, xs) when s = "." => list_cons(x, stream2list(xs))
        | ~stream_vt_cons (x, xs) => list_cons(s + "/" + x, stream2list(xs))
        | ~stream_vt_nil() => list_nil
  in
    stream2list(ffiles)
  end

fn map_depth(xs : List0(string), excludes : List0(string)) : List0(string) =
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
