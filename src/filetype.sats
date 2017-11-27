typedef source_contents = @{ // programming languages
                             rust = int
                           , haskell = int
                           , ats = int
                           , python = int
                           , vimscript = int
                           , elm = int
                           , idris = int
                           , madlang = int
                           // documentation
                           , tex = int
                           , markdown = int
                           // configuration
                           , yaml = int
                           , toml = int
                           , cabal = int
                           // parser generators
                           , happy = int
                           , alex = int
                           }

datatype pl_type =
  | rust of int
  | haskell of int
  | perl of int
  | verilog of int
  | vhdl of int
  | agda of int
  | futhark of int
  | ats of int
  | idris of int
  | python of int
  | go of int
  | elm of int
  | purescript of int
  | vimscript of int
  | ocaml of int
  | madlang of int
  | tex of int
  | markdown of int
  | yaml of int
  | toml of int
  | cabal of int
  | happy of int
  | alex of int
