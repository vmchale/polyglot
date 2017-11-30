typedef file = @{ lines = int
                , files = int
                }

typedef command_line = @{ version = bool
                        , help = bool
                        , excludes = [ m: int | m <= 40 ] list(string, m)
                        }

typedef source_contents = @{ rust = file
                           , haskell = file
                           , ats = file
                           , python = file
                           , vimscript = file
                           , elm = file
                           , idris = file
                           , madlang = file
                           , tex = file
                           , markdown = file
                           , yaml = file
                           , toml = file
                           , cabal = file
                           , happy = file
                           , alex = file
                           , go = file
                           , html = file
                           , css = file
                           , verilog = file
                           , vhdl = file
                           , c = file
                           , purescript = file
                           , futhark = file
                           , brainfuck = file
                           , ruby = file
                           , julia = file
                           , perl = file
                           , ocaml = file
                           , agda = file
                           , cobol = file
                           , tcl = file
                           , r = file
                           , lua = file
                           , cpp = file
                           , lalrpop = file
                           , header = file
                           , sixten = file
                           , dhall = file
                           , ipkg = file
                           , makefile = file
                           , justfile = file
                           , ion = file
                           , bash = file
                           , hamlet = file
                           , cassius = file
                           , lucius = file
                           , julius = file
                           }

typedef source_contents_r = ref(source_contents)

datatype pl_type =
  | unknown
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
  | go of int
  | html of int
  | css of int
  | c of int
  | brainfuck of int
  | ruby of int
  | julia of int
  | cobol of int
  | tcl of int
  | r of int
  | lua of int
  | cpp of int
  | lalrpop of int
  | header of int
  | sixten of int
  | dhall of int
  | ipkg of int
  | makefile of int
  | justfile of int
  | ion of int
  | bash of int
  | hamlet of int
  | cassius of int
  | lucius of int
  | julius of int
