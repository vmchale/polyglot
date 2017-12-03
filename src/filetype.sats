// Type for a collection of files (monoidal)
typedef file = @{ lines = int
                , files = int
                }

// Type for the parsed command-line arguments. 
typedef command_line = @{ version = bool
                        , help = bool
                        , table = bool
                        , excludes = [ m: nat ] list(string, m)
                        , includes = [ m: nat ] list(string, m)
                        }

// Program state, tracking *all* supported file types in an unboxed structure.
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
                           , mercury = file
                           , yacc = file
                           , lex = file
                           , coq = file
                           , jupyter = file
                           , java = file
                           , scala = file
                           , erlang = file
                           , elixir = file
                           , pony = file
                           , clojure = file
                           }

// Reference to source_contents; used to update the structure.
typedef source_contents_r = ref(source_contents)

// Sum type representing all supported data types.
datavtype pl_type =
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
  | mercury of int
  | yacc of int
  | lex of int
  | coq of int
  | jupyter of int
  | java of int
  | scala of int
  | erlang of int
  | elixir of int
  | pony of int
  | clojure of int
