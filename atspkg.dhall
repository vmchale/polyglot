let pkg
  = { bin = 
      [
        { src = "src/polyglot.dats"
        , target = "target/poly" 
        , libs = [ "pthread" ]
        , gc = False
        }
      ]
    , test =
      [
        { src = "test/test.dats"
        , target = "target/test"
        , libs = ([] : List Text)
        , gc = False
        }
      ]
    , man = ([ "man/poly.md" ] : Optional Text)
    , version = [0,3,9]
    }

in pkg
