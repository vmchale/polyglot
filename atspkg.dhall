let pkg : { bin : List { src : Text, target : Text, libs : List Text }, test : List { src : Text, target : Text, libs : List Text }, man : Optional Text }
  = { bin = 
      [
        { src = "src/polyglot.dats"
        , target = "target/poly" 
        , libs = [ "pthread" ]
        }
      ]
    , test =
      [
        { src = "test/test.dats"
        , target = "target/test"
        , libs = ([] : List Text)
        }
      ]
    , man = ([ "man/poly.md" ] : Optional Text)
    }

in pkg
