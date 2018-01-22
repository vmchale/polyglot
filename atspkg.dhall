let pkg : { bin : List { src : Text, target : Text, libs : List Text, gc : Bool }, test : List { src : Text, target : Text, libs : List Text, gc : Bool }, man : Optional Text }
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
        , libs = [ "gc" ]
        , gc = True
        }
      ]
    , man = ([ "man/poly.md" ] : Optional Text)
    }

in pkg
