let pkg : { bin : List { src : Text, target : Text, libs : List Text, gc : Bool }, test : List { src : Text, target : Text, libs : List Text, gc : Bool }, man : Optional Text, version : List Integer }
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
    , version = [0,3,8]
    }

in pkg
