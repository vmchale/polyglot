let pkg = https://raw.githubusercontent.com/vmchale/atspkg/master/atspkg.dhall

in pkg //
  { bin =
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
  , compiler = [0,3,8]
  }
