let pkg = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/default.dhall

in pkg //
  { bin =
    [
      { src = "src/polyglot.dats"
      , target = "target/poly"
      , libs = [ "pthread" ]
      , gc = False
      }
    ,
      { src = "src/compat.dats"
      , target = "target/poly-compat"
      , libs = ([] : List Text)
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
  , dependencies = [ https://raw.githubusercontent.com/vmchale/ats-concurrency/master/atspkg.dhall ]
  }
