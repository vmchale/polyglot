let pkg = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/default.dhall

in pkg //
  { bin =
    [
      { src = "src/compat.dats"
      , target = "target/poly"
      , libs = ([] : List Text)
      , gc = True
      }
    ]
  , test =
    [
      { src = "test/test.dats"
      , target = "target/test"
      , libs = ([] : List Text)
      , gc = True
      }
    ]
  , man = ([ "man/poly.md" ] : Optional Text)
  , compiler = [0,3,8]
  , version = [0,3,9]
  , dependencies = [ https://raw.githubusercontent.com/vmchale/ats-concurrency/master/atspkg.dhall ]
  , cflags = [ "-flto", "-O2", "-mtune=native" ]
  }
