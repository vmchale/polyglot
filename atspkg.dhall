let pkg = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/default.dhall
in
let dbin = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/default-bin.dhall

in pkg //
  { bin =
    [
      dbin //
      { src = "src/polyglot.dats"
      , target = "target/poly"
      , gcBin = True
      , libs = [ "pthread" ]
      }
    ]
  , test =
    [ dbin //
      { src = "test/test.dats"
      , target = "target/test"
      , gcBin = True
      }
    ]
  , man = ([ "man/poly.md" ] : Optional Text)
  , compiler = [0,3,8]
  , dependencies = [ "concurrency" ]
  , cflags = [ "-flto", "-O2", "-mtune=native" ]

  }
