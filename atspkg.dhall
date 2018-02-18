let prelude = https://raw.githubusercontent.com/vmchale/atspkg/master/dhall/atspkg-prelude.dhall

in prelude.default //
  { bin =
    [
      prelude.bin //
      { src = "src/polyglot.dats"
      , target = "target/poly"
      , gcBin = True
      , libs = [ "pthread" ]
      }
    ]
  , test =
    [ prelude.bin //
      { src = "test/test.dats"
      , target = "target/test"
      , gcBin = True
      }
    ]
  , man = ([ "man/poly.md" ] : Optional Text)
  , compiler = [0,3,8]
  , dependencies = prelude.mapPlainDeps [ "concurrency", "specats", "nproc-ats" ]
  , cflags = [ "-flto", "-O2", "-mtune=native" ]
  }
