let prelude = https://raw.githubusercontent.com/vmchale/atspkg/master/ats-pkg/dhall/atspkg-prelude.dhall
in

let not = https://ipfs.io/ipfs/QmdtKd5Q7tebdo6rXfZed4kN6DXmErRQHJ4PsNCtca9GbB/Prelude/Bool/not
in

let cross = True
in

prelude.default ⫽ 
  { bin =
    [
      prelude.bin ⫽ 
      { src = "src/polyglot.dats"
      , target = "target/poly"
      , gcBin = True
      , libs = [ "pthread" ]
      }
    ]
  , test =
    [ prelude.bin ⫽ 
      { src = "test/test.dats"
      , target = "target/test"
      , gcBin = True
      }
    ]
  , man = [ "man/poly.md" ] : Optional Text
  , completions = [ "compleat/poly.usage" ] : Optional Text
  , compiler = [0,3,10]
  , dependencies = prelude.mapPlainDeps [ "concurrency", "specats" ]
  , cflags = [ "-flto", "-O2" ] # (if not cross then [ "-mtune=native" ] else ([] : List Text))
  }
