let pkg = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/default.dhall

in pkg //
  { bin =
    [
      { src = "src/compat.dats"
      , target = "target/poly"
      , libs = [ "gc" ]
      , gc = True
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
  , compiler = [0,3,8]
  , dependencies = [ https://raw.githubusercontent.com/vmchale/ats-concurrency/master/atspkg.dhall ]
  , clib = [ https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/libc-atomic-ops.dhall
           , https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/libc-gc.dhall ]
  }
