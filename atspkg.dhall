let pkg = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/default.dhall
in
let dbin = https://raw.githubusercontent.com/vmchale/atspkg/master/pkgs/default-bin.dhall

in pkg //
  { bin =
    [
      dbin //
      { src = "src/compat.dats"
      , target = "target/poly"
      , gc = True
      }
    ]
  , test =
    [ dbin //
      { src = "test/test.dats"
      , target = "target/test"
      , gc = True
      }
    ]
  , man = ([ "man/poly.md" ] : Optional Text)
  , compiler = [0,3,8]
  , version = [0,3,9]
  , dependencies = [ https://raw.githubusercontent.com/vmchale/ats-concurrency/master/atspkg.dhall
                   , https://raw.githubusercontent.com/vmchale/ats-linecount/master/pkg.dhall
                   ]
  , cflags = [ "-flto", "-O2", "-mtune=native" ]
  }
