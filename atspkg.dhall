{- Imports -}
let prelude = http://hackage.haskell.org/package/ats-pkg/src/dhall/atspkg-prelude.dhall
in

let not = http://hackage.haskell.org/package/dhall/src/Prelude/Bool/not
in

{- Configuration -}
let cross = True
in

let parallel = True
in

let cc = "gcc"
in

-- TODO figure out icc configuration options
-- also --disable-threads for redox? idk

{- Configuration helpers -}
let srcFile =
  if parallel
    then "polyglot"
    else "compat"
in

let deps =
  if parallel
    then [ "concurrency", "specats", "edit-distance" ]
    else [ "specats", "edit-distance" ]
in

let native =
  if not cross
    then [ "-mtune=native" ]
    else ([] : List Text)
in

let iccFlags =
    [ "-D__PURE_INTEL_C99_HEADERS__" ]
in

prelude.default ⫽
  { bin =
    [
      prelude.bin ⫽
      { src = "src/${srcFile}.dats"
      , target = "${prelude.atsProject}/poly"
      , gcBin = True
      , libs = [ "pthread" ]
      }
    ]
  , test =
    [ prelude.bin ⫽
      { src = "test/test.dats"
      , target = "${prelude.atsProject}/test"
      , gcBin = True
      , libs = [ "pthread" ]
      }
    ]
  , man = [ "man/poly.md" ] : Optional Text
  , completions = [ "compleat/poly.usage" ] : Optional Text
  , dependencies = prelude.mapPlainDeps deps
  , cflags = [ "-flto", "-O2", "-static" ] # native # iccFlags
  , ccompiler = cc
  , debPkg = prelude.mkDeb
      (prelude.debian "polyglot" ⫽
        { version = [0,4,45]
        , maintainer = "Vanessa McHale <vamchale@gmail.com>"
        , description = "Determine project contents"
        , manpage = [ "man/poly.1" ]
          : Optional Text
        , binaries = [ "${prelude.atsProject}/poly" ]
        })
  }
