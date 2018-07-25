{- Imports -}
let prelude = http://hackage.haskell.org/package/ats-pkg/src/dhall/atspkg-prelude.dhall
in

let not = http://hackage.haskell.org/package/dhall/src/Prelude/Bool/not
in

-- TODO figure out icc configuration options

{- Configuration helpers -}
let iccFlags =
    [ "-D__PURE_INTEL_C99_HEADERS__" ]
in

let
pkg = λ(x : { cross : Bool, parallel : Bool }) →

    let native =
        if not x.cross
            then [ "-mtune=native" ]
            else ([] : List Text)
        in

    let deps =
        if x.parallel
            then [ "concurrency" ]
            else ([] : List Text)
        in

    let srcFile =
        if x.parallel
            then "polyglot"
            else "compat"
        in

    let cc =
        if not x.cross then "icc" else "gcc"
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
    , dependencies = (prelude.mapPlainDeps deps)
        # [ prelude.upperDeps { name = "specats", version = [0,2,3] }, prelude.lowerDeps { name = "edit-distance", version = [0,3,0] }]
    , cflags = [ "-flto", "-O2", "-static" ] # native # iccFlags
    -- , ccompiler = cc
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
in pkg
