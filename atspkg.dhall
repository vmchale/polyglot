let prelude = https://raw.githubusercontent.com/vmchale/atspkg/master/ats-pkg/dhall/atspkg-prelude.dhall sha256:33e41e509b6cfd0b075d1a8a5210ddfd1919372f9d972c2da783c6187d2298ba

let not = https://raw.githubusercontent.com/dhall-lang/dhall-lang/9f259cd68870b912fbf2f2a08cd63dc3ccba9dc3/Prelude/Bool/not sha256:723df402df24377d8a853afed08d9d69a0a6d86e2e5b2bac8960b0d4756c7dc4

let
pkg = λ(cfg : { gc : Bool, cross : Bool, parallel : Bool, static : Bool, icc : Bool }) →

    let native =
        if not cfg.cross
            then [ "-mtune=native" ]
            else ([] : List Text)

    let deps =
        if cfg.parallel
            then [ "concurrency" ]
            else ([] : List Text)

    let staticFlag =
        if cfg.static
            then [ "-static" ]
            else ([] : List Text)

    let srcFile =
        if cfg.parallel
            then "polyglot"
            else "compat"

    let cc =
        if cfg.icc
            then prelude.icc
            else prelude.gcc

    let iccFlags =
        if cfg.icc
            then prelude.iccFlags
            else ([] : List Text)
    in

    prelude.default ⫽
        { bin =
            [ prelude.bin ⫽
                { src = "src/${srcFile}.dats"
                , target = "${prelude.atsProject}/poly"
                , gcBin = cfg.gc
                , libs = [ "pthread" ]
                }
            ]
        , test =
            [ prelude.bin ⫽
                { src = "test/test.dats"
                , target = "${prelude.atsProject}/test"
                , gcBin = cfg.gc
                , libs = [ "pthread" ]
                }
            ]
        , bench =
            [ prelude.bin ⫽
                { src = "test/bench.dats"
                , target = "${prelude.atsProject}/bench"
                , gcBin = cfg.gc
                , libs = [ "pthread" ]
                }
            ]
        , man = Some "man/poly.md"
        , completions = Some "compleat/poly.usage"
        , dependencies = (prelude.mapPlainDeps deps) #
            [ prelude.upperDeps { name = "specats", version = [0,2,3] }
            , prelude.lowerDeps { name = "edit-distance", version = [0,4,0] }
            , prelude.plainDeps "ats-bench"
            , prelude.plainDeps "stack"
            ]
        , clib = if cfg.gc
          then [ prelude.upperDeps { name = "gc", version = [7,6,8] } ]
          else prelude.mapPlainDeps ([] : List Text)
        -- TODO: make a mapIncludes function?
        , cflags = [ "-I", "include", "-I", ".", "-flto", "-O2" ] # staticFlag # native # iccFlags
        , ccompiler = prelude.printCompiler cc
        , compiler = [0,3,13]
        , version = [0,3,13]
        , debPkg = prelude.mkDeb
            (prelude.debian "polyglot" ⫽
                { version = [0,5,28]
                , maintainer = "Vanessa McHale <vamchale@gmail.com>"
                , description = "Determine project contents"
                , manpage = Some "man/poly.1"
                , binaries = [ "${prelude.atsProject}/poly" ]
                })
        }

in pkg
