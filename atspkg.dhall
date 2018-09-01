let prelude = http://hackage.haskell.org/package/ats-pkg/src/dhall/atspkg-prelude.dhall
in

let not = http://hackage.haskell.org/package/dhall/src/Prelude/Bool/not
in

let
pkg = λ(cfg : { gc : Bool, cross : Bool, parallel : Bool, static : Bool, icc : Bool }) →

    let native =
        if not cfg.cross
            then [ "-mtune=native" ]
            else ([] : List Text)
        in

    let deps =
        if cfg.parallel
            then [ "concurrency" ]
            else ([] : List Text)
        in

    let staticFlag =
        if cfg.static
            then [ "-static" ]
            else ([] : List Text)
        in

    let srcFile =
        if cfg.parallel
            then "polyglot"
            else "compat"
        in

    let cc =
        if cfg.icc
            then prelude.icc
            else prelude.gcc
    in

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
            , prelude.bin ⫽
                { src = "test/bench.dats"
                , target = "${prelude.atsProject}/bench"
                , gcBin = cfg.gc
                , libs = [ "pthread" ]
                }
            ]
        , man = [ "man/poly.md" ] : Optional Text
        , completions = [ "compleat/poly.usage" ] : Optional Text
        , dependencies = (prelude.mapPlainDeps deps) #
            [ prelude.upperDeps { name = "specats", version = [0,2,3] }
            , prelude.lowerDeps { name = "edit-distance", version = [0,4,0] }
            , prelude.plainDeps "ats-bench"
            ]
        -- TODO: make a mapIncludes function?
        , cflags = [ "-I", "include", "-I", ".", "-flto", "-O2" ] # staticFlag # native # iccFlags
        , ccompiler = prelude.printCompiler cc
        , debPkg = prelude.mkDeb
            (prelude.debian "polyglot" ⫽
                { version = [0,5,7]
                , maintainer = "Vanessa McHale <vamchale@gmail.com>"
                , description = "Determine project contents"
                , manpage = [ "man/poly.1" ]
                    : Optional Text
                , binaries = [ "${prelude.atsProject}/poly" ]
                })
        -- , atsLib = False
        }

in pkg
