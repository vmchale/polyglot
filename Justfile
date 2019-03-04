poly:
    @rm -f *_dats.c
    @rm -f *_sats.c
    @poly -e data

ci:
    tomlcheck --file .atsfmt.toml
    yamllint .travis.yml
    yamllint .yamllint
    atspkg test --pkg-args "./native.dhall" target/test
    shellcheck bash/install.sh -e SC2016
    shellcheck bash/buildall
    shellcheck bash/next
    shellcheck bash/release
    shellcheck bash/update

# taskset --cpu-list 1 poly
bench:
    @poly ~/git-builds/OpenBLAS >> /dev/null
    bench "poly ~/git-builds/OpenBLAS" "loc -u ~/git-builds/OpenBLAS" "tokei ~/git-builds/OpenBLAS" "scc -c --cocomo --binary ~/git-builds/OpenBLAS" "gocloc ~/git-builds/OpenBLAS" "enry -mode=line ~/git-builds/OpenBLAS" "linguist ~/git-builds/OpenBLAS" "cloc ~/git-builds/OpenBLAS"
