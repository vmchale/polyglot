update:
    rustup update
    sudo apt update && sudo apt upgrade
    cargo install --git https://github.com/cgag/loc --force
    cargo install tokei --force
    go get -u github.com/boyter/scc/
    go get -u go get gopkg.in/src-d/enry.v1/...
    go get -u github.com/hhatto/gocloc/cmd/gocloc
    sudo gem install github-linguist

# taskset --cpu-list 1 poly
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
    shellcheck bash/build
    shellcheck bash/next
    shellcheck bash/release

bench:
    @poly ~/git-builds/rust >> /dev/null
    bench "poly ~/git-builds/rust" "loc -u ~/git-builds/rust" "tokei ~/git-builds/rust" "scc -c -co ~/git-builds/rust" "gocloc ~/git-builds/rust" "enry -mode=line ~/git-builds/rust" "linguist ~/git-builds/rust" "cloc ~/git-builds/rust"
    bench "poly ~/git-builds/OpenBLAS" "loc -u ~/git-builds/OpenBLAS" "tokei ~/git-builds/OpenBLAS" "scc -c -co ~/git-builds/OpenBLAS" "gocloc ~/git-builds/OpenBLAS" "enry -mode=line ~/git-builds/OpenBLAS" "linguist ~/git-builds/OpenBLAS" "cloc ~/git-builds/OpenBLAS"
    bench "poly ~/git-builds/go" "loc -u ~/git-builds/go" "tokei ~/git-builds/go" "scc -c -co ~/git-builds/go" "gocloc ~/git-builds/go" "enry -mode=line ~/git-builds/go" "linguist ~/git-builds/go" "cloc ~/git-builds/go"
    bench "poly ~/git-builds/linux" "loc -u ~/git-builds/linux" "tokei ~/git-builds/linux" "scc -c -co ~/git-builds/linux" "linuxcloc ~/git-builds/linux" "enry -mode=line ~/git-builds/linux" "linguist ~/git-builds/linux" "cloc ~/git-builds/linux" "sloccount ~/git-builds/linux"
