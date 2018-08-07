poly:
    @rm -f *_dats.c
    @poly -e data

ci:
    tomlcheck --file .atsfmt.toml
    yamllint .travis.yml
    yamllint .yamllint
    atspkg test

bench:
    bench "poly ~/git-builds/rust" "loc -u ~/git-builds/rust" "tokei ~/git-builds/rust" "scc -c ~/git-builds/rust" "gocloc ~/git-builds/rust" "enry -mode=line ~/git-builds/rust" "linguist ~/git-builds/rust" "cloc ~/git-builds/rust"
    bench "poly ~/git-builds/go" "loc -u ~/git-builds/go" "tokei ~/git-builds/go" "scc -c ~/git-builds/go" "gocloc ~/git-builds/go" "enry -mode=line ~/git-builds/go" "linguist ~/git-builds/go" "cloc ~/git-builds/go"
