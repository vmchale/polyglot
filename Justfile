poly:
    poly -e data


# attempted: s390x-linux-gnu, powerpc64-linux-gnu, h8300-hms, powerpc-linux-gnu; gcc-mips-linux-gnu; gcc-mips64-linux-gnuabi64; gcc-mingw-w64-x86-64; arm-linux-gnueabi; msp430; hhpa-linux-gnu
all:
    atspkg clean ; atspkg nuke ; atspkg build --target=arm-linux-gnueabihf ; exit 0

ci:
    tomlcheck --file .atsfmt.toml
    yamllint .travis.yml
    yamllint .yamllint
    atspkg test

bench:
    bench "poly ~/git-builds/rust" "loc -u ~/git-builds/rust" "tokei ~/git-builds/rust"

release: all
    git tag "$(grep -P -o '\d+\.\d+\.\d+' src/cli.dats)"
    git push origin --tags
    git tag -d "$(grep -P -o '\d+\.\d+\.\d+' src/cli.dats)"
    git push origin master
    github-release release -s $(cat ~/.git-token) -u vmchale -r polyglot -t "$(grep -P -o '\d+\.\d+\.\d+' src/cli.dats)"
    github-release upload -s $(cat ~/.git-token) -u vmchale -r polyglot -n poly-arm-linux-gnueabihf -f target/poly -t "$(grep -P -o '\d+\.\d+\.\d+' src/cli.dats)"
    github-release upload -s $(cat ~/.git-token) -u vmchale -r polyglot -n poly.1 -f man/poly.1 -t "$(grep -P -o '\d+\.\d+\.\d+' src/cli.dats)"
    github-release upload -s $(cat ~/.git-token) -u vmchale -r polyglot -n poly.usage -f compleat/poly.usage -t "$(grep -P -o '\d+\.\d+\.\d+' src/cli.dats)"

next:
    @export VERSION=$(cat src/polyglot.dats | grep -P -o '\d+\.\d+\.\d+' src/cli.dats | awk -F. '{$NF+=1; print $0}' | sed 's/ /\./g') && echo $VERSION && sed -i "s/[0-9]\+\.[0-9]\+\.[0-9]\+\+/$VERSION/" src/cli.dats
    @git commit -am "version bump"
