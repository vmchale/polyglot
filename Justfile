poly:
    @rm -f *_dats.c
    @poly -e data

# TODO arm-linux-gnueabi sh4-linux-gnu hppa-linux-gnu hppa64-linux-gnu etc.
all:
    atspkg build --target=s390x-linux-gnu
    atspkg build --target=arm-linux-gnueabihf
    atspkg build --target=powerpc64-linux-gnu
    atspkg build --target=powerpc64le-linux-gnu
    atspkg build --target=powerpc-linux-gnu
    atspkg build --target=aarch64-linux-gnu
    atspkg build --target=alpha-linux-gnu
    atspkg build --target=m68k-linux-gnu
    atspkg build --target=mips-linux-gnu
    atspkg build --target=mipsel-linux-gnu
    atspkg build --target=mips64-linux-gnuabi64
    atspkg build --target=mips64el-linux-gnuabi64

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
    github-release upload -s $(cat ~/.git-token) -u vmchale -r polyglot -n poly-s390x-linux-gnu -f target/poly-s390x-linux-gnu -t "$(grep -P -o '\d+\.\d+\.\d+' src/cli.dats)"
    github-release upload -s $(cat ~/.git-token) -u vmchale -r polyglot -n poly-arm-linux-gnueabihf -f target/poly-arm-linux-gnueabihf -t "$(grep -P -o '\d+\.\d+\.\d+' src/cli.dats)"
    github-release upload -s $(cat ~/.git-token) -u vmchale -r polyglot -n poly-aarch64-linux-gnu -f target/poly-aarch64-linux-gnu -t "$(grep -P -o '\d+\.\d+\.\d+' src/cli.dats)"
    github-release upload -s $(cat ~/.git-token) -u vmchale -r polyglot -n poly-powerpc-linux-gnu -f target/poly-powerpc-linux-gnu -t "$(grep -P -o '\d+\.\d+\.\d+' src/cli.dats)"
    github-release upload -s $(cat ~/.git-token) -u vmchale -r polyglot -n poly-powerpc64-linux-gnu -f target/poly-powerpc64-linux-gnu -t "$(grep -P -o '\d+\.\d+\.\d+' src/cli.dats)"
    github-release upload -s $(cat ~/.git-token) -u vmchale -r polyglot -n poly-powerpc64le-linux-gnu -f target/poly-powerpc64le-linux-gnu -t "$(grep -P -o '\d+\.\d+\.\d+' src/cli.dats)"
    github-release upload -s $(cat ~/.git-token) -u vmchale -r polyglot -n poly-alpha-linux-gnu -f target/poly-alpha-linux-gnu -t "$(grep -P -o '\d+\.\d+\.\d+' src/cli.dats)"
    github-release upload -s $(cat ~/.git-token) -u vmchale -r polyglot -n poly-mips-linux-gnu -f target/poly-mips-linux-gnu -t "$(grep -P -o '\d+\.\d+\.\d+' src/cli.dats)"
    github-release upload -s $(cat ~/.git-token) -u vmchale -r polyglot -n poly-mipsel-linux-gnu -f target/poly-mipsel-linux-gnu -t "$(grep -P -o '\d+\.\d+\.\d+' src/cli.dats)"
    github-release upload -s $(cat ~/.git-token) -u vmchale -r polyglot -n poly-mips64-linux-gnuabi64 -f target/poly-mipsel-linux-gnu -t "$(grep -P -o '\d+\.\d+\.\d+' src/cli.dats)"
    github-release upload -s $(cat ~/.git-token) -u vmchale -r polyglot -n poly-mips64el-linux-gnuabi64 -f target/poly-mipsel-linux-gnu -t "$(grep -P -o '\d+\.\d+\.\d+' src/cli.dats)"
    github-release upload -s $(cat ~/.git-token) -u vmchale -r polyglot -n poly.1 -f man/poly.1 -t "$(grep -P -o '\d+\.\d+\.\d+' src/cli.dats)"
    github-release upload -s $(cat ~/.git-token) -u vmchale -r polyglot -n poly.usage -f compleat/poly.usage -t "$(grep -P -o '\d+\.\d+\.\d+' src/cli.dats)"

next:
    @export VERSION=$(cat src/polyglot.dats | grep -P -o '\d+\.\d+\.\d+' src/cli.dats | awk -F. '{$NF+=1; print $0}' | sed 's/ /\./g') && echo $VERSION && sed -i "s/[0-9]\+\.[0-9]\+\.[0-9]\+\+/$VERSION/" src/cli.dats
    @git commit -am "version bump"
