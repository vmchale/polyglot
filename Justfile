poly:
    @rm -f *_dats.c
    @poly -e data

all:
    atspkg build --pkg-args "./native.dhall" target/polyglot.deb
    atspkg build --pkg-args "./gc.dhall" --target=s390x-linux-gnu
    atspkg build --pkg-args "./gc.dhall" --target=arm-linux-gnueabihf
    atspkg build --pkg-args "./gc.dhall" --target=powerpc64-linux-gnu
    atspkg build --pkg-args "./gc.dhall" --target=powerpc64le-linux-gnu
    atspkg build --pkg-args "./gc.dhall" --target=powerpc-linux-gnu
    atspkg build --pkg-args "./gc.dhall" --target=aarch64-linux-gnu
    atspkg build --pkg-args "./gc.dhall" --target=alpha-linux-gnu
    atspkg build --pkg-args "./gc.dhall" --target=m68k-linux-gnu
    atspkg build --pkg-args "./gc.dhall" --target=mips-linux-gnu
    atspkg build --pkg-args "./gc.dhall" --target=mipsel-linux-gnu
    atspkg build --pkg-args "./gc.dhall" --target=mips64-linux-gnuabi64
    atspkg build --pkg-args "./gc.dhall" --target=mips64el-linux-gnuabi64
    atspkg build --pkg-args "./gc.dhall" --target=i686-linux-gnu
    atspkg build --pkg-args "./no-gc.dhall" --target=riscv64-linux-gnu
    atspkg build --pkg-args "./no-gc.dhall" --target=sh4-linux-gnu
    atspkg build --pkg-args "./no-gc.dhall" --target=hppa-linux-gnu
    atspkg build --pkg-args "./no-gc.dhall" --target=sparc64-linux-gnu
    atspkg build --pkg-args "./no-gc.dhall" --target=arm-linux-gnueabi

ci:
    tomlcheck --file .atsfmt.toml
    yamllint .travis.yml
    yamllint .yamllint
    atspkg test

bench:
    bench "poly ~/git-builds/rust" "loc -u ~/git-builds/rust" "tokei ~/git-builds/rust" "scc -c ~/git-builds/rust" "gocloc ~/git-builds/rust" "enry -mode=line ~/git-builds/rust" "linguist ~/git-builds/rust" "cloc ~/git-builds/rust"
    bench "poly ~/git-builds/go" "loc -u ~/git-builds/go" "tokei ~/git-builds/go" "scc -c ~/git-builds/go" "gocloc ~/git-builds/go" "enry -mode=line ~/git-builds/go" "linguist ~/git-builds/go" "cloc ~/git-builds/go"
