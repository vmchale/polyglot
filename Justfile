windows:
    patscc src/polyglot-compat.dats -atsccomp "x86_64-w64-mingw32-gcc -flto -I/usr/local/lib/ats2-postiats-0.3.8/ccomp/runtime/ -I/usr/local/lib/ats2-postiats-0.3.8/" -DATS_MEMALLOC_LIBC -o target/poly.exe -cleanaft -O2 -mtune=native

cross:
    patscc src/polyglot-compat.dats -atsccomp "clang -target x86_64-apple-darwin -flto -I/usr/local/lib/ats2-postiats-0.3.8/ccomp/runtime/ -I/usr/local/lib/ats2-postiats-0.3.8/" -DATS_MEMALLOC_LIBC -o target/poly-clang -cleanaft -O2 -mtune=native

ci:
    @yamllint .travis.yml
    @hlint shake.hs
    @shellcheck bash/install.sh
    @tomlcheck --file .atsfmt.toml
    @./build

release:
    ./build all
    git tag "$(grep -P -o '\d+\.\d+\.\d+' src/polyglot.dats)"
    git push origin --tags
    git tag -d "$(grep -P -o '\d+\.\d+\.\d+' src/polyglot.dats)"
    git push origin master
    github-release release -s $(cat ~/.git-token) -u vmchale -r polyglot -t "$(grep -P -o '\d+\.\d+\.\d+' src/polyglot.dats)"
    github-release upload -s $(cat ~/.git-token) -u vmchale -r polyglot -n poly.1 -f man/poly.1 -t "$(grep -P -o '\d+\.\d+\.\d+' src/polyglot.dats)"
    github-release upload -s $(cat ~/.git-token) -u vmchale -r polyglot -n poly.usage -f compleat/poly.usage -t "$(grep -P -o '\d+\.\d+\.\d+' src/polyglot.dats)"
    github-release upload -s $(cat ~/.git-token) -u vmchale -r polyglot -n poly-arm-linux-gnueabihf -f target/poly-arm-linux-gnueabihf -t "$(grep -P -o '\d+\.\d+\.\d+' src/polyglot.dats)"
    github-release upload -s $(cat ~/.git-token) -u vmchale -r polyglot -n poly-arm-linux-gnueabi -f target/poly-arm-linux-gnueabi -t "$(grep -P -o '\d+\.\d+\.\d+' src/polyglot.dats)"
    github-release upload -s $(cat ~/.git-token) -u vmchale -r polyglot -n poly-aarch64-linux-gnu -f target/poly-aarch64-linux-gnu -t "$(grep -P -o '\d+\.\d+\.\d+' src/polyglot.dats)"
    github-release upload -s $(cat ~/.git-token) -u vmchale -r polyglot -n poly-musl -f target/poly-musl -t "$(grep -P -o '\d+\.\d+\.\d+' src/polyglot.dats)"

next:
    @export VERSION=$(cat src/polyglot.dats | grep -P -o '\d+\.\d+\.\d+' src/polyglot.dats | awk -F. '{$NF+=1; print $0}' | sed 's/ /\./g') && echo $VERSION && sed -i "s/[0-9]\+\.[0-9]\+\.[0-9]\+\+/$VERSION/" src/polyglot.dats
    @git commit -am "version bump"
