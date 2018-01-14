ci:
    @yamllint .travis.yml
    @hlint shake.hs
    @shellcheck bash/install.sh
    @tomlcheck --file .atsfmt.toml
    @./build

release:
    git tag "$(grep -P -o '\d+\.\d+\.\d+' src/polyglot.dats)"
    git push origin --tags
    git tag -d "$(grep -P -o '\d+\.\d+\.\d+' src/polyglot.dats)"
    git push origin master
    pijul tag -m "$(grep -P -o '\d+\.\d+\.\d+' src/polyglot.dats)"
    github-release release -s $(cat ~/.git-token) -u vmchale -r polyglot -t "$(grep -P -o '\d+\.\d+\.\d+' src/polyglot.dats)"
    github-release upload -s $(cat ~/.git-token) -u vmchale -r polyglot -n poly.1 -f man/poly.1 -t "$(grep -P -o '\d+\.\d+\.\d+' src/polyglot.dats)"
    github-release upload -s $(cat ~/.git-token) -u vmchale -r polyglot -n poly.usage -f compleat/poly.usage -t "$(grep -P -o '\d+\.\d+\.\d+' src/polyglot.dats)"

next:
    @export VERSION=$(cat src/polyglot.dats | grep -P -o '\d+\.\d+\.\d+' src/polyglot.dats | awk -F. '{$NF+=1; print $0}' | sed 's/ /\./g') && echo $VERSION && sed -i "s/[0-9]\+\.[0-9]\+\.[0-9]\+\+/$VERSION/" src/polyglot.dats
    @git commit -am "version bump"
