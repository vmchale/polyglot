#!/usr/bin/env sh

set -e
set pipefail

# from: https://stackoverflow.com/a/4024263
verlte() {
    [  "$1" = "$(printf "%s\\n%s" "$1" "$2" | sort -V | head -n1)" ]
}

libcVersion() {
    ldd --version | head -n1 | awk 'NF>1{print $NF}'
}

getTarget() {
    if [ "$(uname)" = "Darwin" ]
    then
        echo "poly-$(uname -m)-apple-darwin"
    else
        case $(uname -m) in
            "x86_64") verlte 2.27 "$(libcVersion)"
                case $? in
                    1) MACHINE="unknown-linux";;
                    0) MACHINE="unknown-linux-icc";;
                esac;;
            "arm") MACHINE="linux-gnueabihf";;
            "mips64"*) MACHINE="linux-gnuabi64";;
            *) MACHINE="linux-gnu";;
        esac
        echo "poly-$(uname -m)-$MACHINE"
    fi
}

main() {

    latest="$(curl -s https://github.com/vmchale/polyglot/releases/latest/ | cut -d'"' -f2 | rev | cut -d'/' -f1 | rev)"
    binname=$(getTarget)

    mkdir -p "$HOME/.local/bin"
    mkdir -p "$HOME/.local/share/man/man1/"
    mkdir -p "$HOME/.compleat"

    dest=$HOME/.local/bin/poly
    man_dest=$HOME/.local/share/man/man1/poly.1
    compleat_dest=$HOME/.compleat/poly.usage

    if command -v wget > /dev/null ; then
        wget https://github.com/vmchale/polyglot/releases/download/"$latest"/"$binname" -O "$dest"
        wget https://github.com/vmchale/polyglot/releases/download/"$latest"/poly.1 -O "$man_dest"
        wget https://github.com/vmchale/polyglot/releases/download/"$latest"/poly.usage -O "$compleat_dest"
    else
        curl https://github.com/vmchale/polyglot/releases/download/"$latest"/"$binname" -o "$dest"
        curl https://github.com/vmchale/polyglot/releases/download/"$latest"/poly.1 -o "$man_dest"
        curl https://github.com/vmchale/polyglot/releases/download/"$latest"/poly.usage -o "$compleat_dest"
    fi
    chmod +x "$dest"

}

main
