#!/usr/bin/env sh

set -e
set pipefail

getTarget() {
    if [ "$(uname)" = "Darwin" ]
    then
        echo "poly-$(uname -m)-apple-darwin"
    else
        case $(uname -m) in
            "x86_64") MACHINE="unknown-linux-icc";;
            "arm") MACHINE="linux-gnueabihf";;
            "mips64"*) MACHINE="linux-gnuabi64";;
            *) MACHINE="linux-gnu";;
        esac
        echo "poly-$(uname -m)-$MACHINE"
    fi
}

addBin() {

    printf 'export PATH=$HOME/.local/bin:$PATH' >> "$HOME"/.bashrc
    export PATH=$HOME/.local/bin:$PATH

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

    # install manpages + compleat usage
    if command -v wget > /dev/null ; then
        wget https://github.com/vmchale/polyglot/releases/download/"$latest"/poly.1 -O "$man_dest"
        wget https://github.com/vmchale/polyglot/releases/download/"$latest"/poly.usage -O "$compleat_dest"
    else
        curl -L https://github.com/vmchale/polyglot/releases/download/"$latest"/poly.1 -o "$man_dest"
        curl -L https://github.com/vmchale/polyglot/releases/download/"$latest"/poly.usage -o "$compleat_dest"
    fi

    # if we're on mac, use atspkg to install, otherwise download binaries
    if [ "$(uname)" = "Darwin" ]
    then

        curl -sSl https://raw.githubusercontent.com/vmchale/atspkg/master/bash/install.sh | sh -s
        atspkg remote https://github.com/vmchale/polyglot/archive/master.zip --pkg-args '{ gc = True, cross = True, parallel = True, static = False, icc = False }'

    else

        if command -v wget > /dev/null ; then
            wget https://github.com/vmchale/polyglot/releases/download/"$latest"/"$binname" -O "$dest"
        else
            curl -L https://github.com/vmchale/polyglot/releases/download/"$latest"/"$binname" -o "$dest"
        fi

        chmod +x "$dest"

        case :$PATH: in
            *:$HOME/.local/bin:*) ;;
            *) addBin ;;
        esac

    fi

}

main
