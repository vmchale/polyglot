#!/usr/bin/env bash

main() {

    local latest="$(curl -s https://github.com/vmchale/polyglot/releases/latest/ | cut -d'"' -f2 | rev | cut -d'/' -f1 | rev)"
    mkdir -p "$HOME/.local/bin"
    mkdir -p "$HOME/.local/share/man/man1/"
    mkdir -p "$HOME/.compleat"
    local dest=$HOME/.local/bin/poly
    local man_dest=$HOME/.local/bin/poly.1
    local compleat_dest=$HOME/.compleat/poly.usage
    if which duma > /dev/null ; then
        duma https://github.com/vmchale/polyglot/releases/download/"$latest"/poly -O "$dest"
        duma https://github.com/vmchale/polyglot/releases/download/"$latest"/poly.1 -O "$man_dest"
        duma https://github.com/vmchale/polyglot/releases/download/"$latest"/poly.usage -O "$compleat_dest"
    else
        wget https://github.com/vmchale/polyglot/releases/download/"$latest"/poly -O "$dest"
        wget https://github.com/vmchale/polyglot/releases/download/"$latest"/poly.1 -O "$man_dest"
        wget https://github.com/vmchale/polyglot/releases/download/"$latest"/poly.usage -O "$compleat_dest"
    fi
    chmod +x "$dest"

}

main
