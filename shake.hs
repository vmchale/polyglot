#!/usr/bin/env stack
-- stack runghc --resolver nightly-2017-11-16 --package shake --install-ghc

import           Data.Maybe        (fromMaybe)
import           Data.Monoid
import           Development.Shake

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles=".shake" } $ do
    want [ "target/polyglot", "man/poly.1" ]

    "man/poly.1" %> \_ -> do
        need ["man/MANPAGE.md"]
        cmd ["pandoc", "man/MANPAGE.md", "-s", "-t", "man", "-o", "man/poly.1"]

    "build" %> \_ -> do
        need ["shake.hs"]
        cmd_ ["mkdir", "-p", ".shake"]
        command_ [Cwd ".shake"] "cp" ["../shake.hs", "."]
        command [Cwd ".shake"] "ghc" ["-O", "shake.hs", "-o", "../build", "-Wall", "-Werror", "-Wincomplete-uni-patterns", "-Wincomplete-record-updates"]

    "target/polyglot" %> \_ -> do
        dats <- getDirectoryFiles "" ["//*.dats"]
        sats <- getDirectoryFiles "" ["//*.sats"]
        need $ dats <> sats
        cmd_ ["mkdir", "-p", "target"]
        let patshome = "/usr/local/lib/ats2-postiats-0.3.8"
        -- FIXME use pats-filter on the output.
        command [EchoStderr False, AddEnv "PATSHOME" patshome] "patscc" (dats ++ ["-DATS_MEMALLOC_LIBC", "-o", "target/polyglot", "-cleanaft", "-O3", "-mtune=native"])

    "bench" ~> do
        need ["target/polyglot"]
        let dir = " /home/vanessa/git-builds/rust"
        cmd $ ["bench"] <> ((++dir) <$> ["target/polyglot", "tokei", "loc -u", "enry", "cloc", "linguist"])

    "install" ~> do
        need ["target/polyglot", "man/poly.1"]
        home <- getEnv "HOME"
        cmd_ ["cp", "man/poly.1", fromMaybe "" home ++ "/.local/share/man/man1/"]
        cmd ["cp", "target/polyglot", fromMaybe "" home ++ "/.local/bin/poly"]

    "run" ~> do
        need ["target/polyglot"]
        cmd ["./target/polyglot", "."]

    "clean" ~> do
        cmd_ ["sn", "c"]
        cmd_ ["rm", "-f", "man/poly.1"]
        removeFilesAfter "." ["//*.c", "tags", "build"]
        removeFilesAfter ".shake" ["//*"]
        removeFilesAfter "target" ["//*"]
