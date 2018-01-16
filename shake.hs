#!/usr/bin/env stack
-- stack runghc --resolver nightly-2017-12-01 --package shake --install-ghc

{-# LANGUAGE ScopedTypeVariables #-}

import           Data.Maybe        (fromMaybe)
import           Data.Monoid
import           Development.Shake
import           System.Exit       (ExitCode (..))

{-# ANN module "HLint: ignore Reduce duplication" #-}

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles=".shake" } $ do
    want [ "target/poly"
         , "man/poly.1"
         -- , "target/poly-s390x-linux-gnu"
         -- , "target/poly-aarch64-linux-gnu"
         -- , "target/poly-sparc64-linux-gnu"
         -- , "target/poly-arm-linux-gnueabihf"
         -- , "target/poly-arm-linux-gnueabi"
         ]

    "lint" ~> do
        cmd_ "shellcheck --exclude SC2155 bash/install.sh"
        cmd "hlint shake.hs"

    "man/poly.1" %> \_ -> do
        need ["man/MANPAGE.md"]
        cmd ["pandoc", "man/MANPAGE.md", "-s", "-t", "man", "-o", "man/poly.1"]

    "build" %> \_ -> do
        need ["shake.hs"]
        cmd_ ["mkdir", "-p", ".shake"]
        command_ [Cwd ".shake"] "cp" ["../shake.hs", "."]
        command [Cwd ".shake"] "ghc-8.2.2" ["-O", "shake.hs", "-o", "../build", "-Wall", "-Werror", "-Wincomplete-uni-patterns", "-Wincomplete-record-updates"]

    "target/poly-*" %> \out -> do
        let target = drop 1 . dropWhile (/='-') $ out
        cmd $ "patscc -atsccomp " ++ target ++ "-gcc -I/usr/local/lib/ats2-postiats-0.3.8/ccomp/runtime/ -I/usr/local/lib/ats2-postiats-0.3.8/ src/polyglot.dats -DATS_MEMALLOC_LIBC -o target/poly-" ++ target ++ " -cleanaft -O2 -flto"

    "target/poly.c" %> \_ -> do
        dats <- getDirectoryFiles "" ["//*.dats"]
        sats <- getDirectoryFiles "" ["//*.sats"]
        need $ dats <> sats
        cmd_ ("patscc -DATS_MEMALLOC_LIBC -ccats " ++ unwords dats)
        cmd "mv poly_dats.c" "target/poly.c"

    "target/poly" %> \_ -> do
        dats <- getDirectoryFiles "" ["//*.dats"]
        sats <- getDirectoryFiles "" ["//*.sats"]
        need $ dats <> sats
        cmd_ ["mkdir", "-p", "target"]
        let patshome = "/usr/local/lib/ats2-postiats-0.3.8"
        (Exit c, Stderr err) <- command [EchoStderr False, AddEnv "PATSHOME" patshome] "patscc" ["src/polyglot.dats", "-atsccomp", "gcc -flto -I/usr/local/lib/ats2-postiats-0.3.8/ccomp/runtime/ -I/usr/local/lib/ats2-postiats-0.3.8/", "-DATS_MEMALLOC_LIBC", "-o", "target/poly", "-cleanaft", "-O2", "-mtune=native", "-lpthread"]
        cmd_ [Stdin err] Shell "pats-filter"
        if c /= ExitSuccess
            then error "patscc failure"
            else pure ()

    "bench" ~> do
        need ["target/poly"]
        let dir = " /home/vanessa/git-builds/rust"
        (Stdout (_ :: String)) <- cmd $ "poly " ++ dir
        cmd_ ["mkdir", "-p", "docs"]
        cmd $ ["bench", "--output=docs/bench.html"] <> ((++dir) <$> ["target/poly", "tokei", "loc", "cloc", "linguist", "numactl --physcpubind=+1 loc -u"])

    "install" ~> do
        need ["target/poly", "man/poly.1", "compleat/poly.usage"]
        home <- getEnv "HOME"
        cmd_ ["mkdir", "-p", fromMaybe "" home ++ "/.compleat"]
        cmd_ ["cp", "man/poly.1", fromMaybe "" home ++ "/.local/share/man/man1/"]
        cmd_ ["cp", "compleat/poly.usage", fromMaybe "" home ++ "/.compleat"]
        cmd ["cp", "target/poly", fromMaybe "" home ++ "/.local/bin/poly"]

    "run" ~> do
        need ["target/poly"]
        cmd ["./target/poly", "."]

    "clean" ~> do
        cmd_ ["sn", "c"]
        cmd_ ["rm", "-f", "man/poly.1"]
        removeFilesAfter "." ["//*.c", "tags"]
        removeFilesAfter ".shake" ["//*"]
        removeFilesAfter "docs" ["//*"]
        removeFilesAfter "target" ["//*"]
