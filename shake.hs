#!/usr/bin/env stack
-- stack runghc --resolver nightly-2017-11-16 --package shake --install-ghc

import           Data.Maybe        (fromMaybe)
import           Data.Monoid
import           Development.Shake

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles=".shake" } $ do
    want [ "target/polyglot" ]

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
        -- let patshome = "/usr/local/lib/ats2-postiats-0.3.8"
        command [EchoStderr False]{-, AddEnv "PATSHOME" patshome]-} "patscc" (dats ++ ["-DATS_MEMALLOC_LIBC", "-o", "target/polyglot", "-cleanaft", "-O3", "-mtune=native"])

    "bench" ~> do
        need ["target/polyglot"]
        let dir = " /home/vanessa/programming/haskell/done"
        cmd $ ["bench"] <> ((++dir) <$> ["target/polyglot", "tokei", "loc --exclude 'dist'", "enry", "cloc"])

    "install" ~> do
        need ["target/polyglot"]
        home <- getEnv "HOME"
        cmd ["cp", "target/polyglot", fromMaybe "" home ++ "/.local/bin/poly"]

    "run" ~> do
        need ["target/polyglot"]
        cmd ["./target/polyglot", "."]

    "clean" ~> do
        cmd_ ["sn", "c"]
        removeFilesAfter "." ["//*.c", "tags", "build"]
        removeFilesAfter ".shake" ["//*"]
        removeFilesAfter "target" ["//*"]
