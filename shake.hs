{-# LANGUAGE ScopedTypeVariables #-}

import           Data.Maybe                (fromMaybe)
import           Data.Monoid
import qualified Data.Text.Lazy            as TL
import           Development.Shake
-- import           Development.Shake.ATS
import           Development.Shake.Linters
import           Dhall
import           Language.ATS.Package.Type
-- import           System.Exit               (ExitCode (..))

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles=".shake" } $ do
    want [ "target/poly"
         , "man/poly.1"
         ]

    "all" ~> need [ "target/poly"
                  , "target/poly-aarch64-linux-gnu"
                  , "target/poly-arm-linux-gnueabihf"
                  , "target/poly-arm-linux-gnueabi"
                  , "target/poly-musl"
                  ]
    "ci" ~> do
        need [ "target/test", "target/poly", "man/poly.1" ]
        sequence_ [ yamllint
                  , shellcheck =<< getShell
                  , tomlcheck
                  , ghc ["."]
                  , hlint ["."]
                  , dhall
                  , atsfmt =<< getAts
                  , stylishHaskell =<< getHs ["."]
                  ]
        cmd_ "target/test"

    "test" ~> do
        need [ "target/test" ]
        cmd "target/test"

    "man/poly.1" %> \_ -> do
        need ["man/MANPAGE.md"]
        cmd ["pandoc", "man/MANPAGE.md", "-s", "-t", "man", "-o", "man/poly.1"]

    "build" %> \_ -> do
        need ["shake.hs"]
        cmd_ ["mkdir", "-p", ".shake"]
        command_ [Cwd ".shake"] "cp" ["../shake.hs", "."]
        r <- command [Cwd ".shake"] "ghc-8.2.2" ["-O", "shake.hs", "-o", "../build", "-Wall", "-Werror", "-Wincomplete-uni-patterns", "-Wincomplete-record-updates"]
        cmd_ [Cwd ".shake"] "rm" ["shake.hs"]
        pure r

    "target/poly-*" %> \out -> do
        let t = drop 1 . dropWhile (/='-') $ out
        let maybeStatic = if t == "musl" then "-static " else ""
        command [] "patscc" ["-atsccomp", t ++ "-gcc" ++ " " ++ maybeStatic ++ "-I/usr/local/lib/ats2-postiats-0.3.8/ccomp/runtime/ -I/usr/local/lib/ats2-postiats-0.3.8/", "src/polyglot.dats", "-DATS_MEMALLOC_LIBC", "-o", "target/poly-" ++ t, "-cleanaft", "-O2", "-flto", "-lpthread"]

    pkgToAction =<< liftIO (input auto (TL.pack "./atspkg.dhall"))

    "bench" ~> do
        need ["target/poly"]
        let dir = " /home/vanessa/git-builds/rust"
        (Stdout (_ :: String)) <- cmd $ "poly " ++ dir
        cmd $ ["bench"] <> ((++dir) <$> ["target/poly", "tokei", "loc", "cloc", "linguist", "numactl --physcpubind=+1 loc -u", "target/poly -p"])

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
        removeFilesAfter "target" ["//*"]
