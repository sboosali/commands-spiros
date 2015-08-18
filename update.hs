#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}
import Turtle
-- runhaskell ./update.hs
-- cabal install --user turtle
-- https://hackage.haskell.org/package/turtle
shell_ cmd      = shell cmd      empty
proc_  cmd args = proc  cmd args empty
rsync      args = proc_ "rsync" args
fp2txt = format fp
--txt2fp = fromString 

(-:) = (,)
bimapPair f g = map (\(a,b) -> (f a, g b))

__file__ = "update.hs"
idir = "/Users/sboosalis/commands-core"

main = do
 chmod rwx __file__

 mktree ("./sources/Commands/Plugins/Example")
 rsync [ "-av" --, "--dry-run"
       , idir<>"/sources/Commands/Plugins/Example/"
       ,        "sources/Commands/Plugins/Example" ]
 rsync [ "-av" --, "--dry-run"
       , idir<>"/sources/Commands/Plugins/Example.hs"
       ,        "sources/Commands/Plugins/Example.hs" ]
 mktree ("sources/Commands/Frontends/Dragon13/")
 rsync [ "-av" --, "--dry-run"
       , idir<>"/sources/Commands/Frontends/Dragon13/Shim.hs"
       ,        "sources/Commands/Frontends/Dragon13/Shim.hs" ]
 echo ""
 view$ find chars "sources"

 odir <- pwd
 let filesAbs = map (\(fileA,fileB) -> (odir<>fileA, odir<>fileB)) filePairs
 testEdiffFiles filesAbs
 echo ""
 echoEdiffFiles filesAbs

 return ()

testEdiffFiles
 = traverse testfileDie
 . concatMap (\(fileA,fileB) -> [fileA,fileB])

testfileDie file = do
 exists <- testfile file
 if   exists
 then return()
 else die$ format ("file doesn't exist: "%fp) file

echoEdiffFiles
 = traverse echo
 . map (uncurry ediff_files)
 . bimapPair fp2txt fp2txt

-- (ediff-files FILE-A FILE-B &optional STARTUP-HOOKS)
ediff_files fileA fileB = format ("(ediff-files "%s%" "%s%")") (repr fileA) (repr fileB)
 -- show the strings for double quotes
 -- escaped double quotes: format ("(ediff-files \""%s%"\" \""%s%"\")") (fileA) (fileB)

filePairs =
 ["sources/Commands/Plugins/Example.hs"-: "config/Commands/Plugins/Spiros.hs"

 ,"sources/Commands/Plugins/Example/Root.hs"-: "config/Commands/Plugins/Spiros/Root.hs"
 ,"sources/Commands/Plugins/Example/Phrase.hs"-:"config/Commands/Plugins/Spiros/Phrase.hs"
 ,"sources/Commands/Plugins/Example/Shortcut.hs"-: "config/Commands/Plugins/Spiros/Shortcut.hs"

 ,"sources/Commands/Frontends/Dragon13/Shim.hs"-: "config/Commands/Plugins/Spiros/Shim.hs"
-- ,""-: ""
 ]

