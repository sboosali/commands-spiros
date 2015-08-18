#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
import Turtle
-- runhaskell ./update.hs
-- cabal install --user turtle
-- https://hackage.haskell.org/package/turtle
shell_ cmd      = shell cmd      empty
proc_  cmd args = proc  cmd args empty
rsync      args = proc_ "rsync" args

__file__ = "update.hs"
idir = "/Users/sboosalis/commands-core"

main = do
 chmod rwx __file__
 mktree ("./sources/Commands/Plugins/Example")
 rsync [ "-av" --, "--dry-run"
       , idir<>"/sources/Commands/Plugins/Example"
       ,        "sources/Commands/Plugins/Example" ]
 mktree ("sources/Commands/Frontends/Dragon13/")
 rsync [ "-av" --, "--dry-run"
       , idir<>"/sources/Commands/Frontends/Dragon13/Shim.hs"
       ,        "sources/Commands/Frontends/Dragon13/Shim.hs" ]
 view$ find chars "sources"
