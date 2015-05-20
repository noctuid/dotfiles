{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module MyAntigen where

import Antigen (
                -- Rudimentary imports
                AntigenConfiguration (..)
              , bundle
              , antigen
                -- If you want to source a bit trickier plugins
              , ZshPlugin (..)
              , antigenSourcingStrategy
              , filePathsSourcingStrategy
              )
import Shelly (shelly)

bundles =
  [ bundle "Tarrasch/zsh-command-not-found"
  , bundle "Tarrasch/zsh-bd"
  , bundle "zsh-users/zsh-syntax-highlighting"
  , bundle "zsh-users/zsh-history-substring-search"
  
  -- , bundle "hchbaw/opp.zsh"
  -- , bundle "Tarrasch/zsh-autoenv"
  , (bundle "robbyrussell/oh-my-zsh")
     { sourcingStrategy = filePathsSourcingStrategy
                           [ "lib/theme-and-appearance.zsh"
                           , "lib/git.zsh"
                           , "lib/completion.zsh"
                           ] }
  ]

config = AntigenConfiguration bundles

main :: IO ()
main = shelly $ antigen config
