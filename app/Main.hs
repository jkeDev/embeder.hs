{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.FileEmbed (embedStringFile)
import Lib

main :: IO ()
main = embeder $(embedStringFile ".discord-token")
