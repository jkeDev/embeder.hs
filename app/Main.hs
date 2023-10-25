{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.IO as TIO
import Lib

main :: IO ()
main = TIO.readFile ".discord-token" >>= embeder
