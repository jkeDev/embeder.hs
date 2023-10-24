{-# LANGUAGE TemplateHaskell #-}

module Lenses where

import Discord.Types as D

import Helper

$(makeExternalLens ''D.CreateEmbed)
