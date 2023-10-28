{-# LANGUAGE TemplateHaskell #-}

module Lenses where

import Discord.Types as D
import TH

$(makeExternalLens ''D.CreateEmbed)
$(makeExternalLens ''D.EmbedField)
