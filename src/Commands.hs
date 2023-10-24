{-# LANGUAGE OverloadedStrings #-}

module Commands where

import Control.Lens (over)
import Data.Map as M

import Discord
import Discord.Interactions
import Discord.Types

import Helper
import State

create :: Maybe ResolvedData -> Maybe OptionsData -> InteractionId -> InteractionToken -> UserState -> DiscordHandler UserState
create _ (Just (OptionsDataValues [OptionDataValueString "base" (Right base), OptionDataValueString "name" (Right name)])) =
  undefined
-- TODO: use lenses
-- pure $ state
create _ _ = missingImplementationResponse

spawn :: Maybe ResolvedData -> Maybe OptionsData -> InteractionId -> InteractionToken -> UserState -> DiscordHandler UserState
-- spawn = undefined
spawn _ _ = missingImplementationResponse

set :: Maybe ResolvedData -> Maybe OptionsData -> InteractionId -> InteractionToken -> UserState -> DiscordHandler UserState
--set = undefined
set _ _ = missingImplementationResponse
