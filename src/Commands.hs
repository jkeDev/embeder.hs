{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Commands where

import Control.Lens as L
import Control.Monad (join)
import Data.Composition ((.:))
import Data.Map as M
import Data.Maybe (fromMaybe)

import Discord
import Discord.Interactions
import Discord.Types

import Helper
import State

create :: Maybe ResolvedData -> Maybe OptionsData -> InteractionId -> InteractionToken -> UserState -> DiscordHandler UserState
create _ (Just (OptionsDataValues [OptionDataValueString "base" (Right base), OptionDataValueString "name" (Right name)])) =
  \_ _ ->
    L.traverseOf userEmbedTemplates $
      join $ pure .: M.insert name . fromMaybe def . M.lookup base
create _ _ = missingImplementationResponse

spawn :: Maybe ResolvedData -> Maybe OptionsData -> InteractionId -> InteractionToken -> UserState -> DiscordHandler UserState
-- spawn = undefined
spawn _ _ = missingImplementationResponse

set :: Maybe ResolvedData -> Maybe OptionsData -> InteractionId -> InteractionToken -> UserState -> DiscordHandler UserState
--set = undefined
set _ _ = missingImplementationResponse
