{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Commands where

import Control.Lens as L
import Data.Map as M
import Data.Maybe (fromMaybe)

import Discord
import Discord.Interactions
import Discord.Types

import Helper
import State
import Control.Monad (join)

create :: Maybe ResolvedData -> Maybe OptionsData -> InteractionId -> InteractionToken -> UserState -> DiscordHandler UserState
create _ (Just (OptionsDataValues [OptionDataValueString "base" (Right base), OptionDataValueString "name" (Right name)])) =
  \_ _ -> pure . L.over userEmbedTemplates (join $ M.insert name . fromMaybe def . M.lookup base)
create _ _ = missingImplementationResponse

spawn :: Maybe ResolvedData -> Maybe OptionsData -> InteractionId -> InteractionToken -> UserState -> DiscordHandler UserState
-- spawn = undefined
spawn _ _ = missingImplementationResponse

set :: Maybe ResolvedData -> Maybe OptionsData -> InteractionId -> InteractionToken -> UserState -> DiscordHandler UserState
--set = undefined
set _ _ = missingImplementationResponse
