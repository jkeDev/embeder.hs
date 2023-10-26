{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Commands where

import Control.Lens as L
import Control.Monad (join)
import Data.Functor (($>))
import Data.Map as M
import Data.Maybe (fromMaybe)

import Discord
import Discord.Interactions
import Discord.Requests
import Discord.Types

import Helper
import State

-- TODO: handle missing stuff differently then to just assume the default

create :: Maybe ResolvedData -> Maybe OptionsData -> InteractionId -> InteractionToken -> UserState -> DiscordHandler UserState
create _ (Just (OptionsDataValues [OptionDataValueString "base" (Right base), OptionDataValueString "name" (Right name)])) =
  \iId iToken ->
    L.traverseOf
      userEmbedTemplates
      ( \ts ->
          let t = fromMaybe def $ M.lookup base ts
           in spawn' iId iToken t () $> M.insert name t ts
      )
create _ _ = missingImplementationResponse

spawn :: Maybe ResolvedData -> Maybe OptionsData -> InteractionId -> InteractionToken -> UserState -> DiscordHandler UserState
-- spawn = undefined
spawn _ (Just (OptionsDataValues [OptionDataValueString "name" (Right name)])) =
  \iId iToken -> join $ spawn' iId iToken . fromMaybe def . M.lookup name . L.view userEmbedTemplates
spawn _ _ = missingImplementationResponse

spawn' :: InteractionId -> InteractionToken -> CreateEmbed -> a -> DiscordHandler a
spawn' iId iToken embed x =
  fmap (const x) . handleRestCallError . restCall' . CreateInteractionResponse iId iToken . InteractionResponseChannelMessage $
    InteractionResponseMessage (pure False) Nothing (pure . pure $ embed) Nothing Nothing Nothing Nothing

set :: Maybe ResolvedData -> Maybe OptionsData -> InteractionId -> InteractionToken -> UserState -> DiscordHandler UserState
set _ (Just (OptionsDataValues [OptionDataValueString "changes" (Right changes)])) =
  missingImplementationResponse
set _ _ = missingImplementationResponse
