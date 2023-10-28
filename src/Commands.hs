{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Commands where

import qualified Control.Lens as L
import Data.Foldable (foldl')
import Data.Functor (($>))
import qualified Data.Map as M
import Data.Text (pack, unpack)
import Data.Tuple.Extra ((&&&))

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A
import qualified Data.Aeson.KeyMap as A
import Discord
import Discord.Interactions
import Discord.Requests
import Discord.Types

import Data.Aeson (fromJSON)
import Data.Maybe (fromMaybe, listToMaybe)
import Helper
import Joint
import Lenses as DL
import Parsing
import State

-- TODO: handle missing stuff differently then to just assume the default

-- TODO: remove create command as it should be replaced with 'save as template'
saveTemplate :: Maybe ResolvedData -> MessageId -> InteractionId -> InteractionToken -> UserState -> DiscordHandler UserState
{-
saveTemplate (Just ResolvedData{resolvedDataMessages = Just (A.Object rawMessages)}) mId iId iToken state =
  A.lookup (A.fromString . show $ mId) rawMessages
    >>= A.decode . A.encode
    >>= listToMaybe
-}
saveTemplate _ _ iId iToken state = missingImplementationResponse iId iToken state

spawn :: Maybe ResolvedData -> Maybe OptionsData -> InteractionId -> InteractionToken -> UserState -> DiscordHandler UserState
-- spawn = undefined
spawn
  _
  (Just (OptionsDataValues [OptionDataValueString "name" (Right name), OptionDataValueString "id" (Right tiId)]))
  iId
  iToken
  state = handleRestCallError' state $ L.traverseOf userEmbedInstances (M.alterF (const $ pure <$> instance') tiId) state
   where
    instance' = spawnInstance *> getMessageRef >>= \mRef -> lift $ TemplateInstance mRef def <$> template'
    instance'' = makeInstance' def <$> template'
    template' :: Either RestCallErrorCode CreateEmbed =
      maybe (Left $ RestCallErrorCode 404 "Template does not exist" "") Right
        . M.lookup name
        . L.view userEmbedTemplates
        $ state
    spawnInstance = lift instance'' >>= spawn' iId iToken
    getApplicationId = fullApplicationID <$> restCall' GetCurrentApplication
    getMessageRef = (messageChannelId &&& messageId) <$> getMessage
    getMessage = getApplicationId >>= \appId -> restCall' $ GetOriginalInteractionResponse appId iToken
spawn _ _ iId iToken state = missingImplementationResponse iId iToken state

spawn' :: InteractionId -> InteractionToken -> CreateEmbed -> (DiscordHandler :>: Either RestCallErrorCode) ()
spawn' iId iToken embed =
  restCall' . CreateInteractionResponse iId iToken . InteractionResponseChannelMessage $
    InteractionResponseMessage (pure False) Nothing (pure . pure $ embed) Nothing Nothing Nothing Nothing

set :: Maybe ResolvedData -> Maybe OptionsData -> InteractionId -> InteractionToken -> UserState -> DiscordHandler UserState
set
  _
  (Just (OptionsDataValues [OptionDataValueString "id" (Right tiId), OptionDataValueString "changes" (Right changes)]))
  iId
  iToken
  state = either handleError handleSuccess $ (parseSetArgs . unpack) changes >>= updateInstance
   where
    updateInstance args = L.traverseOf userEmbedInstances (updateInstance' args) state
    updateInstance' args = M.alterF (maybe (Left "instance does not exist") (Right . pure . set' args)) tiId
    handleError msg = errorResponse msg iId iToken state
    handleSuccess state' = handleRestCallError (updateMessage state' *> sendUserReply) $> state'
    updateMessage state' = restCall' . EditMessage (L.view templateRef ti) $ def{messageDetailedEmbeds = Just [makeInstance ti]}
     where
      (Just ti) = M.lookup tiId $ L.view userEmbedInstances state'
    sendUserReply = statusResponse "Updated" iId iToken
set _ _ iId iToken state = missingImplementationResponse iId iToken state

set' :: SetArgs -> TemplateInstance -> TemplateInstance
set' args = mergeAddedFields . mergeDeletedFields . mergeValues
 where
  mergeAddedFields = L.over (template . DL.createEmbedFields) (<> addedFields')
  addedFields' :: [EmbedField] = (\name -> EmbedField name ("<" <> name <> ">") $ Just False) <$> addFields args
  mergeDeletedFields = L.over (template . DL.createEmbedFields) . removeFields' $ removeFields args
  removeFields' = flip . foldl' $ \xs remove -> filter (isFieldName remove) xs
  isFieldName name (EmbedField name' _ _) = name == name'
  mergeValues = L.over instanceValues (\vs -> foldl' mergeValues' vs $ setValues args)
  mergeValues' vs (k, v) = M.insert k v vs
