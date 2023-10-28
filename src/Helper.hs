{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Helper where

import Control.Monad.Reader (asks, liftIO)
import Data.Functor (void)
import Data.Text (Text, pack)
import UnliftIO (MVar, modifyMVar_, writeChan)

import Discord
import Discord.Handle
import Discord.Interactions
import Discord.Requests
import Discord.Types

import Joint
import State (State, UserState, adjustF'')

writeLog :: Text -> DiscordHandler ()
writeLog msg = asks (writeChan . discordHandleLog) <*> pure msg >>= liftIO

handleRestCallError :: (DiscordHandler :>: Either RestCallErrorCode) () -> DiscordHandler ()
handleRestCallError = handleRestCallError' ()
handleRestCallError' :: a -> (DiscordHandler :>: Either RestCallErrorCode) a -> DiscordHandler a
handleRestCallError' a res = runUT res >>= either (fmap (const a) . writeLog . pack . show) pure

restCall' :: (Request (r a), FromJSON a) => r a -> (DiscordHandler :>: Either RestCallErrorCode) a
restCall' = (:>:) . restCall

missingImplementationResponse :: InteractionId -> InteractionToken -> a -> DiscordHandler a
missingImplementationResponse = errorResponse "**Implementation error for command**"
errorResponse :: Text -> InteractionId -> InteractionToken -> a -> DiscordHandler a
errorResponse msg iId iToken state = fmap (const state) . handleRestCallError $ statusResponse msg iId iToken
statusResponse :: Text -> InteractionId -> InteractionToken -> (DiscordHandler :>: Either RestCallErrorCode) ()
statusResponse msg iId iToken =
  restCall'
    . CreateInteractionResponse iId iToken
    . InteractionResponseChannelMessage
    $ InteractionResponseMessage
      (pure False)
      (pure msg)
      Nothing
      Nothing
      (pure . InteractionResponseMessageFlags . pure $ InteractionResponseMessageFlagEphermeral)
      Nothing
      Nothing


handleChatInput ::
  MVar State ->
  InteractionId ->
  InteractionToken ->
  UserId ->
  (InteractionId -> InteractionToken -> UserState -> DiscordHandler UserState) ->
  DiscordHandler ()
handleChatInput state iId iToken uId handler =
  void . modifyMVar_ state $ adjustF'' (handler iId iToken) uId
