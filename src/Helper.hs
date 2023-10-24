{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Helper where

import Control.Lens.Internal.FieldTH (LensRules (_fieldToDef))
import Control.Lens.TH
import Control.Monad.Reader (asks, liftIO)
import Data.Functor (void)
import Data.Text (Text, pack)
import Language.Haskell.TH (DecsQ)
import Language.Haskell.TH.Syntax (Name, mkName, nameBase)
import UnliftIO (MVar, withMVar, writeChan)

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
handleRestCallError res = runUT res >>= either (writeLog . pack . show) pure

restCall' :: (Request (r a), FromJSON a) => r a -> (DiscordHandler :>: Either RestCallErrorCode) a
restCall' = (:>:) . restCall

missingImplementationResponse :: InteractionId -> InteractionToken -> a -> DiscordHandler a
missingImplementationResponse iId iToken state =
  fmap (const state)
    . handleRestCallError
    . restCall'
    . CreateInteractionResponse iId iToken
    . InteractionResponseChannelMessage
    $ InteractionResponseMessage (Just False) (Just "**Implementation error for command**") Nothing Nothing Nothing Nothing Nothing

makeExternalLens :: Name -> DecsQ
makeExternalLens =
  makeLensesWith $
    lensRules
      { _fieldToDef = \_ _ n ->
          case nameBase n of
            x : xs -> [TopName . mkName $ x : xs]
            _ -> []
      }

handleChatInput ::
  MVar State ->
  InteractionId ->
  InteractionToken ->
  UserId ->
  (InteractionId -> InteractionToken -> UserState -> DiscordHandler UserState) ->
  DiscordHandler ()
handleChatInput state iId iToken uId handler =
  void . withMVar state $ adjustF'' (handler iId iToken) uId
