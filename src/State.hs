{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module State where

import Control.Lens
import Data.Composition ((.:))
import Data.Default (Default, def)
import Data.FileEmbed (embedFile)
import Data.Functor (($>))
import Data.Map (Map, alter, alterF, fromList)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Text.IO as TIO
import Discord.Internal.Rest
import UnliftIO (IOException, MVar, newMVar, readMVar, try)

type State = Map UserId UserState
data UserState = UserState
  { _userEmbedTemplates :: Map Text CreateEmbed
  , _userEmbedInstances :: Map Integer MessageId
  }
  deriving (Show, Read)

$(makeLenses ''UserState)

missingTexture :: CreateEmbedImage
missingTexture = CreateEmbedImageUpload $(embedFile "missing-texture.png")
instance Default UserState where
  def =
    UserState
      ( fromList
          [
            ( "full"
            , CreateEmbed
                "<author>"
                "https://example.com/"
                (Just missingTexture)
                "<title>"
                "https://example.com/"
                (Just missingTexture)
                "<desc>"
                []
                (Just missingTexture)
                "<footer>"
                (Just missingTexture)
                (Just DiscordColorDiscordGreen)
                Nothing
            )
          , ("base", def{createEmbedTitle = "<title>", createEmbedDescription = "<desc>"})
          , ("empty", def)
          ]
      )
      def

loadState :: IO (MVar State)
loadState =
  try (read . unpack <$> TIO.readFile "./globalState")
    >>= \case
      Right file -> TIO.putStrLn "Loaded state" $> file
      Left (_ :: IOException) -> TIO.putStrLn "State not found. Create new state" $> def
    >>= newMVar

-- FIXME: don't save the default templates if they havn't been overriden
saveState :: MVar State -> IO ()
saveState state = readMVar state >>= TIO.writeFile "./globalState" . pack . show

adjust' :: Ord k => (v -> v) -> v -> k -> Map k v -> Map k v
adjust' f = alter . (pure .: f .: fromMaybe)

adjust'' :: (Ord k, Default v) => (v -> v) -> k -> Map k v -> Map k v
adjust'' = flip adjust' def

adjustF' :: (Ord k, Functor f) => (v -> f v) -> v -> k -> Map k v -> f (Map k v)
adjustF' f = alterF . (fmap pure .: f .: fromMaybe)

adjustF'' :: (Ord k, Default v, Functor f) => (v -> f v) -> k -> Map k v -> f (Map k v)
adjustF'' = flip adjustF' def
