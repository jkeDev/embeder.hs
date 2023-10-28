{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module State where

import Control.Applicative (Applicative (liftA2))
import Control.Lens
import Data.Composition ((.:))
import Data.Default (Default, def)
import Data.FileEmbed (embedFile)
import Data.Foldable (foldl')
import Data.Functor (($>))
import Data.Map (Map, alter, alterF, fromList)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Text.IO as TIO
import Discord.Internal.Rest
import qualified Lenses as DL
import Parsing
import UnliftIO (IOException, MVar, newMVar, readMVar, try)

type State = Map UserId UserState
data UserState = UserState
  { _userEmbedTemplates :: Map Text CreateEmbed
  , _userEmbedInstances :: Map Text TemplateInstance
  }
  deriving (Show, Read)
data TemplateInstance = TemplateInstance
  { _templateRef :: (ChannelId, MessageId)
  , _instanceValues :: Map Text Text
  , _template :: CreateEmbed
  }
  deriving (Show, Read)

$(makeLenses ''UserState)
$(makeLenses ''TemplateInstance)

missingTexture :: CreateEmbedImage
-- FIXME: embeds don't work correctly with imageuploads
--missingTexture = CreateEmbedImageUpload $(embedFile "missing-texture.png")
missingTexture = CreateEmbedImageUrl "https://cdn.discordapp.com/attachments/1165396288301449316/1167145452077064264/titlethumbnail.png?ex=654d0fe8&is=653a9ae8&hm=83dfbe6c113a6ccceabf38b100c039d4ef163db5e967faf77b42f8472b9d64e4&"
instance Default UserState where
  def =
    UserState
      ( fromList
          [
            ( "full"
            , CreateEmbed
                "Auth: <author>"
                ""
                (Just missingTexture)
                "Tits: <title>"
                ""
                (Just missingTexture)
                "Desc: <desc>"
                []
                (Just missingTexture)
                "Foot: <footer>"
                (Just missingTexture)
                (Just DiscordColorDiscordGreen)
                Nothing
            )
          , ("base", def{createEmbedTitle = "<title>", createEmbedDescription = "<desc>"})
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

makeInstance :: TemplateInstance -> CreateEmbed
makeInstance = liftA2 makeInstance' (view instanceValues) (view template)
makeInstance' :: Map Text Text -> CreateEmbed -> CreateEmbed
makeInstance' src embed =
  foldl' (&) embed $
    flip over handlePlaceholder
      <$> [ DL.createEmbedAuthorName
          , DL.createEmbedTitle
          , DL.createEmbedDescription
          , DL.createEmbedFields . each . DL.embedFieldValue
          , DL.createEmbedFooterText
          ]
 where
  handlePlaceholder = either id id . parseReplace src . unpack

adjust' :: Ord k => (v -> v) -> v -> k -> Map k v -> Map k v
adjust' f = alter . (pure .: f .: fromMaybe)

adjust'' :: (Ord k, Default v) => (v -> v) -> k -> Map k v -> Map k v
adjust'' = flip adjust' def

adjustF' :: (Ord k, Functor f) => (v -> f v) -> v -> k -> Map k v -> f (Map k v)
adjustF' f = alterF . (fmap pure .: f .: fromMaybe)

adjustF'' :: (Ord k, Default v, Functor f) => (v -> f v) -> k -> Map k v -> f (Map k v)
adjustF'' = flip adjustF' def
