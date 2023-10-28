{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Parsing (
  parseSetArgs,
  SetArgs,
  addFields,
  removeFields,
  setValues,
  parseReplace,
) where

import Control.Lens (Bifunctor (bimap))
import Data.Bool.HT (if')
import Data.Either.HT (mapLeft)
import Data.Foldable (fold)
import Data.Function (on)
import Data.Functor (($>))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, singleton)
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P

data SetArgs = SetArgs
  { addFields :: [Text]
  , removeFields :: [Text]
  , setValues :: [(Text, Text)]
  }
data SetArg
  = Assignment Bool String String
  | FieldChange FieldChange String
data FieldChange = AddField | DelField

instance Semigroup SetArgs where
  SetArgs add1 del1 val1 <> SetArgs add2 del2 val2 =
    SetArgs (add1 <> add2) (del1 <> del2) (val1 <> val2)
instance Monoid SetArgs where mempty = SetArgs [] [] []

parseSetArgs :: String -> Either Text SetArgs
parseSetArgs = mapLeft (pack . show) . parse (chainl1 setArg (pure (<>)) <* eof) "message"

type Parser = Parsec String ()

quotedStr :: Parser String
reservedOp :: String -> Parser ()
identifier :: Parser String
P.TokenParser
  { P.stringLiteral = quotedStr
  , P.reservedOp = reservedOp
  , P.identifier = identifier
  } =
    P.makeTokenParser $
      emptyDef
        { P.reservedOpNames = ["+", "-", "="]
        }

setArg :: Parser SetArgs
setArg =
  ( \case
      Assignment isNew key val -> SetArgs (if' isNew [pack key] []) [] [(pack key, pack val)]
      FieldChange AddField key -> SetArgs [pack key] [] []
      FieldChange DelField key -> SetArgs [] [pack key] []
  )
    <$> (assignment <|> fieldChange)

assignment :: Parser SetArg
assignment = Assignment <$> mode <*> stringParser <* reservedOp "=" <*> val
 where
  mode = option False $ reservedOp "+" $> True
  val = option "" stringParser

fieldChange :: Parser SetArg
fieldChange = FieldChange <$> mode <*> stringParser
 where
  mode = reservedOp "+" $> AddField <|> reservedOp "-" $> DelField

stringParser :: Parser String
stringParser = identifier <|> quotedStr

data Text'
  = LiteralText Text
  | Placeholder Text

unText' :: M.Map Text Text -> Text' -> Text
unText' _ (LiteralText text) = text
unText' src (Placeholder name) = fromMaybe "" $ M.lookup name src

parseReplace :: M.Map Text Text -> String -> Either Text Text
parseReplace src = bimap (pack . show) (foldMap $ unText' src) . parse (many (placeholder <|> LiteralText <$> literalText) <* eof) "field"

placeholder :: Parser Text'
placeholder = char '<' *> (Placeholder <$> literalText) <* char '>'

literalText :: Parser Text
literalText =
  fmap fold . many1 $
    try (on (<>) singleton <$> space <*> char '>')
      <|> try (singleton <$> noneOf "<>")
      <|> try (on (<>) singleton <$> char '<' <*> space)
