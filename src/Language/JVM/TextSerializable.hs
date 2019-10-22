{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-|
Module      : Language.JVM.TextSerializable
Copyright   : (c) Christian Gram Kalhauge, 2019
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module can parse and serialize text to structures
-}
module Language.JVM.TextSerializable where


-- template-haskell
import Language.Haskell.TH

-- base
import Data.String

-- attoparsec
import           Data.Attoparsec.Text

-- text
import qualified Data.Text              as Text
import qualified Data.Text.Lazy         as Lazy
import Data.Text.Lazy.Builder           as Builder

-- | A class that indicates that something can be turned from and to
-- text.
class TextSerializable a where
  -- | A `TypeParse` should be parsable
  parseText :: Parser a

  -- | A `TypeParse` should be printable
  toBuilder :: a -> Builder

  -- | Parse a type from text
  deserialize :: Text.Text -> Either String a
  deserialize = deserializeWith parseText

  -- | Print a type from text
  serialize :: a -> Text.Text
  serialize = serializeWith toBuilder

-- | Parse a type from text
deserializeWith :: Parser a -> Text.Text -> Either String a
deserializeWith p = parseOnly (p <* endOfInput)

-- | Print a type from text
serializeWith :: (a -> Builder) -> a -> Text.Text
serializeWith serializer = Lazy.toStrict . Builder.toLazyText . serializer

showViaTextSerializable :: TextSerializable a => a -> String
showViaTextSerializable = show . serialize
{-# INLINE showViaTextSerializable #-}

fromStringViaTextSerializable :: TextSerializable a => String -> a
fromStringViaTextSerializable a =
  case deserialize (Text.pack a) of
    Right a' -> a'
    Left msg -> error $
      "While parsing a fromString instance we got this error message: " <> msg
      <> "Maybe the string " <> show a <> " is wrongly formatted."

{-# INLINE fromStringViaTextSerializable #-}

-- -- | Parse a type from text
-- toLazyText :: TextSerializable a => a -> Lazy.Text
-- toLazyText = Builder.toLazyText . typeToBuilder

deriveFromTextSerializable :: Name -> Q [Dec]
deriveFromTextSerializable name =
  concat <$> sequence
  [ [d|instance Show ($n) where show = showViaTextSerializable |]
  , [d|instance IsString ($n) where fromString = fromStringViaTextSerializable |]
  ] where n = conT name
