{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module Text.Html.Email.Validate
    ( -- * Validating
      isValidEmail
      -- * Parsing
    , EmailAddress(..)
    , emailToText
    , parseEmail
    , emailParser
    ) where

import           Control.Applicative
import           Control.Monad        (when)
import           Data.Attoparsec.Text
import           Data.Data            (Data, Typeable)
import           Data.Either          (isRight)
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics         (Generic)
import qualified Text.Read            as Read

-- | Represents an email address
data EmailAddress = EmailAddress
  { localPart  :: Text
  , domainPart :: Text
  } deriving (Eq, Ord, Data, Typeable, Generic)

instance Show EmailAddress where
  show = Text.unpack . emailToText

instance Read EmailAddress where
  readListPrec = Read.readListPrecDefault
  readPrec = Read.parens $ do
    text <- Read.readPrec
    case parseEmail text of
      Right email -> pure email
      Left  _err  -> Read.pfail

-- | Convert to text.
--
--   >>> emailToText $ EmailAddress "name" "example.com"
--   "name@example.com
emailToText :: EmailAddress -> Text
emailToText EmailAddress{..} =
  localPart <> Text.singleton '@' <> domainPart

-- | Validates given email. Email shouldn't have trailing or preceding spaces
--
--   >>> :set -XOverloadedStrings
--   >>> isValidEmail "name@example.com"
--   True
--   >>> isValidEmail "name@example..com"
--   False
isValidEmail :: Text -> Bool
isValidEmail = isRight . parseEmail

-- | Parce an email. Error messages aren't very helpful.
parseEmail :: Text -> Either String EmailAddress
parseEmail = parseOnly emailParser

-- | Attoparsec parser.
emailParser :: Parser EmailAddress
emailParser =
  EmailAddress
    <$> (local  <* char '@')
    <*> (domain <* endOfInput)

local :: Parser Text
local =
  takeWhile1 (inClass "A-Za-z0-9!#$%&'*+/=?^_`{|}~.-")

domain :: Parser Text
domain =
  Text.intercalate "." <$> label `sepBy1` char '.'

label :: Parser Text
label = do
  lbl <- Text.intercalate "-" <$> takeWhile1 (inClass "A-Za-z0-9") `sepBy1` char '-'
  when (Text.length lbl > 63) $ fail "Label is too long"
  pure lbl
