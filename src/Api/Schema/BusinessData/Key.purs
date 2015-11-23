module Api.Schema.BusinessData.Key
( Key(..)
, IsRowKey(..)
, YLocation(..)
, ZLocation(..)
, parseKeyF
) where

import Prelude

import Data.Function
import Data.Tuple
import Data.Tuple.Nested
import Data.Either
import Data.List (fromList)
import Data.String (fromCharArray, toCharArray)
import Data.Foreign
import Data.Foldable (foldl)

import Control.Alt
import Control.Apply

import Text.Parsing.StringParser
import Text.Parsing.StringParser.Combinators
import Text.Parsing.StringParser.String

import Types

data Key
  = KeyHeaderFact      CellId
  | KeyFact            CellId IsRowKey        YLocation ZLocation
  | KeySubsetZSelected                 AxisId                     SubsetMemberId
  | KeyCustomZMember                   AxisId                     CustomMemberId
  | KeyCustomRow                       AxisId           ZLocation CustomMemberId -- value: order

data IsRowKey
  = RowKey
  | NoRowKey

data YLocation
  = YLocClosed
  | YLocCustom AxisId CustomMemberId

data ZLocation
  = ZLocClosed
  | ZLocCustom AxisId CustomMemberId
  | ZLocSubset AxisId SubsetMemberId

instance showKey :: Show Key where
  show (KeyHeaderFact c)        = "a" <> showCell c
  show (KeyFact c r y z)        = "b" <> showCell c <> show r   <> show y <> show z
  show (KeySubsetZSelected a s) = "c" <> showAxis a <> showSM s
  show (KeyCustomZMember a c)   = "d" <> showAxis a <> showCM c
  show (KeyCustomRow a z c)     = "e" <> showAxis a <> show z   <> showCM c

instance showIsRowKey :: Show IsRowKey where
  show RowKey                   = "f"
  show NoRowKey                 = "g"

instance showYLocation :: Show YLocation where
  show YLocClosed               = "h"
  show (YLocCustom a c)         = "i" <> showAxis a <> showCM c

instance showZLocation :: Show ZLocation where
  show ZLocClosed               = "j"
  show (ZLocCustom a c)         = "k" <> showAxis a <> showCM c
  show (ZLocSubset a s)         = "l" <> showAxis a <> showSM s

showCell :: CellId -> String
showCell c                      = "m" <> show c

showAxis :: AxisId -> String
showAxis a                      = "n" <> show a

showCM :: CustomMemberId -> String
showCM c                        = "o<" <> c <> ">"

showSM :: SubsetMemberId -> String
showSM s                        = "p" <> show s

-- Eq

instance eqKey :: Eq Key where
  eq = eq `on` show

-- Ord

instance ordKey :: Ord Key where
  compare = compare `on` show

-- Read

parseKeyF :: String -> F Key
parseKeyF str = case runParser key str of
  Left err -> Left $ JSONError $ "Failed parsing key: " <> show err
  Right a -> Right a

key :: Parser Key
key =
      string "a" *> (KeyHeaderFact      <$> cellId)
  <|> string "b" *> (KeyFact            <$> cellId <*> rowKey <*> yLocation <*> zLocation)
  <|> string "c" *> (KeySubsetZSelected <$> axisId <*> subsetMemberId)
  <|> string "d" *> (KeyCustomZMember   <$> axisId <*> customMemberId)
  <|> string "e" *> (KeyCustomRow       <$> axisId <*> zLocation <*> customMemberId)
  <?> "Key"

rowKey :: Parser IsRowKey
rowKey =
      string "f" *> pure RowKey
  <|> string "g" *> pure NoRowKey
  <?> "RowKey"

yLocation :: Parser YLocation
yLocation =
      string "h" *> pure YLocClosed
  <|> string "i" *> (YLocCustom <$> axisId <*> customMemberId)
  <?> "YLocation"

zLocation :: Parser ZLocation
zLocation =
      string "j" *> pure ZLocClosed
  <|> string "k" *> (ZLocCustom <$> axisId <*> customMemberId)
  <|> string "l" *> (ZLocSubset <$> axisId <*> subsetMemberId)
  <?> "ZLocation"

cellId :: Parser CellId
cellId =
      string "m" *> integer

axisId :: Parser AxisId
axisId =
      string "n" *> integer

customMemberId :: Parser CustomMemberId
customMemberId =
      string "o" *> string "<" *> hex <* string ">"

subsetMemberId :: Parser SubsetMemberId
subsetMemberId =
      string "p" *> integer

-- Generic parse stuff

pDigit :: Parser Int
pDigit =
        (string "0" *> pure 0)
    <|> (string "1" *> pure 1)
    <|> (string "2" *> pure 2)
    <|> (string "3" *> pure 3)
    <|> (string "4" *> pure 4)
    <|> (string "5" *> pure 5)
    <|> (string "6" *> pure 6)
    <|> (string "7" *> pure 7)
    <|> (string "8" *> pure 8)
    <|> (string "9" *> pure 9)

hex :: Parser String
hex = fromCharArray <<< fromList <$> (many1 $ oneOf $ toCharArray "0123456789abcdef")

integer :: Parser Int
integer = foldl addDigit 0 <$> many1 pDigit
  where addDigit num d = 10 * num + d
