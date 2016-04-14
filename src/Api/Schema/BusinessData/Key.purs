module Api.Schema.BusinessData.Key
( Key(..)
, IsRowKey(..)
, YLocation(..)
, ZLocation(..)
, parseKeyF
) where

import Prelude (class Ord, class Eq, class Show, (<$>), (+), (*), ($), (<<<), pure, (<*>), show, (<>), compare, eq)

import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))

import Data.Function (on)
import Data.Either (Either(Right, Left))
import Data.List (fromList)
import Data.String (fromCharArray, toCharArray)
import Data.Foreign (F, ForeignError(JSONError))
import Data.Foldable (foldl)

import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.Combinators (many1, (<?>))
import Text.Parsing.StringParser.String (oneOf, string)

import Types (OrdinateId, SubsetMemberId, CustomMemberId, AxisId, CellId)

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
  = ZLocSingle
  | ZLocClosed OrdinateId
  | ZLocCustom AxisId CustomMemberId
  | ZLocSubset AxisId SubsetMemberId

instance showKey :: Show Key where
  show (KeyHeaderFact c)        = "a" <> showCell c
  show (KeyFact c r y z)        = "b" <> showCell c <> show r   <> show y <> show z
  show (KeySubsetZSelected a s) = "c" <> showAxis a <> showSM s
  show (KeyCustomZMember a c)   = "d" <> showAxis a <> showCM c
  show (KeyCustomRow a z c)     = "e" <> showAxis a <> show z   <> showCM c

instance showIsRowKey :: Show IsRowKey where
  show (RowKey)                 = "f"
  show (NoRowKey)               = "g"

instance showYLocation :: Show YLocation where
  show (YLocClosed)             = "h"
  show (YLocCustom a c)         = "i" <> showAxis a <> showCM c

instance showZLocation :: Show ZLocation where
  show (ZLocSingle)             = "j"
  show (ZLocClosed o)           = "k" <> showOrd o
  show (ZLocCustom a c)         = "l" <> showAxis a <> showCM c
  show (ZLocSubset a s)         = "m" <> showAxis a <> showSM s

showCell :: CellId -> String
showCell c                      = "n" <> show c

showAxis :: AxisId -> String
showAxis a                      = "o" <> show a

showCM :: CustomMemberId -> String
showCM c                        = "p<" <> c <> ">"

showSM :: SubsetMemberId -> String
showSM s                        = "q" <> show s

showOrd :: OrdinateId -> String
showOrd o                       = "r" <> show o

-- Eq

instance eqKey :: Eq Key where
  eq = eq `on` show

instance eqZLocation :: Eq ZLocation where
  eq = eq `on` show

-- Ord

instance ordKey :: Ord Key where
  compare = compare `on` show

instance ordZLocation :: Ord ZLocation where
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
      string "j" *> pure ZLocSingle
  <|> string "k" *> (ZLocClosed <$> ordinateId)
  <|> string "l" *> (ZLocCustom <$> axisId <*> customMemberId)
  <|> string "m" *> (ZLocSubset <$> axisId <*> subsetMemberId)
  <?> "ZLocation"

cellId :: Parser CellId
cellId =
      string "n" *> integer

axisId :: Parser AxisId
axisId =
      string "o" *> integer

customMemberId :: Parser CustomMemberId
customMemberId =
      string "p" *> string "<" *> hex <* string ">"

subsetMemberId :: Parser SubsetMemberId
subsetMemberId =
      string "q" *> integer

ordinateId :: Parser OrdinateId
ordinateId =
      string "r" *> integer

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
