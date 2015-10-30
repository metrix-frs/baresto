module Api.Schema.BusinessData.Key
( Key(..)
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
  | KeyPlainFact       CellId
  | KeySubsetZFact     CellId AxisId SubsetMemberId
  | KeyCustomZFact     CellId AxisId CustomMemberId
  | KeyCustomYFact     CellId AxisId CustomMemberId
  | KeyCustomYZFact    CellId AxisId CustomMemberId AxisId CustomMemberId
  | KeyMemberYFact     CellId AxisId CustomMemberId
  | KeyMemberYZFact    CellId AxisId CustomMemberId AxisId CustomMemberId
  | KeyCustomYOrdinate        AxisId CustomMemberId
  | KeyCustomZMember          AxisId CustomMemberId
  | KeySubsetZSelected        AxisId SubsetMemberId

-- Show

instance showKey :: Show Key where
  show (KeyHeaderFact e)               = "a" <> showCell e
  show (KeyPlainFact e)                = "b" <> showCell e
  show (KeySubsetZFact e a s)          = "c" <> showCell e <> showAxis a  <> showSubsetM s
  show (KeyCustomZFact e a c)          = "d" <> showCell e <> showAxis a  <> showCustomM c
  show (KeyCustomYFact e a c)          = "e" <> showCell e <> showAxis a  <> showCustomM c
  show (KeyCustomYZFact e ay cy az cz) = "f" <> showCell e <> showAxis ay <> showCustomM cy <> showAxis az <> showCustomM cz
  show (KeyMemberYFact e a c)          = "g" <> showCell e <> showAxis a  <> showCustomM c
  show (KeyMemberYZFact e ay cy az cz) = "h" <> showCell e <> showAxis ay <> showCustomM cy <> showAxis az <> showCustomM cz
  show (KeyCustomYOrdinate a c)        = "i" <>               showAxis a  <> showCustomM c
  show (KeyCustomZMember a c)          = "j" <>               showAxis a  <> showCustomM c
  show (KeySubsetZSelected a s)        = "k" <>               showAxis a  <> showSubsetM s

showCell :: CellId -> String
showCell c = "l" <> show c

showAxis :: AxisId -> String
showAxis a = "m" <> show a

showCustomM :: CustomMemberId -> String
showCustomM cm = "n<" <> cm <> ">"

showSubsetM :: SubsetMemberId -> String
showSubsetM sm = "o" <> show sm

-- Eq

instance eqKey :: Eq Key where
  eq = eq `on` show

-- Ord

instance ordKey :: Ord Key where
  compare = compare `on` show

-- Read

parseKeyF :: String -> F Key
parseKeyF str = case runParser pKey str of
  Left err -> Left $ JSONError $ "Failed parsing key: " <> show err
  Right a -> Right a

pKey :: Parser Key
pKey =
      string "a" *> (KeyHeaderFact      <$> pCellId)
  <|> string "b" *> (KeyPlainFact       <$> pCellId)
  <|> string "c" *> (KeySubsetZFact     <$> pCellId <*> pAxisId <*> pSubsetMemberId)
  <|> string "d" *> (KeyCustomZFact     <$> pCellId <*> pAxisId <*> pCustomMemberId)
  <|> string "e" *> (KeyCustomYFact     <$> pCellId <*> pAxisId <*> pCustomMemberId)
  <|> string "f" *> (KeyCustomYZFact    <$> pCellId <*> pAxisId <*> pCustomMemberId <*> pAxisId <*> pCustomMemberId)
  <|> string "g" *> (KeyMemberYFact     <$> pCellId <*> pAxisId <*> pCustomMemberId)
  <|> string "h" *> (KeyMemberYZFact    <$> pCellId <*> pAxisId <*> pCustomMemberId <*> pAxisId <*> pCustomMemberId)
  <|> string "i" *> (KeyCustomYOrdinate <$> pAxisId <*> pCustomMemberId)
  <|> string "j" *> (KeyCustomZMember   <$> pAxisId <*> pCustomMemberId)
  <|> string "k" *> (KeySubsetZSelected <$> pAxisId <*> pSubsetMemberId)
  <?> "Key"

pCellId :: Parser CellId
pCellId = string "l" *> pInt

pAxisId :: Parser AxisId
pAxisId = string "m" *> pInt

pCustomMemberId :: Parser CustomMemberId
pCustomMemberId = string "n" *> (between (string "<") (string ">") pHex)

pSubsetMemberId :: Parser SubsetMemberId
pSubsetMemberId = string "o" *> pInt

-- Generic parse stuff

pDigit :: Parser Int
pDigit = (string "0" *> pure 0)
    <|> (string "1" *> pure 1)
    <|> (string "2" *> pure 2)
    <|> (string "3" *> pure 3)
    <|> (string "4" *> pure 4)
    <|> (string "5" *> pure 5)
    <|> (string "6" *> pure 6)
    <|> (string "7" *> pure 7)
    <|> (string "8" *> pure 8)
    <|> (string "9" *> pure 9)

pHex :: Parser String
pHex = fromCharArray <<< fromList <$> (many1 $ oneOf $ toCharArray "0123456789abcdef")

pInt :: Parser Int
pInt = foldl addDigit 0 <$> many1 pDigit
  where addDigit num d = 10 * num + d
