module Utils
  ( maxInt
  , cls
  , readId
  , makeIndexed
  , getIndices
  , minOrd
  , maxOrd
  , getEntropy
  , createEvent
  , createCustomEvent
  , customEventDetail
  , shorten
  ) where

import Prelude
import Global
import qualified Math as Math

import Data.Int hiding (round)
import Data.Either
import Data.Maybe
import Data.Tuple
import Data.Array hiding (take)
import Data.String (take)
import Data.Map (Map())
import Data.Nullable
import Data.String (toCharArray, fromCharArray)

import DOM.Event.Types (Event(), EventType(..))

import Control.Monad.Eff.Random

import Control.Monad.Eff
import Control.Bind
import Control.Alt ((<|>))

import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Core as H


maxInt :: Int -> Int -> Int
maxInt x y = fromMaybe 0 $ fromNumber $ Math.max (toNumber x) (toNumber y)

cls :: forall r i. String -> P.IProp (class :: P.I | r) i
cls = P.class_ <<< H.className

readId :: String -> Int
readId str = case fromNumber (readInt 10 str) of
  Nothing -> 0
  Just x  -> x

makeIndexed :: forall a. Array a -> Array (Tuple Int a)
makeIndexed xs = zip (0 .. length xs) xs

getIndices :: forall a. Array a -> Array Int
getIndices xs = map fst $ makeIndexed xs

minOrd :: forall a. (Ord a) => a -> a -> a
minOrd x y = case compare x y of
  LT -> x
  _ -> y

maxOrd :: forall a. (Ord a) => a -> a -> a
maxOrd x y = case compare x y of
  GT -> x
  _ -> y

-- | Collect n/2 bytes of entropy using JS's `Math.random()`
-- and return in hexadecimal form.
getEntropy :: forall e. Int -> Eff (random :: RANDOM | e) String
getEntropy n = fromCharArray <$> replicateM n do
    i <- randomInt 0 15
    return $ fromMaybe '0' $ alphabet !! i
  where
    alphabet = toCharArray "0123456789abcdef"

foreign import createEventImpl :: String -> Event

createEvent :: EventType -> Event
createEvent (EventType typ) = createEventImpl typ

foreign import createCustomEventImpl :: String -> String -> Event

createCustomEvent :: EventType -> String -> Event
createCustomEvent (EventType typ) msg = createCustomEventImpl typ msg

foreign import customEventDetailImpl :: Event -> Nullable String

customEventDetail :: Event -> Maybe String
customEventDetail = toMaybe <<< customEventDetailImpl

shorten :: String -> Int -> Maybe String
shorten s len = let short = take len s in
  if s == short then Nothing
                else Just short
