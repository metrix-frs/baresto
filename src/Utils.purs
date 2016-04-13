module Utils
  ( maxInt
  , cls
  , readId
  , makeIndexed
  , getIndices
  , minOrd
  , maxOrd
  , getEntropy
  , initClipboard
  , createEvent
  , createErrorEvent
  , errorEventDetail
  , shorten
  , peek'
  , getInputFileList
  , Pagination
  , paginate
  ) where

import Prelude
import Global
import Math as Math

import Data.Int hiding (round)
import Data.Either
import Data.Maybe
import Data.Tuple
import Data.Array
import Data.String as Str
import Data.Map (Map())
import Data.Nullable
import Data.String (toCharArray, fromCharArray)

import DOM.Event.Types (Event(), EventType(..))
import DOM (DOM())
import DOM.File.Types (FileList())

import Control.Monad.Eff.Random

import Control.Monad.Eff
import Control.Bind
import Control.Alt ((<|>))

import Halogen
import Halogen.Component.ChildPath (ChildPath(), prjSlot, prjQuery)
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Core as H

import Types


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

foreign import initClipboard :: forall e. String -> Eff (dom :: DOM | e) Unit

foreign import createEventImpl :: String -> Event

createEvent :: EventType -> Event
createEvent (EventType typ) = createEventImpl typ

foreign import createErrorEventImpl :: String -> ErrorDetail -> Event

createErrorEvent :: EventType -> ErrorDetail -> Event
createErrorEvent (EventType typ) detail = createErrorEventImpl typ detail

foreign import errorEventDetailImpl :: Event -> Nullable ErrorDetail

errorEventDetail :: Event -> Maybe ErrorDetail
errorEventDetail = toMaybe <<< errorEventDetailImpl

shorten :: String -> Int -> Maybe String
shorten s len = let short = Str.take len s in
  if s == short then Nothing
                else Just short

-- TODO: purescript-halogen PR
peek' :: forall s s' s'' f f' f'' g p p' a
       . ChildPath s'' s' f'' f' p' p
      -> ChildF p f' a
      -> (p' -> f'' a -> ParentDSL s s' f f' g p Unit)
      -> ParentDSL s s' f f' g p Unit
peek' cp (ChildF s q) action = case Tuple (prjSlot cp s) (prjQuery cp q) of
  Tuple (Just s') (Just q') -> action s' q'
  _ -> pure unit

foreign import getInputFileListImpl :: forall eff. String -> Eff (dom :: DOM | eff) (Nullable FileList)

-- | Get the input field with the given id. Only returns `Just` if at least one file
-- has been selected.
getInputFileList :: forall eff. String -> Eff (dom :: DOM | eff) (Maybe FileList)
getInputFileList i = toMaybe <$> getInputFileListImpl i

-- Pagination

type Pagination a =
  { pages :: Int
  , page  :: Int
  , from  :: Int
  , to    :: Int
  , total :: Int
  , items :: Array a
  }

paginate :: forall a. Int -> Array a -> Int -> Pagination a
paginate segmentLength xs currentPage =
    { pages: pages
    , page: page
    , from: minOrd (offset + 1) len
    , to: minOrd (offset + segmentLength) len
    , total: len
    , items: take segmentLength $ drop offset xs
    }
  where
    len = length xs
    offset = segmentLength * (page - 1)
    pages = maxOrd (ceil $ toNumber len / toNumber segmentLength) 1
    page = minOrd pages currentPage
