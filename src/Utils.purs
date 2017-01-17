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
  , tryFormatNumber
  , fromChars
  , non
  ) where

import Prelude
import Data.String as Str
import Halogen.HTML.Core as H
import Halogen.HTML.Properties.Indexed as P
import Math as Math
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import DOM (DOM)
import DOM.Event.Types (Event, EventType(..))
import DOM.File.Types (FileList)
import Data.Array (drop, take, length, (!!), (..), zip)
import Data.Foldable (class Foldable, foldMap)
import Data.Int (toNumber, ceil, fromNumber)
import Data.Lens (Iso', iso)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Nullable (Nullable, toMaybe)
import Data.String (fromCharArray, singleton, toCharArray)
import Data.Tuple (Tuple(Tuple), fst)
import Data.Unfoldable (replicateA)
import Global (readInt)
import Halogen (ParentDSL, ChildF(ChildF))
import Halogen.Component.ChildPath (ChildPath, prjSlot, prjQuery)
import Types (ErrorDetail)

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
getEntropy n = fromCharArray <$> replicateA n do
    i <- randomInt 0 15
    pure $ fromMaybe '0' $ alphabet !! i
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

-- Number formatting

foreign import tryFormatNumber :: Int -> String -> String

-- Char <-> String

fromChars :: forall f. Foldable f => f Char -> String
fromChars = foldMap singleton

-- non Iso

non :: forall a. Eq a => a -> Iso' (Maybe a) a
non def = iso (fromMaybe def) go
  where go a | a == def = Nothing
             | otherwise = Just a
