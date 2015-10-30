module Api.Schema.BusinessData where

import Prelude

import qualified Data.Map as M
import qualified Data.StrMap as SM
import           Data.Maybe
import           Data.Tuple
import           Data.Foreign
import           Data.Foreign.Class
import           Data.Foreign.Keys
import           Data.Traversable
import           Data.List (toList)

import Optic.Core

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode
import Data.Argonaut.Combinators ((:=), (~>))

import Types
import Api.Schema.BusinessData.Key


newtype BDSnapshot = BDSnapshot (M.Map Key String)

_BDSnapshot :: LensP BDSnapshot (M.Map Key String)
_BDSnapshot = lens (\(BDSnapshot m) -> m) (\_ m -> BDSnapshot m)

instance isForeignBDSnapshot :: IsForeign BDSnapshot where
  read json = do
    ks <- keys json
    let mkPair k = Tuple <$> parseKeyF k <*> readProp k json
    kvs <- traverse mkPair ks
    pure $ BDSnapshot $ M.fromList $ toList kvs

emptyBDSnapshot :: BDSnapshot
emptyBDSnapshot = BDSnapshot $ M.empty

newtype BDUpdate = BDUpdate (M.Map Key (Maybe String))

instance encodeJsonBDUpdate :: EncodeJson BDUpdate where
  encodeJson (BDUpdate m) = encodeJson $ SM.fromList (showKeys <$> M.toList m)
    where showKeys (Tuple k v) = Tuple (show k) v

_BDUpdate :: LensP BDUpdate (M.Map Key (Maybe String))
_BDUpdate = lens (\(BDUpdate m) -> m) (\_ m -> BDUpdate m)

newtype BDSnapshotMsg = BDSnapshotMsg
  { updateId :: UpdateId
  , timeStamp :: String
  , values :: BDSnapshot
  }

instance isForeignBDSnapshotMsg :: IsForeign BDSnapshotMsg where
  read json = do
    msg <- { updateId: _, timeStamp: _, values: _ }
      <$> readProp "updateId" json
      <*> readProp "timeStamp" json
      <*> readProp "values" json
    pure $ BDSnapshotMsg msg

newtype BDUpdateMsg = BDUpdateMsg
  { parentUpdateId :: UpdateId
  , values :: BDUpdate
  }

instance encodeJsonBDUpdateMsg :: EncodeJson BDUpdateMsg where
  encodeJson (BDUpdateMsg r) = "parentUpdateId" := r.parentUpdateId
                            ~> "values"         := r.values
                            ~> jsonEmptyObject

newtype BDUpdateResponse = BDUpdateResponse
  { newUpdateId :: UpdateId
  , timeStamp :: String
  }

instance isForeignBDUpdateResponse :: IsForeign BDUpdateResponse where
  read json = do
    fv <- readProp "newUpdateId" json
    time <- readProp "timeStamp" json
    pure $ BDUpdateResponse { newUpdateId: fv, timeStamp: time }
