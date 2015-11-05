module Api.Schema.BusinessData where

import Prelude

import qualified Data.Map as M
import qualified Data.StrMap as SM
import           Data.Maybe
import           Data.Tuple
import           Data.Foreign
import           Data.Foreign.Class
import           Data.Foreign.Keys
import           Data.Foreign.NullOrUndefined
import           Data.Traversable
import           Data.List (toList)

import Optic.Core

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode
import Data.Argonaut.Combinators ((:=), (~>))

import Types
import Api.Schema.BusinessData.Key


newtype Update = Update (M.Map Key (Tuple (Maybe String) (Maybe String)))

instance isForeignUpdate :: IsForeign Update where
  read json = do
    ks <- keys json
    let mkPair k = Tuple <$> parseKeyF k <*> readProp k json
    kvs <- traverse mkPair ks
    pure $ Update $ M.fromList $ (\(Tuple k (Change t)) -> Tuple k t) <$> toList kvs

instance encodeJsonUpdate :: EncodeJson Update where
  encodeJson (Update m) = encodeJson $ SM.fromList (showKeys <$> M.toList m)
    where showKeys (Tuple k v) = Tuple (show k) (Change v)

newtype Change = Change (Tuple (Maybe String) (Maybe String))

instance isForeignChange :: IsForeign Change where
  read json = do
    old <- runNullOrUndefined <$> readProp "old" json
    new <- runNullOrUndefined <$> readProp "new" json
    pure $ Change $ Tuple old new

instance encodeJsonChange :: EncodeJson Change where
  encodeJson (Change (Tuple mOld mNew)) =
      mAdd "old" mOld <<< mAdd "new" mNew $ jsonEmptyObject
    where mAdd k mVal obj = case mVal of
            Just val -> k := val ~> obj
            Nothing -> obj

--

newtype UpdatePost = UpdatePost
  { updatePostParentId :: UpdateId
  , updatePostFileId   :: FileId
  , updatePostUpdate   :: Update
  }

instance encodeJsonUpdatePost :: EncodeJson UpdatePost where
  encodeJson (UpdatePost p) = "parentId" := p.updatePostParentId
                           ~> "fileId" := p.updatePostFileId
                           ~> "update" := p.updatePostUpdate
                           ~> jsonEmptyObject

data UpdateGet = UpdateGet
  { updateGetId       :: UpdateId
  , updateGetCreated  :: UTCTime
  , updateGetParentId :: Maybe UpdateId
  , updateGetUpdate   :: Update
  }

instance isForeignUpdateGet :: IsForeign UpdateGet where
  read json = do
    upd <- { updateGetId: _
           , updateGetCreated: _
           , updateGetParentId: _
           , updateGetUpdate: _
           }
      <$> readProp "updateId" json
      <*> readProp "created" json
      <*> (runNullOrUndefined <$> readProp "parentId" json)
      <*> readProp "update" json
    pure $ UpdateGet upd

data UpdateConfirmation = UpdateConfirmation
  { updateConfUpdateId :: UpdateId
  , updateConfCreated  :: UTCTime
  }

instance isForeignUpdateConfirmation :: IsForeign UpdateConfirmation where
  read json = do
    conf <- { updateConfUpdateId: _
            , updateConfCreated: _
            }
      <$> readProp "updateId" json
      <*> readProp "created" json
    pure $ UpdateConfirmation conf
