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
  , updatePostUpdate   :: Update
  }

instance encodeJsonUpdatePost :: EncodeJson UpdatePost where
  encodeJson (UpdatePost p) = "parentId" := p.updatePostParentId
                           ~> "update" := p.updatePostUpdate
                           ~> jsonEmptyObject

newtype UpdateGet = UpdateGet
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

newtype UpdateDesc = UpdateDesc
  { updateDescUpdateId :: UpdateId
  , updateDescCreated  :: UTCTime
  , updateDescAuthor   :: String
  , updateDescTags     :: Array TagDesc
  }

instance isForeignUpdateDesc :: IsForeign UpdateDesc where
  read json = do
    desc <- { updateDescUpdateId: _
            , updateDescCreated: _
            , updateDescAuthor: _
            , updateDescTags: _
            }
      <$> readProp "updateId" json
      <*> readProp "created" json
      <*> readProp "author" json
      <*> readProp "tags" json
    pure $ UpdateDesc desc

newtype TagDesc = TagDesc
  { tagDescTagId    :: TagId
  , tagDescUpdateId :: UpdateId
  , tagDescTagName  :: String
  }

instance isForeignTagDesc :: IsForeign TagDesc where
  read json = do
    desc <- { tagDescTagId: _
            , tagDescUpdateId: _
            , tagDescTagName: _
            }
      <$> readProp "tagId" json
      <*> readProp "updateId" json
      <*> readProp "name" json
    pure $ TagDesc desc
