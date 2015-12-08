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

import Data.Argonaut.Core (jsonEmptyObject, fromString)
import Data.Argonaut.Encode
import Data.Argonaut.Combinators ((:=), (~>))

import Types
import Api.Schema.Validation
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

data ValidationType
  = VTNone
  | VTWhole
  | VTUpdate

instance encodeJsonValidationType :: EncodeJson ValidationType where
  encodeJson VTNone   = fromString "none"
  encodeJson VTWhole  = fromString "whole"
  encodeJson VTUpdate = fromString "update"

newtype UpdatePost = UpdatePost
  { updatePostParentId       :: UpdateId
  , updatePostUpdate         :: Update
  , updatePostValidationType :: ValidationType
  }

instance encodeJsonUpdatePost :: EncodeJson UpdatePost where
  encodeJson (UpdatePost p) = "parentId"       := p.updatePostParentId
                           ~> "update"         := p.updatePostUpdate
                           ~> "validationType" := p.updatePostValidationType
                           ~> jsonEmptyObject

newtype UpdatePostResult = UpdatePostResult
  { uprUpdateDesc       :: UpdateDesc
  , uprValidationResult :: ValidationResult
  }

instance isForeignUpdatePostResult :: IsForeign UpdatePostResult where
  read json = do
    upr <- { uprUpdateDesc: _
           , uprValidationResult: _
           }
      <$> readProp "updateDesc" json
      <*> readProp "validationResult" json
    pure $ UpdatePostResult upr

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
  , tagDescCreated  :: UTCTime
  }

instance isForeignTagDesc :: IsForeign TagDesc where
  read json = do
    desc <- { tagDescTagId: _
            , tagDescUpdateId: _
            , tagDescTagName: _
            , tagDescCreated: _
            }
      <$> readProp "tagId" json
      <*> readProp "updateId" json
      <*> readProp "name" json
      <*> readProp "created" json
    pure $ TagDesc desc
