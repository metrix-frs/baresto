module Api.Schema.BusinessData where

import Prelude (pure, ($), (<*>), (<$>), bind, (<<<), show)

import Control.Monad.Error.Class (throwError)

import Data.Map as M
import Data.StrMap as SM
import Data.Maybe (Maybe(Nothing, Just))
import Data.Tuple (Tuple(Tuple))
import Data.Foreign (ForeignError(JSONError))
import Data.Foreign.Class (class IsForeign, readProp)
import Data.Foreign.Keys (keys)
import Data.Foreign.NullOrUndefined (runNullOrUndefined)
import Data.Traversable (traverse)
import Data.List (toList)
import Data.Argonaut.Core (jsonEmptyObject, fromString)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Combinators ((:=), (~>))

import Types (UTCTime, UpdateId, TagId)
import Api.Schema.Validation (HoleCoords, ValidationResult)
import Api.Schema.BusinessData.Key (Key, parseKeyF)

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
  , updateDescEntries  :: Array UpdateEntry
  }

instance isForeignUpdateDesc :: IsForeign UpdateDesc where
  read json = do
    desc <- { updateDescUpdateId: _
            , updateDescCreated: _
            , updateDescAuthor: _
            , updateDescTags: _
            , updateDescEntries: _
            }
      <$> readProp "updateId" json
      <*> readProp "created" json
      <*> readProp "author" json
      <*> readProp "tags" json
      <*> readProp "entries" json
    pure $ UpdateDesc desc

newtype UpdateEntry = UpdateEntry
  { updateEntryLoc :: UpdateEntryHuman
  , updateEntryOld :: Maybe String
  , updateEntryNew :: Maybe String
  }

instance isForeignUpdateEntry :: IsForeign UpdateEntry where
  read json = do
    entry <- { updateEntryLoc: _
             , updateEntryOld: _
             , updateEntryNew: _
             }
      <$> readProp "location" json
      <*> (runNullOrUndefined <$> readProp "old" json)
      <*> (runNullOrUndefined <$> readProp "new" json)
    pure $ UpdateEntry entry

data UpdateEntryHuman
  = HumanHeaderFact String               -- label
  | HumanFact       String HoleCoords    -- table coords
  | HumanSubsetZ    String String        -- table member
  | HumanCustomZ    String               -- table
  | HumanCustomRow  String String String -- table member sheet

instance isForeignUpdateEntryHuman :: IsForeign UpdateEntryHuman where
  read json = do
    typ <- readProp "type" json
    case typ of
      "header"    -> HumanHeaderFact <$> readProp "label" json
      "fact"      -> HumanFact <$> readProp "table" json
                               <*> readProp "coords" json
      "subsetZ"   -> HumanSubsetZ <$> readProp "table" json
                                  <*> readProp "member" json
      "customZ"   -> HumanCustomZ <$> readProp "table" json
      "customRow" -> HumanCustomRow <$> readProp "table" json
                                    <*> readProp "member" json
                                    <*> readProp "sheet" json
      _           -> throwError $ JSONError "expected `header`, `fact`, `subsetZ`, `customZ` or `customRow`"

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
