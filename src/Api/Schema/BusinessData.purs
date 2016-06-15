module Api.Schema.BusinessData where

import Data.Map as M
import Data.StrMap as SM
import Api.Schema.BusinessData.Key (Key, parseKeyF)
import Api.Schema.BusinessData.Value (Value, UpdateValue)
import Api.Schema.Validation (HoleCoords, ValidationResult)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Combinators ((:=), (~>))
import Data.Argonaut.Core (jsonEmptyObject, fromString)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Foreign (ForeignError(JSONError))
import Data.Foreign.Class (class IsForeign, readProp)
import Data.Foreign.Keys (keys)
import Data.Foreign.NullOrUndefined (runNullOrUndefined)
import Data.List (toList)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Prelude (pure, ($), (<*>), (<$>), bind, show)
import Types (UTCTime, UpdateId, TagId)

newtype Update = Update (M.Map Key UpdateValue)

instance encodeJsonUpdate :: EncodeJson Update where
  encodeJson (Update m) = encodeJson $ SM.fromList (showKeys <$> M.toList m)
    where showKeys (Tuple k v) = Tuple (show k) v

newtype Snapshot = Snapshot (M.Map Key Value)

instance isForeignUpdate :: IsForeign Snapshot where
  read json = do
    ks <- keys json
    let mkPair k = Tuple <$> parseKeyF k <*> readProp k json
    kvs <- traverse mkPair ks
    pure $ Snapshot $ M.fromList $ toList kvs

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

newtype SnapshotDesc = SnapshotDesc
  { snapshotDescId       :: UpdateId
  , snapshotDescCreated  :: UTCTime
  , snapshotDescParentId :: Maybe UpdateId
  , snapshotDescSnapshot :: Snapshot
  }

instance isForeignUpdateGet :: IsForeign SnapshotDesc where
  read json = do
    upd <- { snapshotDescId: _
           , snapshotDescCreated: _
           , snapshotDescParentId: _
           , snapshotDescSnapshot: _
           }
      <$> readProp "updateId" json
      <*> readProp "created" json
      <*> (runNullOrUndefined <$> readProp "parentId" json)
      <*> readProp "update" json
    pure $ SnapshotDesc upd

newtype UpdateDesc = UpdateDesc
  { updateDescUpdateId :: UpdateId
  , updateDescCreated  :: UTCTime
  , updateDescAuthor   :: String
  , updateDescTags     :: Array TagDesc
  , updateDescChanges  :: Array UpdateChange
  }

instance isForeignUpdateDesc :: IsForeign UpdateDesc where
  read json = do
    desc <- { updateDescUpdateId: _
            , updateDescCreated: _
            , updateDescAuthor: _
            , updateDescTags: _
            , updateDescChanges: _
            }
      <$> readProp "updateId" json
      <*> readProp "created" json
      <*> readProp "author" json
      <*> readProp "tags" json
      <*> readProp "changes" json
    pure $ UpdateDesc desc

newtype UpdateChange = UpdateChange
  { updateEntryLoc :: ChangeLocationHuman
  , updateEntryOld :: Value
  , updateEntryNew :: Value
  }

instance isForeignUpdateEntry :: IsForeign UpdateChange where
  read json = do
    entry <- { updateEntryLoc: _
             , updateEntryOld: _
             , updateEntryNew: _
             }
      <$> readProp "location" json
      <*> readProp "old" json
      <*> readProp "new" json
    pure $ UpdateChange entry

data ChangeLocationHuman
  = HumanHeaderFact String               -- label
  | HumanFact       String HoleCoords    -- table coords
  | HumanSubsetZ    String String        -- table member
  | HumanCustomZ    String               -- table
  | HumanCustomRow  String String String -- table member sheet

instance isForeignUpdateEntryHuman :: IsForeign ChangeLocationHuman where
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
