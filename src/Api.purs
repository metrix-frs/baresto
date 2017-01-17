module Api where

import Prelude
import Component.ErrorBox as ErrorBox
import Component.Spinner as Spinner
import Api.Common (Api, getJsonResponse, getUnitResponse, uploadFiles, postJson)
import Api.Schema (JsonEither, Name(Name))
import Api.Schema.Auth (AuthInfo)
import Api.Schema.BusinessData (SnapshotDesc, UpdateDesc, UpdatePostResult, UpdatePost, TagDesc)
import Api.Schema.File (File, FileDesc)
import Api.Schema.Import (CsvImportConf, XbrlImportConf)
import Api.Schema.Module (Module)
import Api.Schema.Selector (Framework)
import Api.Schema.Table (Table)
import Api.Schema.Validation (ValidationResult)
import Control.Monad.Aff.Free (class Affable, fromAff, fromEff)
import Control.Monad.Except.Trans (runExceptT)
import DOM.File.Types (FileList)
import Data.Either (Either(Right, Left))
import Data.Foreign.Null (Null)
import Halogen.Component (ParentDSL, ComponentDSL, liftQuery)
import Network.HTTP.Affjax (get)
import Types (FileId, ModuleId, TableId, TagId, UpdateId, Effects)

apiCall
  :: forall a s f g
   . ( Affable Effects g
     , Monad g )
  => Api _ a
  -> (a -> ComponentDSL s f g Unit)
  -> ComponentDSL s f g Unit
apiCall call onSuccess = do
  fromEff $ Spinner.dispatch true
  result <- fromAff $ runExceptT call
  fromEff $ Spinner.dispatch false
  case result of
    Left err -> fromEff $ ErrorBox.raise err
    Right x -> onSuccess x

apiCallParent
  :: forall a s s' f f' g p
   . ( Affable Effects g
     , Monad g )
  => Api _ a
  -> (a -> ParentDSL s s' f f' g p Unit)
  -> ParentDSL s s' f f' g p Unit
apiCallParent call onSuccess = do
  liftQuery $ fromEff $ Spinner.dispatch true
  result <- liftQuery $ fromAff $ runExceptT call
  liftQuery $ fromEff $ Spinner.dispatch false
  case result of
    Left err -> liftQuery $ fromEff $ ErrorBox.raise err
    Right x -> onSuccess x

--

foreign import apiUrl :: String

prefix :: String
prefix = apiUrl <> "/api/v0.1/"

-- Api.Table

getTable :: forall eff. ModuleId -> TableId -> Api eff Table
getTable modId tableId = getJsonResponse "Could not fetch table." $
  get $ prefix <> "table/get/" <> show modId <> "/" <> show tableId

getHeader :: forall eff. Api eff Table
getHeader = getJsonResponse "Could not fetch header." $
  get $ prefix <> "table/header/DE"

-- Api.Module

getModule :: forall eff. ModuleId -> Api eff Module
getModule modId = getJsonResponse "Could not load templates of module." $
  get $ prefix <> "module/get/" <> show modId

-- Api.BusinessData

newFile :: forall eff. ModuleId -> String -> Api eff SnapshotDesc
newFile modId name = getJsonResponse "Could not create file." $
  postJson (prefix <> "businessdata/file/new/" <> show modId) (Name name)

deleteFile :: forall eff. FileId -> Api eff Unit
deleteFile fileId = getUnitResponse "Error deleting file." $
  get $ prefix <> "businessdata/file/delete/" <> show fileId

renameFile :: forall eff. FileId -> String -> Api eff String
renameFile fileId newName = getJsonResponse "Error renaming file." $
  postJson (prefix <> "businessdata/file/rename/" <> show fileId) (Name newName)

listFiles :: forall eff. Api eff (Array File)
listFiles = getJsonResponse "Could not get files." $
  get $ prefix <> "businessdata/file/all"

getFileOrphans :: forall eff. FileId -> Api eff (Array UpdateDesc)
getFileOrphans fileId = getJsonResponse "Could not get auto saves." $
  get $ prefix <> "businessdata/file/orphans/" <> show fileId

getFileTags :: forall eff. FileId -> Api eff (Array TagDesc)
getFileTags fileId = getJsonResponse "Could not get file tags." $
  get $ prefix <> "businessdata/file/tags/" <> show fileId

--

newTag :: forall eff. UpdateId -> String -> Api eff TagDesc
newTag updateId name = getJsonResponse "Could not create tag." $
  postJson (prefix <> "businessdata/tag/new/" <> show updateId) (Name name)

deleteTag :: forall eff. TagId -> Api eff Unit
deleteTag tagId = getUnitResponse "Could not delete tag." $
  get $ prefix <> "businessdata/tag/delete/" <> show tagId

renameTag :: forall eff. TagId -> String -> Api eff String
renameTag tagId newName = getJsonResponse "Could not rename tag." $
  postJson (prefix <> "businessdata/tag/rename/" <> show tagId) (Name newName)

--

getUpdateSnapshot :: forall eff. UpdateId -> Api eff SnapshotDesc
getUpdateSnapshot updateId = getJsonResponse "Could not load file." $
  get $ prefix <> "businessdata/update/snapshot/" <> show updateId

postUpdate :: forall eff. UpdatePost -> Api eff UpdatePostResult
postUpdate upd = getJsonResponse "Could not send update." $
  postJson (prefix <> "businessdata/update") upd

getFileDetails :: UpdateId -> forall eff. Api eff FileDesc
getFileDetails updateId = getJsonResponse "Could not get file details." $
  get $ prefix <> "businessdata/update/file/" <> show updateId

getUpdatePast :: forall eff. UpdateId -> Api eff (Array UpdateDesc)
getUpdatePast updateId = getJsonResponse "Could not get revisions." $
  get $ prefix <> "businessdata/update/past/" <> show updateId

pruneOrphan :: forall eff. UpdateId -> Api eff Unit
pruneOrphan updateId = getUnitResponse "Could not delete orphan." $
  get $ prefix <> "businessdata/update/prune/" <> show updateId

--

uploadXbrl :: forall eff. FileList -> Api eff XbrlImportConf
uploadXbrl files = getJsonResponse "Could not upload XBRL file." $
  uploadFiles (prefix <> "xbrl/import") files

uploadBaresto :: forall eff. FileList -> Api eff File
uploadBaresto files = getJsonResponse "Could not upload Baresto file." $
  uploadFiles (prefix <> "baresto/import") files

uploadCsv :: forall eff. UpdateId -> FileList -> Api eff CsvImportConf
uploadCsv lastUpdateId files = getJsonResponse "Could not upload CSV file." $
  uploadFiles (prefix <> "csv/import/" <> show lastUpdateId) files

-- Api.Selector

listFrameworks :: forall eff. Api eff (Array Framework)
listFrameworks = getJsonResponse "Could not get frameworks." $
  get $ prefix <> "selector/frameworks"

-- Api.Validate

validate :: forall eff. UpdateId -> Api eff ValidationResult
validate updateId = getJsonResponse "Could not validate." $
  get $ prefix <> "validate/byUpdateId/" <> show updateId

-- Api.Auth

login :: forall eff. String -> String -> Api eff (JsonEither String AuthInfo)
login customerId pw = getJsonResponse "Could not login." $
  get $ prefix <> "auth/login/?customerId=" <> customerId <> "&password=" <> pw

logout :: forall eff. Api eff Unit
logout = getUnitResponse "Error logging out." $
  get $ prefix <> "auth/logout"

loginStatus :: forall eff. Api eff (Null AuthInfo)
loginStatus = getJsonResponse "Could not get login status." $
  get $ prefix <> "auth/status"
