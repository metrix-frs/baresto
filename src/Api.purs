module Api where

import Prelude

import Control.Apply
import Control.Monad.Aff (attempt, Aff())
import Control.Monad.Aff.Class
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception (error, message)
import Control.Monad.Eff.Console (log)
import Control.Monad.Error.Class (throwError)

import Data.Either

import DOM.File.Types (FileList())

import Network.HTTP.Affjax
import Network.HTTP.Affjax.Response

import Halogen.Query (liftEff', liftAff')
import Halogen.Component

import Types

import Api.Common
import Api.Schema
import Api.Schema.Selector
import Api.Schema.File
import Api.Schema.Module
import Api.Schema.BusinessData
import Api.Schema.Auth
import Api.Schema.Table
import Api.Schema.Finding
import Api.Schema.Import

import qualified Component.Spinner as Spinner
import qualified Component.ErrorBox as ErrorBox

apiCall :: forall eff a s f g. (MonadEff (Effects eff) g, MonadAff (Effects eff) g, Functor g)
        => Aff (Effects eff) a -> (a -> ComponentDSL s f g Unit) -> ComponentDSL s f g Unit
apiCall call onSuccess = do
  liftEff' $ Spinner.dispatch true
  result <- liftAff' $ attempt call
  liftEff' $ Spinner.dispatch false
  case result of
    Left err -> liftEff' $ ErrorBox.raise $ message err
    Right x -> onSuccess x

apiCallParent :: forall eff a s s' f f' g p. (MonadEff (Effects eff) g, MonadAff (Effects eff) g, Functor g)
        => Aff (Effects eff) a -> (a -> ParentDSL s s' f f' g p Unit) -> ParentDSL s s' f f' g p Unit
apiCallParent call onSuccess = do
  liftQuery $ liftEff' $ Spinner.dispatch true
  result <- liftQuery $ liftAff' $ attempt call
  liftQuery $ liftEff' $ Spinner.dispatch false
  case result of
    Left err -> liftQuery $ liftEff' $ ErrorBox.raise $ message err
    Right x -> onSuccess x

--

prefix :: String
prefix = "/api/v0.1/"

-- Api.Table

getTable :: forall eff. ModuleId -> TableId -> Aff (Effects eff) Table
getTable modId tableId = getJsonResponse "Could not fetch table." $
  get $ prefix <> "table/get/" <> show modId <> "/" <> show tableId

getHeader :: forall eff. Aff (Effects eff) Table
getHeader = getJsonResponse "Could not fetch header." $
  get $ prefix <> "table/header/DE"

-- Api.Module

getModule :: forall eff. ModuleId -> Aff (Effects eff) Module
getModule modId = getJsonResponse "Could not load templates of module." $
  get $ prefix <> "module/get/" <> show modId

-- Api.BusinessData

newFile :: forall eff. ModuleId -> String -> Aff (Effects eff) UpdateGet
newFile modId name = getJsonResponse "Could not create file." $
  get $ prefix <> "businessdata/file/new/" <> show modId <> "/" <> name

deleteFile :: forall eff. FileId -> Aff (Effects eff) Unit
deleteFile fileId = getUnitResponse "Error deleting file." $
  get $ prefix <> "businessdata/file/delete/" <> show fileId

renameFile :: forall eff. FileId -> String -> Aff (Effects eff) Unit
renameFile fileId newName = getUnitResponse "Error renaming file." $
  get $ prefix <> "businessdata/file/rename/" <> show fileId <> "/" <> newName

getFileOrphans :: forall eff. FileId -> Aff (Effects eff) (Array UpdateDesc)
getFileOrphans fileId = getJsonResponse "Could not get auto saves." $
  get $ prefix <> "businessdata/file/orphans/" <> show fileId

getFileTags :: forall eff. FileId -> Aff (Effects eff) (Array TagDesc)
getFileTags fileId = getJsonResponse "Could not get file tags." $
  get $ prefix <> "businessdata/file/tags/" <> show fileId

--

newTag :: forall eff. UpdateId -> String -> Aff (Effects eff) TagDesc
newTag updateId name = getJsonResponse "Could not create tag." $
  get $ prefix <> "businessdata/tag/new/" <> show updateId <> "/" <> name

deleteTag :: forall eff. TagId -> Aff (Effects eff) Unit
deleteTag tagId = getUnitResponse "Could not delete tag." $
  get $ prefix <> "businessdata/tag/delete/" <> show tagId

renameTag :: forall eff. TagId -> String -> Aff (Effects eff) Unit
renameTag tagId newName = getUnitResponse "Could not rename tag." $
  get $ prefix <> "businessdata/tag/rename/" <> show tagId <> "/" <> newName

--

getUpdateSnapshot :: forall eff. UpdateId -> Aff (Effects eff) UpdateGet
getUpdateSnapshot updateId = getJsonResponse "Could not load file." $
  get $ prefix <> "businessdata/update/snapshot/" <> show updateId

postUpdate :: forall eff. UpdatePost -> Aff (Effects eff) UpdateDesc
postUpdate upd = getJsonResponse "Could not send update." $
  postJson (prefix <> "businessdata/update") upd

getUpdatePast :: forall eff. UpdateId -> Aff (Effects eff) (Array UpdateDesc)
getUpdatePast updateId = getJsonResponse "Could not get revisions." $
  get $ prefix <> "businessdata/update/past/" <> show updateId

pruneOrphan :: forall eff. UpdateId -> Aff (Effects eff) Unit
pruneOrphan updateId = getUnitResponse "Could not delete orphan." $
  get $ prefix <> "businessdata/update/prune/" <> show updateId

--

uploadXbrl :: forall eff. FileList -> Aff (Effects eff) (ServerResponse XbrlImportConf)
uploadXbrl files = getJsonResponse "Could not upload XBRL file." $
  uploadFiles (prefix <> "xbrl/import") files

uploadCsv :: forall eff. UpdateId -> FileList -> Aff (Effects eff) (ServerResponse CsvImportConf)
uploadCsv lastUpdateId files = getJsonResponse "Could not upload CSV file." $
  uploadFiles (prefix <> "csv/import/" <> show lastUpdateId) files

-- Api.Selector

listFrameworks :: forall eff. Aff (Effects eff) (Array Framework)
listFrameworks = getJsonResponse "Could not get frameworks." $
  get $ prefix <> "selector/frameworks"

listFiles :: forall eff. Aff (Effects eff) (Array File)
listFiles = getJsonResponse "Could not get files." $
  get $ prefix <> "selector/files"

-- Api.Validate

validate :: forall eff. UpdateId -> Aff (Effects eff) (Array Finding)
validate updateId = getJsonResponse "Could not validate." $
  get $ prefix <> "validate/byUpdateId/" <> show updateId

-- Api.Auth

login :: forall eff. String -> String -> Aff (Effects eff) LoginResponse
login customerId pw = getJsonResponse "Could not login." $
  get $ prefix <> "auth/login/?customerId=" <> customerId <> "&password=" <> pw

logout :: forall eff. Aff (Effects eff) Unit
logout = getUnitResponse "Error logging out." $
  get $ prefix <> "auth/logout"

loginStatus :: forall eff. Aff (Effects eff) LoginStatus
loginStatus = getJsonResponse "Could not get login status." $
  get $ prefix <> "auth/status"
