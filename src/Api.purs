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

import Network.HTTP.Affjax

import Network.HTTP.Affjax.Response

import Halogen.Query (liftEff', liftAff')
import Halogen.Component

import Types

import Api.Common
import Api.Schema.Framework
import Api.Schema.Module
import Api.Schema.BusinessData
import Api.Schema.Auth
import Api.Schema.Table
import Api.Schema.Finding

import qualified Component.Spinner as Spinner
import qualified Component.ErrorBox as ErrorBox

apiCall :: forall eff a s f g. (MonadEff (Effects eff) g, MonadAff (Effects eff) g, Functor g)
        => Aff (Effects eff) a -> (a -> ComponentDSL s f g Unit) -> ComponentDSL s f g Unit
apiCall call onSuccess = do
  liftEff' $ Spinner.dispatch true
  result <- liftAff' $ attempt call
  case result of
    Left err -> liftEff' $ ErrorBox.raise $ message err
    Right x -> onSuccess x
  liftEff' $ Spinner.dispatch false

apiCallParent :: forall eff a s s' f f' g p. (MonadEff (Effects eff) g, MonadAff (Effects eff) g, Functor g)
        => Aff (Effects eff) a -> (a -> ParentDSL s s' f f' g p Unit) -> ParentDSL s s' f f' g p Unit
apiCallParent call onSuccess = do
  liftQuery $ liftEff' $ Spinner.dispatch true
  result <- liftQuery $ liftAff' $ attempt call
  case result of
    Left err -> liftQuery $ liftEff' $ ErrorBox.raise $ message err
    Right x -> onSuccess x
  liftQuery $ liftEff' $ Spinner.dispatch false

--

getTable :: forall e. ModuleId -> TableId -> Aff (Effects e) Table
getTable modId tableId = getJsonResponse "Could not fetch table." $
  get $ "/api/v0.1/table/get/" <> show modId <> "/" <> show tableId

getHeader :: forall e. Aff (Effects e) Table
getHeader = getJsonResponse "Could not fetch header." $
  get $ "/api/v0.1/template/header/DE"

getFrameworks :: forall e. Aff (Effects e) (Array Framework)
getFrameworks = getJsonResponse "Could not load frameworks." $
  get "/api/v0.1/modules"

getModule :: forall eff. ModuleId -> Aff (Effects eff) Module
getModule modId = getJsonResponse "Could not load templates of module." $
  get $ "/api/v0.1/module/" <> show modId

loadSnapshot :: forall e. Aff (Effects e) BDSnapshotMsg
loadSnapshot = getJsonResponse "Could not load snapshot." $
  get "/api/v0.1/businessdata"

sendUpdate :: forall e. BDUpdateMsg -> Aff (Effects e) BDUpdateResponse
sendUpdate upd = getJsonResponse "Could not send update." $
  postJson "/api/v0.1/businessdata" upd

validate :: forall e. ModuleId -> Aff (Effects e) (Array Finding)
validate modId = getJsonResponse "Could not validate." $
  get $ "/api/v0.1/validate/byModuleId/" <> show modId

-- Api.Auth

login :: forall e. String -> String -> Aff (Effects e) LoginResponse
login customerId pw = getJsonResponse "Could not login." $
  get $ "/api/v0.1/auth/login/?customerId=" <> customerId <> "&password=" <> pw

logout :: forall e. Aff (Effects e) Unit
logout = do
  res <- get "/api/v0.1/auth/logout"
  if succeeded res.status
    then pure (res.response :: String) *> pure unit
    else throwError $ error "Error logging out."

loginStatus :: forall e. Aff (Effects e) LoginStatus
loginStatus = getJsonResponse "Could not get login status." $
  get "/api/v0.1/auth/status"
