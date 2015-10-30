module Api where

import Prelude

import Control.Apply
import Control.Monad.Aff

import Network.HTTP.Affjax

import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)

import Network.HTTP.Affjax.Response

import Types

import Api.Common
import Api.Schema.Framework
import Api.Schema.BusinessData
import Api.Schema.Auth
import Api.Schema.Template
import Api.Schema.Finding

getTemplate :: forall e. ModuleId -> TemplateId -> Aff (Effects e) Template
getTemplate modId templId = getJsonResponse "Could not fetch template." $
  get $ "/api/v0.1/template/get/" <> show modId <> "/" <> show templId

getHeader :: forall e. Aff (Effects e) Template
getHeader = getJsonResponse "Could not fetch header." $
  get $ "/api/v0.1/template/header/DE"

getFrameworks :: forall e. Aff (Effects e) (Array Framework)
getFrameworks = getJsonResponse "Could not load frameworks." $
  get "/api/v0.1/modules"

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
