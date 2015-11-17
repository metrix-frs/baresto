module Api.Common where

import Prelude

import Data.Maybe
import Data.Either
import Data.Foreign
import Data.Foreign.Class

import Network.HTTP.Affjax
import Network.HTTP.Affjax.Request
import Network.HTTP.Affjax.Response
import Network.HTTP.MimeType.Common (applicationJSON, multipartFormData)
import Network.HTTP.StatusCode
import Network.HTTP.Method
import Network.HTTP.RequestHeader

import DOM.File.Types (FileList())
import DOM.XHR.Types (FormData())

import Data.Argonaut.Printer
import Data.Argonaut.Encode

import Control.Monad.Aff
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)

foreign import filesToFormData :: FileList -> FormData

postJson :: forall eff a b. (EncodeJson a, Respondable b) => URL -> a -> Affjax eff b
postJson u c = affjax $ defaultRequest
  { method = POST
  , url = u
  , content = Just $ toRequest (printJson (encodeJson c) :: String)
  , headers = [ContentType applicationJSON]
  }

uploadFiles :: forall eff b. (Respondable b) => URL -> FileList -> Affjax eff b
uploadFiles u f = affjax $ defaultRequest
  { method = POST
  , url = u
  , content = Just $ toRequest $ filesToFormData f
  -- TODO: report purescript-affjax issue about `multipartFormData` and boundary
  -- , headers = [ContentType multipartFormData]
  }

succeeded :: StatusCode -> Boolean
succeeded (StatusCode code) = 200 <= code && code < 300

getJsonResponse :: forall a eff. (IsForeign a)
                => String -> Affjax eff String -> Aff (ajax :: AJAX | eff) a
getJsonResponse msg affjax = do
  res <- affjax
  if succeeded res.status
    then case readJSON res.response of
           Left e -> throwError $ error $ "JSON decode error: " <> show e
           Right x -> pure x
    else throwError $ error msg
