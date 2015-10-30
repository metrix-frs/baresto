module Api.Common where

import Prelude

import Data.Maybe
import Data.Either
import Data.Foreign
import Data.Foreign.Class

import Network.HTTP.Affjax
import Network.HTTP.Affjax.Request
import Network.HTTP.Affjax.Response
import Network.HTTP.MimeType.Common (applicationJSON)
import Network.HTTP.StatusCode
import Network.HTTP.Method
import Network.HTTP.RequestHeader

import Data.Argonaut.Printer
import Data.Argonaut.Encode

import Control.Monad.Aff
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)


postJson :: forall e a b. (EncodeJson a, Respondable b) => URL -> a -> Affjax e b
postJson u c = affjax $ defaultRequest
  { method = POST
  , url = u
  , content = Just $ toRequest (printJson (encodeJson c) :: String)
  , headers = [ContentType applicationJSON]
  }

succeeded :: StatusCode -> Boolean
succeeded (StatusCode code) = 200 <= code && code < 300

getJsonResponse :: forall r a e. (IsForeign a)
                => String -> Affjax e String -> Aff (ajax :: AJAX | e) a
getJsonResponse msg affjax = do
  res <- affjax
  if succeeded res.status
    then case readJSON res.response of
           Left e -> throwError $ error $ "JSON decode error: " <> show e
           Right x -> pure x
    else throwError $ error msg
