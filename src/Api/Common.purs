module Api.Common where

import Api.Schema (ServerResponse(ServerSuccess, ServerError))
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Except (runExcept)
import Control.Monad.Except.Trans (ExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import DOM.File.Types (FileList)
import DOM.XHR.Types (FormData)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Printer (printJson)
import Data.Either (Either(Left, Right))
import Data.Foreign.Class (class IsForeign, readJSON)
import Data.HTTP.Method (Method(POST))
import Data.Maybe (Maybe(Just))
import Data.MediaType.Common (applicationJSON)
import Data.Tuple (snd)
import Network.HTTP.Affjax (Affjax, URL, AJAX, defaultRequest, affjax)
import Network.HTTP.Affjax.Request (toRequest)
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.RequestHeader (RequestHeader(ContentType))
import Network.HTTP.StatusCode (StatusCode(StatusCode))
import Prelude
import Types (ErrorDetail)

foreign import filesToFormData :: FileList -> FormData

type Api eff a = ExceptT ErrorDetail (Aff (ajax :: AJAX | eff)) a

postJson :: forall eff a b. (EncodeJson a, Respondable b) => URL -> a -> Affjax eff b
postJson u c = affjax $ defaultRequest
  { method = Left POST
  , url = u
  , content = Just $ snd $ toRequest (printJson (encodeJson c) :: String)
  , headers = [ContentType applicationJSON]
  }

uploadFiles :: forall eff b. (Respondable b) => URL -> FileList -> Affjax eff b
uploadFiles u f = affjax $ defaultRequest
  { method = Left POST
  , url = u
  , content = Just $ snd $ toRequest $ filesToFormData f
  -- TODO: report purescript-affjax issue about `multipartFormData` and boundary
  -- , headers = [ContentType multipartFormData]
  }

succeeded :: StatusCode -> Boolean
succeeded (StatusCode code) = 200 <= code && code < 300

getJsonResponse :: forall a eff. (IsForeign a)
                => String -> Affjax eff String -> Api eff a
getJsonResponse msg affjax = do
  result <- lift $ attempt affjax
  case result of
    Right res -> if succeeded res.status
      then case runExcept (readJSON res.response) of
             Left e -> throwError
                          { title: "JSON decode error"
                          , body: show e
                          }
             Right x -> case x of
              ServerError e -> throwError e
              ServerSuccess a -> pure a
      else throwError
              { title: msg
              , body: "Probably a server or protocol error. Please consult the log files."
              }
    Left e -> throwError
      { title: "Exception"
      , body: show e
      }

getUnitResponse :: forall eff. String -> Affjax eff String -> Api eff Unit
getUnitResponse msg affjax = do
  result <- lift $ attempt affjax
  case result of
    Right res -> if succeeded res.status
      then pure unit
      else throwError
              { title: msg
              , body: "Probably a connection or protocol error."
              }
    Left e -> throwError
      { title: "Exception"
      , body: show e
      }
