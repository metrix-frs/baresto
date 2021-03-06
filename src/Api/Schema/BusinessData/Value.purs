module Api.Schema.BusinessData.Value where

import Data.Argonaut.Encode.Combinators ((:=), (~>))
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Foreign (fail, ForeignError(JSONError))
import Data.Foreign.Class (class IsForeign, readProp)
import Data.Foreign.NullOrUndefined (unNullOrUndefined)
import Data.Maybe (Maybe)
import Prelude

type Precision = Maybe Int

newtype Value = Value
  { valueData :: Maybe String
  , valuePrecision :: Precision
  }

instance isForeignValue :: IsForeign Value where
  read json = do
    v <- { valueData: _
         , valuePrecision: _
         }
      <$> (unNullOrUndefined <$> readProp "data" json)
      <*> (unNullOrUndefined <$> readProp "precision" json)
    pure $ Value v

instance encodeJsonValue :: EncodeJson Value where
  encodeJson (Value v) = "data" := v.valueData
                      ~> "precision" := v.valuePrecision
                      ~> jsonEmptyObject

data UpdateValue
  = UpdateValueData (Maybe String)
  | UpdateValuePrecision Precision

instance isForeignUpdateValue :: IsForeign UpdateValue where
  read json = do
    tag <- readProp "tag" json
    case tag of
      "data"      -> UpdateValueData <$> (unNullOrUndefined <$> readProp "data" json)
      "precision" -> UpdateValuePrecision <$> (unNullOrUndefined <$> readProp "precision" json)
      _ -> fail $ JSONError "`tag` should be `data`, `precision` or `value`"

instance encodeJsonUpdateValue :: EncodeJson UpdateValue where
  encodeJson (UpdateValueData str) = "tag" := "data"
                                  ~> "data" := str
                                  ~> jsonEmptyObject
  encodeJson (UpdateValuePrecision p) = "tag" := "precision"
                                     ~> "precision" := p
                                     ~> jsonEmptyObject

updateValue :: UpdateValue -> Value -> Value
updateValue upd (Value old) = case upd of
  UpdateValueData d -> Value $ old { valueData = d }
  UpdateValuePrecision p -> Value $ old { valuePrecision = p }
