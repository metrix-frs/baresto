module Api.Schema.BusinessData.Value where

import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Combinators ((:=), (~>))
import Data.Foreign.Class (class IsForeign, readProp)
import Data.Foreign.NullOrUndefined (runNullOrUndefined)
import Data.Maybe (Maybe)
import Prelude (bind, (<$>), (<*>), pure, ($))

type Precision = Maybe Int

newtype Value = Value
  { valueData :: String
  , valuePrecision :: Precision
  }

instance isForeignValue :: IsForeign Value where
  read json = do
    v <- { valueData: _
         , valuePrecision: _
         }
      <$> readProp "data" json
      <*> (runNullOrUndefined <$> readProp "precision" json)
    pure $ Value v

instance encodeJsonValue :: EncodeJson Value where
  encodeJson (Value v) = "data" := v.valueData
                      ~> "precision" := v.valuePrecision
                      ~> jsonEmptyObject

data UpdateValue
  = UpdateValueData String
  | UpdateValuePrecision Precision
  | UpdateValueValue Value

instance encodeJsonUpdateValue :: EncodeJson UpdateValue where
  encodeJson (UpdateValueData str) = "tag" := "data"
                                  ~> "data" := str
                                  ~> jsonEmptyObject
  encodeJson (UpdateValuePrecision p) = "tag" := "precision"
                                     ~> "precision" := p
                                     ~> jsonEmptyObject
  encodeJson (UpdateValueValue v) = "tag" := "value"
                                 ~> "value" := v
                                 ~> jsonEmptyObject
