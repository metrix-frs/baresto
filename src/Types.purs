module Types where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Foreign (ForeignError(JSONError), F, fail)
import Data.Foreign.Class (class IsForeign, read)
import Data.JSDate (JSDate, jsdate)
import Data.String (singleton)
import Data.Tuple (Tuple)
import Global (readInt)
import Handsontable.Types (HOT)
import Network.HTTP.Affjax (AJAX)
import Text.Parsing.StringParser (runParser)
import Text.Parsing.StringParser.Combinators (many1)
import Text.Parsing.StringParser.String (anyDigit, char)

type Effects =
  ( dom :: DOM
  , avar :: AVAR
  , err :: EXCEPTION
  , console :: CONSOLE
  , ajax :: AJAX
  , hot :: HOT
  , random :: RANDOM
  , ref :: REF
  )

type Metrix = Aff Effects

--

type FrameworkId        = Int
type TaxonomyId         = Int
type ConceptualModuleId = Int
type ModuleId           = Int
type TemplateGroupId    = Int
type TemplateId         = Int

type FileId             = Int
type UpdateId           = Int
type TagId              = Int

type TableId            = Int
type AxisId             = Int
type OrdinateId         = Int
type MemberId           = Int
type CellId             = Int

type CustomMemberId     = String
type SubsetMemberId     = Int
type RowKey             = String

type XBRLCode           = String
type Label              = String
type XBRLCodeSet        = Array (Tuple XBRLCode Label)

-- TODO: Rework this whole UTCTime type to use Date and JSDate correctly.
newtype UTCTime = UTCTime JSDate

instance isForeignUTCTime :: IsForeign UTCTime where
  read json = UTCTime <$> (read json >>= parseJSDate)

parseJSDate :: String -> F JSDate
parseJSDate str = case runParser pDate str of
    Left err -> fail $ JSONError $ "Could not parse date: " <> show err
    Right d -> pure d
  where
    pDate = do
      year <- pNumber
      char '-'
      month <- pNumber
      char '-'
      day <- pNumber
      pure $ jsdate
        { year: year
        , month: month
        , day: day
        , hour: 0.0
        , minute: 0.0
        , second: 0.0
        , millisecond: 0.0
        }
    pNumber = readInt 10 <<< (foldMap singleton) <$> (many1 anyDigit)

foreign import showDate :: JSDate -> String
foreign import showDayImpl :: JSDate -> String

showDay :: UTCTime -> String
showDay (UTCTime date) = showDayImpl date

instance showUTCTime :: Show UTCTime where
  show (UTCTime date) = showDate date

--

type ErrorDetail =
  { title :: String
  , body :: String
  }
