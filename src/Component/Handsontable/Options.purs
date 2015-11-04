module Component.Handsontable.Options
  ( tableOptions
  , MergeCells()
  , Props()
  , ColumnProp()
  , CellBorder()
  , CellRenderer()
  ) where

import Prelude

import Control.Bind
import Data.Array hiding (take)
import Data.Traversable
import Data.Foldable
import Data.Maybe
import Data.Tuple
import Data.Function
import Data.Nullable
import Data.String (take)
import Data.Tuple.Nested

import Utils (makeIndexed, getIndices)

import qualified Handsontable       as Hot
import qualified Handsontable.Types as Hot

import Types
import Api.Schema.Table
import Lib.Table
import Lib.BusinessData

import Component.Handsontable.Utils

foreign import data CellRenderer :: *
foreign import data CellBorder :: *
foreign import data ColumnProp :: *

foreign import renderSetClass :: String -> CellRenderer
foreign import renderer :: String -> CellRenderer
foreign import renderHtml :: String -> CellRenderer

foreign import colPropEmpty :: ColumnProp
foreign import colPropWidth :: Int -> ColumnProp

foreign import borderImpl :: Fn8 Int Int Int Int (Nullable Border) (Nullable Border) (Nullable Border) (Nullable Border) CellBorder

border :: Int -> Int -> Int -> Int -> Maybe Border -> Maybe Border -> Maybe Border -> Maybe Border -> CellBorder
border r1 c1 r2 c2 top right bot left = runFn8 borderImpl r1 c1 r2 c2 (toNullable top) (toNullable right) (toNullable bot) (toNullable left)

tableOptions :: S -> Table -> BusinessData -> Hot.Options String _
tableOptions s table@(Table tbl) bd =
  { data:             xHeaderData  tbl.tableXHeader
                   <> xOrdsData    tbl.tableXOrdinates
                   <> yOrdsData    table bd
  , mergeCells:       xHeaderMerge tbl.tableXHeader
                   <> yOrdsMerge   table
  , cell:             xHeaderProps tbl.tableXHeader
                   <> xOrdsProps   table
                   <> yOrdsProps   table bd
                   <> cellProps    s table bd
  , columns:          columns table
  , customBorders:    xHeaderBorders tbl.tableXHeader
                   <> whiteBorders tbl.tableXHeader
  , className:        "sheet"
  , maxRows: case tbl.tableYAxis of
      YAxisClosed _ ords -> length tbl.tableXHeader + 1 + length ords
      YAxisCustom axId _ -> length tbl.tableXHeader + 2 + length (getCustomMembers axId bd)
  , fixedRowsTop: length tbl.tableXHeader + 1
  , fixedColumnsLeft: 2
  , currentRowClassName: "currentRow"
  , currentColClassName: "currentCol"
  }

type Border =
  { width :: Int
  , color :: String
  }

borderColor :: String
borderColor = "#808080"

ordinateColor :: String
ordinateColor = "#EEEEEE"

type Data = Array (Array String)

type Props =
  { row :: Int
  , col :: Int
  , readOnly :: Boolean
  , renderer :: CellRenderer
  , type :: String
  , format :: String
  , dateFormat :: String
  , source :: Array String
  }

defProps :: Props
defProps =
  { row: 0
  , col: 0
  , readOnly: true
  , renderer: renderSetClass ""
  , type: "text"
  , format: ""
  , dateFormat: ""
  , source: []
  }

type MergeCells =
  { row :: Int
  , col :: Int
  , rowspan :: Int
  , colspan :: Int
  }

whiteData :: Array String
whiteData = ["", ""]

whiteCells :: Int -> Array Props
whiteCells rowIndex = row <$> [0,1]
  where row c =
          { row: rowIndex
          , col: c
          , readOnly: true
          , renderer: renderSetClass ""
          , type: "text"
          , format: ""
          , dateFormat: ""
          , source: []
          }

whiteBorders :: XHeader -> Array CellBorder
whiteBorders rows = join $ row <$> range 0 (length rows)
  where
    row i = [ b i 0, b i 1 ]
    b i j = border i j i j (Just {width: 1, color: "white"}) Nothing Nothing (Just {width: 1, color: "white"})

columns :: Table -> Array _
columns (Table tbl) =
  [ colPropEmpty
  , colPropEmpty
  ] <> ((const $ colPropWidth 120) <$> tbl.tableXOrdinates)

xHeaderBorders :: XHeader -> Array CellBorder
xHeaderBorders rows = join $ row <$> makeIndexed rows
  where
    row (Tuple rowIndex cells) = _.value $ mapAccumL cell 2 cells
      where
        cell colIndex (XHeaderCell c) =
            { accum: colIndex + c.colspan
            , value: case c.ordinate of
                Just _  -> border 0 0 0 0 Nothing Nothing Nothing Nothing
                Nothing -> border rowIndex colIndex rowIndex (colIndex + c.colspan - 1)
                               (Just {width: 1, color: ordinateColor}) Nothing Nothing Nothing
            }

xHeaderData :: XHeader -> Data
xHeaderData rows = row <$> makeIndexed rows
  where
    row (Tuple _ cells) = whiteData <> join (toCellData <$> cells)
    toCellData (XHeaderCell c) = fstCol : fillCols
      where
        fstCol = fromMaybe "" $ (\(Ordinate s) -> s.ordinateLabel) <$> c.ordinate
        fillCols = replicate (c.colspan - 1) ""

xOrdsData :: Ordinates -> Data
xOrdsData ords = [whiteData <> (ord <$> ords)]
  where
    ord (Ordinate o) = o.ordinateCode

yOrdsData :: Table -> BusinessData -> Data
yOrdsData table@(Table tbl) bd = case tbl.tableYAxis of
    YAxisClosed _ ords -> closed <$> ords
    YAxisCustom axId lbl -> [[ lbl, "<button id=\"newCustomY\">+</button>" ]] <> (custom <$> getCustomMembers axId bd)
  where
    custom (Tuple memId _) = [ "", "<button id=\"delCustomY" <> memId <> "\">&#8211;</button>" ]
    closed (Ordinate o) = if o.ordinateIsAbstract
      then [ indent o.ordinateLevel (o.ordinateCode <> " " <> o.ordinateLabel) ]
      else [ indent o.ordinateLevel o.ordinateLabel, o.ordinateCode ]
    indent i label =
      let short = take 100 label
          shortLabel = if label == short then label else short <> " <b>...</b>"
      in  "<span style=\"width: " <> show i <> "em; display: inline-block;\"></span><span title=\"" <> label <> "\">" <> shortLabel <> "</span>"

xHeaderMerge :: XHeader -> Array MergeCells
xHeaderMerge rows = join $ row <$> makeIndexed rows
  where
    row (Tuple rowIndex cells) = snd $ foldl mergeFold (Tuple (length whiteData) []) cells
      where
        mergeFold (Tuple colIndex mcs) (XHeaderCell c) = Tuple (colIndex + c.colspan) $
          mcs <> [{ row: rowIndex, col: colIndex, rowspan: 1, colspan: c.colspan }]

yOrdsMerge :: Table -> Array MergeCells
yOrdsMerge (Table tbl) = case tbl.tableYAxis of
    YAxisClosed _ ords -> ord <$> (filter (\(Tuple _ (Ordinate o)) -> o.ordinateIsAbstract) $ makeIndexed ords)
    YAxisCustom _ _ -> []
  where
    rowIndex = length tbl.tableXHeader + 1
    ord (Tuple index (Ordinate o)) = { row: index + rowIndex, col: 0, rowspan: 1, colspan: 2 }

xHeaderProps :: XHeader -> Array Props
xHeaderProps rows = join $ row <$> makeIndexed rows
  where
    row (Tuple rowIndex cells) = whiteCells rowIndex <> (_.value $ mapAccumL go 2 cells)
      where
        go colIndex (XHeaderCell c) =
          { accum: colIndex + c.colspan
          , value: defProps
            { row = rowIndex
            , col = colIndex
            , renderer = renderSetClass $ case c.ordinate of
                Nothing -> "xNoOrdinate"
                Just _  -> "xOrdinate"
            }
          }

xOrdsProps :: Table -> Array Props
xOrdsProps (Table tbl) = whiteCells rowIndex <> (cell <$> makeIndexed tbl.tableXOrdinates)
  where
    rowIndex = length tbl.tableXHeader
    cell (Tuple index (Ordinate o)) = defProps
      { row = rowIndex
      , col = index + 2
      , renderer = renderSetClass "xOrdinateCode"
      }

yOrdsProps :: Table -> BusinessData -> Array Props
yOrdsProps table@(Table tbl) bd = case tbl.tableYAxis of
  YAxisClosed _ ords -> join $ closed <$> makeIndexed ords
  YAxisCustom axId _ -> (join $ custom <$> getIndices (getCustomMembers axId bd)) <>
                        (customHeader <$> getIndices tbl.tableXOrdinates) <>
    [ defProps
      { row = firstRow
      , col = 0
      , renderer = renderSetClass "yAbstract"
      }
    , defProps
      { row = firstRow
      , col = 1
      , renderer = renderHtml "yAbstractCode"
      }
    ]
  where
    customHeader index = defProps
      { row = firstRow
      , col = index + 2
      , renderer = renderSetClass "shaded"
      }
    custom index =
      [ defProps
        { row = firstRow + 1 + index
        , col = 0
        , renderer = renderSetClass "yOrdinate"
        }
      , defProps
        { row = firstRow + 1 + index
        , col = 1
        , renderer = renderHtml "yOrdinateCode"
        }
      ]
    closed (Tuple index (Ordinate o)) =
      [ defProps
        { row = firstRow + index
        , col = 0
        , renderer = renderHtml $ if o.ordinateIsAbstract then "yAbstract" else "yOrdinate"
        }
      , defProps
        { row = firstRow + index
        , col = 1
        , renderer = renderSetClass if o.ordinateIsAbstract then "yAbstract" else "yOrdinateCode"
        }
      ]
    firstRow = length tbl.tableXHeader + 1

cellProps :: S -> Table -> BusinessData -> Array Props
cellProps s table bd = join $ procRow <$> makeIndexed (fromMaybe [] $ getCellTable s table bd)
  where
    procRow (Tuple r row) = procCell r <$> makeIndexed row
    procCell r (Tuple c cell) = let coords = toHotCoords table c r in
      { row: coords.row
      , col: coords.col
      , readOnly: case cell of
          ShadedCell    -> true
          FactCell _ _  -> false
          YMemberCell _ -> false
          NoCell        -> true
      , renderer: case cell of
          ShadedCell       -> renderSetClass "shaded"
          FactCell _ dType -> case dType of
            BooleanData    -> renderer "autocomplete"
            DateData       -> renderer "text"
            IntegerData    -> renderer "numeric"
            MonetaryData   -> renderer "numeric"
            PercentageData -> renderer "numeric"
            CodeData _     -> renderer "autocomplete"
            StringData     -> renderer "text"
            NumberData     -> renderer "numeric"
          YMemberCell _    -> renderer "text"
          NoCell           -> renderSetClass "noCell"
      , type: case cell of
          ShadedCell       -> "text"
          FactCell _ dType -> case dType of
            BooleanData    -> "autocomplete"
            DateData       -> "date"
            IntegerData    -> "numeric"
            MonetaryData   -> "numeric"
            PercentageData -> "numeric"
            CodeData _     -> "autocomplete"
            StringData     -> "text"
            NumberData     -> "numeric"
          YMemberCell _    -> "text"
          NoCell           -> "text"
      , format: case cell of
          ShadedCell       -> ""
          FactCell _ dType -> case dType of
            BooleanData    -> ""
            DateData       -> ""
            IntegerData    -> "0"
            MonetaryData   -> "0,0.00"
            PercentageData -> "0.00%"
            CodeData _     -> ""
            StringData     -> ""
            NumberData     -> "0"
          YMemberCell _    -> ""
          NoCell           -> ""
      , dateFormat: case cell of
          FactCell _ DateData -> "YYYY-MM-DD"
          _                   -> ""
      , source: case cell of
          FactCell _ BooleanData      -> snd <$> boolValueMap
          FactCell _ (CodeData pairs) -> snd <$> pairs
          _                           -> []
      }
