module Component.Validation.Finding
  ( renderFinding
  , renderHoleCoords
  ) where

import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Api.Schema.BusinessData.Value (Value(Value))
import Api.Schema.Table (DataType(..))
import Api.Schema.Validation (Finding(Finding), Formula(FBinary, FUnary, FSet, FModuleParam, FString, FNumber, FBoolean, FIfThenElse, FMember, FSum, FHole), Hole(Hole), HoleCoordX(HCX), HoleCoordY(HCYCustom, HCYClosed), HoleCoordZ(HCZSubset, HCZCustom, HCZClosed, HCZSingleton), HoleCoords(HoleCoords))
import Data.Foldable (intercalate)
import Data.Maybe (fromMaybe, Maybe(Nothing, Just))
import Data.String (take)
import Data.Tuple (Tuple(Tuple))
import Halogen (ComponentHTML)
import Lib.Table (boolValueMap, lookupByFst)
import Prelude
import Utils (cls, tryFormatNumber)

renderFinding :: forall f. Finding -> ComponentHTML f
renderFinding (Finding f) = H.li_ $
  [ H.b_ [ H.text $ f.finCode <> ": " ]
  , H.text f.finMessage
  , H.br_ :: ComponentHTML f
  ] <> case f.finFormula of
         Just formula ->
          [ renderFormula formula
          ]
         Nothing -> []

renderFormula :: forall f. Formula -> ComponentHTML f
renderFormula f = H.div [ cls "formula" ] $ renderTerm f

renderTerm :: forall f. Formula -> Array (ComponentHTML f)
renderTerm f' = case f' of
    FHole h ->
      [ renderHole h
      ]
    FSum hs ->
      intercalate [opv ", "] (pure <<< renderHole <$> hs)
    FMember _ label ->
      [ renderValue label
      ]
    FUnary op f ->
      [ opv op
      , opv "("
      ] <> renderTerm f <>
      [ opv ")"
      ]
    FBinary op lhs rhs ->
      let left = if binaryNeedParen op lhs
            then paren lhs
            else renderTerm lhs
          right = if binaryNeedParen op rhs
            then paren rhs
            else renderTerm rhs
      in left <> [ opv op ] <> right
    FIfThenElse cond t e ->
      [ opv "if "
      ] <> paren cond <>
      [ opv " then "
      ] <> paren t <>
      [ opv " else "
      ] <> paren e
    FBoolean v ->
      [ renderValue (show v)
      ]
    FNumber  v ->
      [ renderValue $ tryFormatNumber 2 v
      ]
    FString  v ->
      [ renderValue v
      ]
    FModuleParam p v ->
      [ renderModuleParam p v
      ]
    FSet fs ->
      [ opv "["
      ] <> intercalate [opv ","] (renderTerm <$> fs) <>
      [ opv "]"
      ]
  where
    opv v = case v of
      "("    -> H.div [ cls "lop lop-lparen" ] []
      ")"    -> H.div [ cls "lop lop-rparen" ] []
      ">="   -> H.div [ cls "lop lop-ge" ] []
      "<="   -> H.div [ cls "lop lop-le" ] []
      "="    -> H.div [ cls "lop lop-eq" ] []
      "+"    -> H.div [ cls "lop lop-plus" ] []
      "-"    -> H.div [ cls "lop lop-minus" ] []
      "elem" -> H.div [ cls "lop lop-elem" ] []
      _ -> H.span [ cls "op" ] [ H.text v ]
    paren term = if needParen term
      then
        [ opv "("
        ] <> renderTerm term <>
        [ opv ")"
        ]
      else
        renderTerm term

binaryNeedParen :: String -> Formula -> Boolean
binaryNeedParen op f = case Tuple op $ getTermOp f of
  Tuple "+" (Just "+") -> false
  Tuple "=" (Just _) -> false
  Tuple "+" (Just "*") -> false
  Tuple "-" (Just "*") -> false
  Tuple "+" (Just "div") -> false
  Tuple "-" (Just "div") -> false
  _ -> true

needParen :: Formula -> Boolean
needParen f = case f of
  FHole _ -> false
  FSum _ -> true
  FMember _ _ -> false
  FUnary _ _ -> false
  FBinary _ _ _ -> true
  FIfThenElse _ _ _ -> false
  FBoolean _ -> false
  FNumber  _ -> false
  FString  _ -> false
  FModuleParam _ _ -> false
  FSet _ -> false


getTermOp :: Formula -> Maybe String
getTermOp f = case f of
  FUnary op _ -> Just op
  FBinary op _ _ -> Just op
  _ -> Nothing

renderValue :: forall f. String -> ComponentHTML f
renderValue value = H.div [ cls "value" ]
  [ H.span [ cls "val" ] [ H.text value ]
  ]

renderModuleParam :: forall f. String -> String -> ComponentHTML f
renderModuleParam p v = H.div [ cls "param" ]
  [ H.text ""
  , H.br_
  , H.text v
  , H.br_
  , H.text p
  ]

renderHole :: forall f. Hole -> ComponentHTML f
renderHole (Hole h) = H.div [ cls "hole" ]
  [ H.span [ cls "holetable" ]
    [ H.text h.holeTemplate ]
  , H.br_
  , H.div [ cls "data" ]
    [ let default = case h.holeDataType of
            BooleanData -> "false"
            DateData -> ""
            IntegerData -> "0"
            MonetaryData -> "0.00"
            PercentageData -> "0.00"
            CodeData _ -> ""
            StringData -> ""
            NumberData -> "0"
      in case h.holeData of
        Value v -> case v.valueData of
          Nothing -> H.span [ cls "missing", P.title "Not filled in, using default." ] [ H.text default ]
          Just d  -> H.text $ case h.holeDataType of
            BooleanData -> fromMaybe "" $ lookupByFst d boolValueMap
            DateData -> d
            IntegerData -> tryFormatNumber 0 d
            MonetaryData -> tryFormatNumber 2 d
            PercentageData -> tryFormatNumber 2 d
            CodeData cd -> fromMaybe d $ lookupByFst d cd
            StringData -> d
            NumberData -> tryFormatNumber 0 d
    ]
  , H.div [ cls "decimals", P.title "Precision (decimal places of absolute error)" ]
    [ H.text $ case h.holeData of
        Value v -> case h.holeDataType of
          MonetaryData -> show $ fromMaybe 2 $ v.valuePrecision
          PercentageData -> show $ fromMaybe 4 $ v.valuePrecision
          _ -> ""
    ]
  , H.br_
  , H.span [ cls "holecoords" ]
    [ renderHoleCoords h.holeCoords ]
  ]

renderHoleCoords :: forall f. HoleCoords -> ComponentHTML f
renderHoleCoords (HoleCoords x y z) = H.span_
  let xStr = case x of
        HCX i ord              -> ord
      yStr = case y of
        HCYClosed i ord        -> ord
        HCYCustom cmId rowKeys -> take 8 cmId
      zStr = case z of
        HCZSingleton           -> Nothing
        HCZClosed i ord        -> Just ord
        HCZCustom cmId cm      -> Just $ take 8 cmId
        HCZSubset smId sm      -> Just $ show smId
  in  [ H.text "("
      , H.b_ [ H.text "r" ]
      , H.text yStr
      , H.text ", "
      , H.b_ [ H.text "c" ]
      , H.text xStr
      ] <> (
        case zStr of
          Just z' ->
            [ H.text ", "
            , H.b_ [ H.text "s" ]
            , H.text z'
            ]
          Nothing ->
            []
      ) <>
      [ H.text ")"
      ]
