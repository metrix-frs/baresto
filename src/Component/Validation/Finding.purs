module Component.Validation.Finding
  ( renderFinding
  , renderHoleCoords
  ) where

import Prelude

import Data.Maybe
import Data.Tuple
import Data.String (take)
import Data.Foldable (intercalate)
import Data.String (joinWith)

import Halogen
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Events.Indexed as E

import Api.Schema.Validation
import Utils (cls)

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
renderTerm f = case f of
    FHole h ->
      [ renderHole h
      ]
    FSum hs ->
      intercalate [opv ", "] (pure <<< renderHole <$> hs)
    FMember s ->
      [ renderValue s
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
      [ renderValue (show v)
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
      "("  -> H.div [ cls "lop lop-lparen" ] []
      ")"  -> H.div [ cls "lop lop-rparen" ] []
      ">=" -> H.div [ cls "lop lop-ge" ] []
      "<=" -> H.div [ cls "lop lop-le" ] []
      "="  -> H.div [ cls "lop lop-eq" ] []
      "+"  -> H.div [ cls "lop lop-plus" ] []
      "-"  -> H.div [ cls "lop lop-minus" ] []
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
  FHole h -> false
  FSum hs -> true
  FMember s -> false
  FUnary op f -> false
  FBinary op lhs rhs -> true
  FIfThenElse cond t e -> false
  FBoolean v -> false
  FNumber  v -> false
  FString  v -> false
  FModuleParam p v -> false
  FSet fs -> false


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
    [ case h.holeData of
        Just d -> H.text d
        Nothing -> H.span [ cls "missing", P.title "Not filled in, treated as zero." ] [ H.text "0.00" ]
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
          Just z ->
            [ H.text ", "
            , H.b_ [ H.text "s" ]
            , H.text z
            ]
          Nothing ->
            []
      ) <>
      [ H.text ")"
      ]
