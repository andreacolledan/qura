module Parser.Annotation where

import PQ.Index
import Parser.Core
import Parser.Index (indexExpression)
import Control.Applicative (optional, Alternative ((<|>)))

--- Effect annotations ---

localMetricAnnotation :: Parser (Maybe Index)
localMetricAnnotation = whenLocalAnalysis $ braces indexExpression

globalMetricAnnotation :: Parser (Maybe Index)
globalMetricAnnotation = whenGlobalAnalysis $ brackets indexExpression

functionAnnotation :: Parser (Maybe (Index, Index))
functionAnnotation = whenGlobalAnalysis $ brackets $ do
  i <- indexExpression
  j <- withDefault Identity $ optional $ comma *> indexExpression
  return (i, j)

listLengthAnnotation :: Parser (IVarId, Index)
listLengthAnnotation = brackets $ do
    id <- identifier <|> (underscore >> return "_") -- allow to parse "_" as a list type index
    lessSign
    i <- indexExpression
    return (id, i)
