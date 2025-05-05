module Panic (
  undesugaredPanic,
  missingGlobalResourceAnnotationPanic,
  missingLocalResourceAnnotationPanic
) where

undesugaredPanic :: String -> String -> a
undesugaredPanic fname index = error $ "Internal error: resource operator was not desugared in `" ++ fname ++ "': " ++ show index

missingGlobalResourceAnnotationPanic :: String -> a
missingGlobalResourceAnnotationPanic fname = error $ "Internal error: missing global resource annotation in `" ++ fname ++ "'"

missingLocalResourceAnnotationPanic :: String -> a
missingLocalResourceAnnotationPanic fname = error $ "Internal error: missing local resource annotation in `" ++ fname ++ "'"
