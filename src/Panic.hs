module Panic (
  panic,
  undesugaredPanic,
  missingGlobalResourceAnnotationPanic,
  missingLocalResourceAnnotationPanic
) where

panic :: String -> a
panic msg = error $ "Internal error: " ++ msg

undesugaredPanic :: String -> String -> a
undesugaredPanic fname index = panic $ "resource operator was not desugared in `" ++ fname ++ "': " ++ show index

missingGlobalResourceAnnotationPanic :: String -> a
missingGlobalResourceAnnotationPanic fname = panic $ "missing global resource annotation in `" ++ fname ++ "'"

missingLocalResourceAnnotationPanic :: String -> a
missingLocalResourceAnnotationPanic fname = panic $ "missing local resource annotation in `" ++ fname ++ "'"
