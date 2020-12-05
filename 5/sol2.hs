{-# LANGUAGE LambdaCase #-}
import MUtil
import Data.List ((\\))

bspToBinary :: String -> Int
bspToBinary
   = toDec . map (\case
               'B' -> '1'
               'F' -> '0'
               'R' -> '1'
               'L' -> '0')

main = do
   nums <- (bspToBinary <$>) <$> getInputs
   print $ [minimum nums..maximum nums] \\ nums
