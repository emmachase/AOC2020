{-# LANGUAGE LambdaCase #-}
import MUtil

bspToBinary :: String -> Int
bspToBinary
   = toDec . map (\case
               'B' -> '1'
               'F' -> '0'
               'R' -> '1'
               'L' -> '0')

main = do
   nums <- (bspToBinary <$>) <$> getInputs
   print $ maximum nums
