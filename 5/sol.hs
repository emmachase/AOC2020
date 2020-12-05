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
  content <- readFile "input.txt"
  let numLines = lines content
  let nums = bspToBinary <$> numLines

  print $ maximum nums
