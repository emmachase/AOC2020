import MUtil
import Data.Maybe

main = do
   content <- readFile "input.txt"
   let rows = lines content

   let slope = cycle <$> rows
   let len = length slope


   let cell dx dy r = slope `nth` (dy*r) `nthM` (dx*r)
   let trees =
         (\(dx, dy) ->
               (== Just '#') <$>
               takeWhile isJust
               (cell dx dy <$> [1..len-1]))

         <$> [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

   print . product $ length . filter id <$> trees

