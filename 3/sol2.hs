import MUtil
import Data.Maybe

main = do
   rows <- getInputs

   let slope = cycle <$> rows

   let cell dx dy r = slope `nth` (dy*r) `nthM` (dx*r)
   let runSled (dx, dy) =
         (== Just '#') <$>
            takeWhile isJust
            (cell dx dy <$> [1..])

   let trees = runSled <$> [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

   print . product $ length . filter id <$> trees
