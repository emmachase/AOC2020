import MUtil

main = do
   rows <- getInputs

   let slope = cycle <$> rows
   let len = length slope


   let cell r = slope !! r !! (r*3)
   let go r = '#' == cell r

   let trees = go <$> [1..len-1]

   print . length $ filter id trees

