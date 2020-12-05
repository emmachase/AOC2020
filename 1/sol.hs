import MUtil
main = do
   nums <- (read <$>) <$> getInputs

   let num = head [ x * y
                  | x <- nums
                  , y <- nums
                  , x + y == 2020]

   print num
