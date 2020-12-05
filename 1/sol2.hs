import MUtil
main = do
   nums <- read <$$> getInputs

   let num = head [ x * y * z
                  | x <- nums
                  , y <- nums
                  , z <- nums
                  , x + y + z == 2020]

   print num
