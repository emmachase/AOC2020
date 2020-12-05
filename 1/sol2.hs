main = do
   content <- readFile "input.txt"
   let numLines = lines content
   let nums = read <$> numLines :: [Int]

   let num = head [ x * y * z
                  | x <- nums
                  , y <- nums
                  , z <- nums
                  , x + y + z == 2020]

   print num
