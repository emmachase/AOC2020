main = do
  content <- readFile "input.txt"
  let numLines = lines content
  let nums = read <$> numLines :: [Int]

  let num = head [ x * y
                 | x <- nums
                 , y <- nums
                 , x + y == 2020]

  print num
