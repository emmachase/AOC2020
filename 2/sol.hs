import Parsing

parsePassword :: Parser (Int, Int, Char, String)
parsePassword = do
   min <- number
   char '-'
   max <- number

   spaces

   cc <- anyChar
   char ':'

   spaces

   password <- word

   return (min, max, cc, password)

checkPassword :: String -> Bool
checkPassword p =
   let (min, max, cc, password) = expectParse parsePassword p
       cnt = length $ filter (==cc) password
   in (cnt >= min) && (cnt <= max)

main = do
   content <- readFile "input.txt"
   let passwords = lines content

   print . length . filter id $ checkPassword <$> passwords


