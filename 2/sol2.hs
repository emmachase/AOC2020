import Parsing
import MUtil

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
       a = Just cc == (password `nth` (min - 1))
       b = Just cc == (password `nth` (max - 1))
   in a /= b

main = do
   passwords <- getInputs

   print . length . filter id $ checkPassword <$> passwords
