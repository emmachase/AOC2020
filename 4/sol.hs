import Data.Maybe
import Parsing
import MUtil

type KVP = (String, String)
type Passport = [KVP] -- Key Value pairs

kvpair :: Parser KVP
kvpair = do
   k <- key
   v <- value
   return (k, v)
   where key   = manyTill anyChar (char ':')
         value = manyTill anyChar space

passport :: Parser Passport
passport = many1 . try $ lookAhead (noneOf "\n") *> kvpair

passportList :: Parser [Passport]
passportList = many (passport <* optional (char '\n'))

requireField :: Passport -> String -> Maybe ()
requireField pp field = () <$ first ((==field) . fst) pp

checkPassport :: Passport -> Maybe ()
checkPassport pp = do
   requireField pp "byr" -- (Birth Year)
   requireField pp "iyr" -- (Issue Year)
   requireField pp "eyr" -- (Expiration Year)
   requireField pp "hgt" -- (Height)
   requireField pp "hcl" -- (Hair Color)
   requireField pp "ecl" -- (Eye Color)
   requireField pp "pid" -- (Passport ID)
   -- Haha funnie
   -- requireField pp "cid" -- (Country ID)

main = do
   contents <- readFile "input.txt"
   let passports = fromRight $ runParse passportList contents

   let valids = filter isJust $ checkPassport <$> passports
   print $ length valids
