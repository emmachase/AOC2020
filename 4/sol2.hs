import Data.Maybe
import Data.Either (isRight)
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

getField :: Passport -> String -> Maybe (String, String)
getField pp field = first ((==field) . fst) pp

validateField :: Passport -> String -> (String -> Bool) -> Maybe ()
validateField pp field validator = do
   (_, value) <- getField pp field
   if validator value
      then Just ()
      else Nothing

checkNumStr :: Int -> Int -> String -> Bool
checkNumStr min max s = isRight n && valid
   where n = runParse number s
         valid = checkRange min max $ fromRight n

checkPassport :: Passport -> Maybe ()
checkPassport pp = do
   validateField pp "byr" (checkNumStr 1920 2002) -- (Birth Year)
   validateField pp "iyr" (checkNumStr 2010 2020) -- (Issue Year)
   validateField pp "eyr" (checkNumStr 2020 2030) -- (Expiration Year)
   validateField pp "hgt"
      (\val ->
         let parseVal = do
               n <- number
               unit <- many1 letter
               return (n, unit)
             pval = runParse parseVal val
         in isRight pval &&
            let (n, unit) = fromRight pval
            in (unit == "cm" && checkRange 150 193 n)
            || (unit == "in" && checkRange 59  76  n)
      ) -- (Height)
   validateField pp "hcl"
      (\val ->
         let checkParse = do
               char '#'
               count 6 (oneOf "0123456789abcdef")
               eof
         in isRight $ runParse checkParse val
      ) -- (Hair Color)
   validateField pp "ecl" (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]) -- (Eye Color)
   validateField pp "pid"
      (\val ->
         let checkParse = do
               count 9 digit
               eof
         in isRight $ runParse checkParse val
      ) -- (Passport ID)

   -- Haha funnie
   -- getField pp "cid" -- (Country ID)

main = do
   contents <- getInput
   let passports = fromRight $ runParse passportList contents

   let valids = filter isJust $ checkPassport <$> passports
   print $ length valids
