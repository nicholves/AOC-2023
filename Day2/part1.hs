import Data.List
import Data.Char
import Data.Text (splitOn, replace, pack, unpack, find)
import Data.Maybe (isJust)
import System.Environment

data Roll = Roll {
    redValue    :: Int
    ,greenValue :: Int
    ,blueValue  :: Int

} deriving (Show)

removeNonDigits:: String -> String
removeNonDigits [] = []
removeNonDigits (h:t) 
    | isDigit h = h : removeNonDigits t
    | otherwise = removeNonDigits t

startsWith:: String -> String -> Bool
startsWith "" _ = True
startsWith (x:xs) (y:ys) 
    | x == y = startsWith xs ys
    | otherwise = False

endsWith:: String -> String -> Bool
endsWith needle haystack = startsWith (reverse needle) (reverse haystack)

atoi :: String -> Int
atoi s = read s :: Int

findStrEndingWith:: String -> [String] -> Maybe String
findStrEndingWith _ [] = Nothing
findStrEndingWith needle (h:t)
    | endsWith needle h = Just h
    | otherwise = findStrEndingWith needle t

parseRoll:: String -> Roll
parseRoll str = let colours = map (unpack . replace (pack " ") (pack "")) (splitOn (pack ",") (pack str))
                    greenStr = findStrEndingWith "green" colours
                    redStr = findStrEndingWith "red" colours
                    blueStr = findStrEndingWith "blue" colours

                    greenAmount =  maybe 0 (atoi . removeNonDigits) greenStr
                    blueAmount  = maybe 0 (atoi . removeNonDigits) blueStr
                    redAmount   = maybe 0 (atoi . removeNonDigits) redStr
                in Roll redAmount greenAmount blueAmount

validateRolls:: [String] -> Bool
validateRolls str = let rolls = map parseRoll str
                    in all (\x -> (redValue x <= 12) && (blueValue x <= 14) && (greenValue x <= 13)) rolls

validateGame:: String -> Bool
validateGame str = do
                    let removeSpaces = replace (pack " ") (pack "")
                        game = unpack . removeSpaces $ (head (tail (splitOn (pack ":") (pack str))))
                        rolls = map unpack (splitOn (pack ";") (pack game))
                        in validateRolls rolls

validateGames:: [String] -> [Bool]
validateGames = map validateGame

addValidIndexes:: [Bool] -> Int -> Int -> Int
addValidIndexes [] _ accum = accum
addValidIndexes (h:t) index accum 
    | h = addValidIndexes t (index + 1) (accum + index)
    | otherwise = addValidIndexes t (index + 1) accum


part1:: [String] -> Int
part1 input = addValidIndexes list 1 0
    where 
        list = validateGames input


main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "input.txt" else head args
    contents <- readFile filename

    print $ part1 $ lines contents