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


getPower:: String -> Int
getPower str = do
                let removeSpaces = replace (pack " ") (pack "")
                    game = unpack . removeSpaces $ (head (tail (splitOn (pack ":") (pack str))))
                    rollsStrs = map unpack (splitOn (pack ";") (pack game))
                    rolls = map parseRoll rollsStrs
                    maxRoll = foldr (\x y -> Roll {redValue = (max (redValue x) (redValue y)), greenValue = (max (greenValue x) (greenValue y)), blueValue = (max (blueValue x) (blueValue y))}) (Roll 0 0 0) rolls
                    in redValue maxRoll * greenValue maxRoll * blueValue maxRoll

getPowers:: [String] -> [Int]
getPowers = map getPower


part2:: [String] -> Int
part2 input = sum list
    where 
        list = getPowers input


main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "input.txt" else head args
    contents <- readFile filename

    print $ part2 $ lines contents