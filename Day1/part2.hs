import Data.Char
import Data.List
import Data.Maybe
import System.Environment (getArgs)

startsWith:: String -> String -> Bool
startsWith _ "" = True
startsWith "" _ = False
startsWith (h:t) (x:xs)
    | h == x = startsWith t xs
    | otherwise = False

getFirstDigit:: String -> Char
getFirstDigit (h:t)  
    | isDigit h = h
    | otherwise = getFirstDigit t

getLastDigit:: String -> Char
getLastDigit str = getFirstDigit $ reverse str

createCalibrationValues:: String -> String
createCalibrationValues str = (getFirstDigit str) : [(getLastDigit str)]

atoi :: String -> Int
atoi s = read s :: Int

part1:: [String] -> Int
part1 input = sum $ map atoi (map createCalibrationValues input)

toAllDigits:: String -> String 
toAllDigits "" = ""
toAllDigits (h:t)
    | isDigit h = h : toAllDigits t
    | startsWith str "one" = '1' : toAllDigits t
    | startsWith str "two" = '2' : toAllDigits t
    | startsWith str "three" = '3' : toAllDigits t
    | startsWith str "four" = '4' : toAllDigits t
    | startsWith str "five" = '5' : toAllDigits t
    | startsWith str "six" = '6' : toAllDigits t
    | startsWith str "seven" = '7' : toAllDigits t
    | startsWith str "eight" = '8' : toAllDigits t
    | startsWith str "nine" = '9' : toAllDigits t
    | otherwise = toAllDigits t
    where
        str = (h:t)

part2:: [String] -> Int
part2 input = part1 (map toAllDigits input)


main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "input.txt" else head args
    contents <- readFile filename

    print $ part2 $ lines contents