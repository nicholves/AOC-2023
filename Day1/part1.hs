import Data.Char
import System.Environment (getArgs)

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

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "input.txt" else head args
    contents <- readFile filename

    print $ part1 $ lines contents