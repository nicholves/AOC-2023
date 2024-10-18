import System.Environment (getArgs)
import Data.Text (splitOn, unpack, pack)
import Data.Char
import Data.List(intersect)

chopCard:: String -> String
chopCard s = unpack (splitOn (pack ":") (pack s) !! 1)

getSelectedNumbers:: String -> [Int]
getSelectedNumbers s = getSpacedNumbers $ unpack (splitOn (pack "|") (pack $ chopCard s) !! 1)

getWinningNumbers:: String -> [Int]
getWinningNumbers s = getSpacedNumbers $ unpack $ head $ splitOn (pack "|") (pack $ chopCard s)

atoi:: String -> Int
atoi s = read (unpack $ head $ splitOn (pack " ") (pack s)) :: Int

getSpacedNumbers:: String -> [Int]
getSpacedNumbers "" = []
getSpacedNumbers (' ':t)  = getSpacedNumbers t
getSpacedNumbers s  = fst (chopDigitsAndValue s) : getSpacedNumbers (snd (chopDigitsAndValue s))


chopDigitsAndValue:: String -> (Int, String)
chopDigitsAndValue s = (atoi s, chopDigits s)

chopDigits:: String -> String
chopDigits "" = []
chopDigits (h:t) 
    | isDigit h = chopDigits t
    | otherwise = t

computeWinnings:: [Int] -> [Int] -> Int
computeWinnings = getMatches

getMatches:: [Int] -> [Int] -> Int
getMatches guesses winners = length $ guesses `intersect` winners

getCardValue:: String -> Int
getCardValue s = computeWinnings selected winners where 
                                                    selected = getSelectedNumbers s
                                                    winners  = getWinningNumbers s

createCopies::  [(String, Int)] -> Int -> Int -> [(String, Int)]
createCopies [] _ _ = []
createCopies a 0 _ = a
createCopies a _ 0 = a
createCopies ((s, v):t) depth copiesToCreate = (s, v+copiesToCreate) : createCopies t (depth - 1) copiesToCreate

computeNumCopies:: [(String, Int)] -> [(String, Int)]
computeNumCopies [h] = [h]
computeNumCopies (h:t) = h : computeNumCopies (createCopies t (getCardValue (fst h)) (snd h))

fillTuples:: [String] -> [(String, Int)]
fillTuples [] = []
fillTuples s = map (\x -> (x, 1)) s


part2:: [String] -> Int
part2 lines = foldr (\(s, value) accum -> value + accum) 0 (computeNumCopies (fillTuples lines))

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "input.txt" else head args
    contents <- readFile filename

    print $ part2 $ lines contents