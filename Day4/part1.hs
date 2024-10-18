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
computeWinnings guesses winners = if k > 0 then 2 ^ (k-1) else 0
                                            where k = getMatches guesses winners

getMatches:: [Int] -> [Int] -> Int
getMatches guesses winners = length $ guesses `intersect` winners

getCardValue:: String -> Int
getCardValue s = computeWinnings selected winners where 
                                                    selected = getSelectedNumbers s
                                                    winners  = getWinningNumbers s


part1:: [String] -> Int
part1 = foldr (\s accum -> accum + (getCardValue s)) 0

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "input.txt" else head args
    contents <- readFile filename

    print $ part1 $ lines contents