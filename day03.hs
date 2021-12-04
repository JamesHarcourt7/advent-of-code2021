import System.IO  
import System.Directory  
import Data.List 

main :: IO ()
main = do 
    handle <- openFile "day03.txt" ReadMode
    contents <- hGetContents handle
    let inputs = lines contents
        answer1 = partOne inputs
        answer2 = partTwo inputs
    
    putStr ("Part one: " ++ (show answer1) ++ "\n")
    putStr ("Part two: " ++ (show answer2) ++ "\n")
    hClose handle

partOne :: [String] -> Int
partOne inputs = (toDecimal common (length common - 1)) * (toDecimal (flipped) (length flipped - 1) )
    where common = map (aux1) (getColumns inputs [])
          flipped = map flipBin common

getColumns :: [String] -> [Int] -> [Int]
getColumns [] acc = acc
getColumns (x:xs) acc | length acc == 0 = getColumns xs (zipWith (rowToInt) x [0 | n <- [1..length x]]) 
                      | otherwise = getColumns xs (zipWith (rowToInt) x acc)

rowToInt :: Char -> Int -> Int
rowToInt x n | x == '1' = n + 1
             | otherwise = n - 1

aux1 :: Int -> Int
aux1 n | n >= 0 = 1
       | otherwise = 0

toDecimal :: [Int] -> Int -> Int
toDecimal (x:[]) 0 = x 
toDecimal _ 0 = error "invalid function call- n too big"
toDecimal (x:[]) _ = error "invalid function call- list too short"
toDecimal (x:xs) n = (x * (2 ^ n)) + toDecimal xs (n - 1)

flipBin :: Int -> Int
flipBin 1 = 0
flipBin 0 = 1
flipBin _ = error "input should be decimal"

partTwo :: [String] -> Int
partTwo inputs = 0
