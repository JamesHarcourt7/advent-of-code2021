import System.IO  
import System.Directory  
import Data.List 

main :: IO ()
main = do 
    handle <- openFile "day02.txt" ReadMode
    contents <- hGetContents handle
    let inputs = lines contents
        answer1 = partOne inputs
        answer2 = partTwo inputs
    
    putStr ("Part one: " ++ (show answer1) ++ "\n")
    putStr ("Part two: " ++ (show answer2) ++ "\n")
    hClose handle


partOne :: [String] -> Int
partOne inputs = (sum forward) * (sum normalised)
    where (forward, down, up) = parseDirections inputs
          normalised = [x | x <- down] ++ [0 - x | x <- up]

parseDirections :: [String] -> ([Int], [Int], [Int])
parseDirections inputs = (forward, down, up)
    where forward = [read(words(x) !! 1) | x <- inputs, head (words x) == "forward"]
          down = [read(words(x) !! 1) | x <- inputs, head (words x) == "down"]
          up = [read(words(x) !! 1) | x <- inputs, head (words x) == "up"]

partTwo :: [String] -> Int
partTwo inputs = 0
