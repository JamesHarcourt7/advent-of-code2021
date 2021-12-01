import System.IO  
import System.Directory  
import Data.List 

main :: IO ()
main = do 
    handle <- openFile "day01.txt" ReadMode
    contents <- hGetContents handle
    let inputs = lines contents
        answer1 = partOne inputs
    
    putStr ("Part one: " ++ (show answer1) ++ "\n")
    hClose handle

aux :: String -> String -> Bool
aux x y = (read y) - (read x) > 0

partOne :: [String] -> Int
partOne inputs = length (filter (\x -> x) (zipWith aux inputs (tail inputs)))
