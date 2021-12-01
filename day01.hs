import System.IO  
import System.Directory  
import Data.List 

main :: IO ()
main = do 
    handle <- openFile "day01.txt" ReadMode
    contents <- hGetContents handle
    let inputs = lines contents
        answer1 = partOne inputs
        answer2 = partTwo inputs
    
    putStr ("Part one: " ++ (show answer1) ++ "\n")
    putStr ("Part two: " ++ (show answer2) ++ "\n")
    hClose handle

aux :: String -> String -> Bool
aux x y = (read y) - (read x) > 0

sumWindow :: (String, String, String) -> Int
sumWindow (x, y, z) = (read x) + (read y) + (read z)

aux2 :: Int -> Int -> Bool
aux2 x y = y - x > 0

partOne :: [String] -> Int
partOne inputs = length (filter (\x -> x) (zipWith aux inputs (tail inputs)))

partTwo :: [String] -> Int
partTwo inputs = length (filter (\x -> x) (zipWith aux2 summed (tail summed)))
    where triples = zip3 inputs (tail inputs) (tail (tail inputs))
          summed = (map (sumWindow) triples)
