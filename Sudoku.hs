module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.List.Split

-------------------------------------------------------------------------

main :: IO ()
main = do
  printSudoku example
  printSudoku example2
  printSudoku allBlankSudoku

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku sudoku = (length (rows sudoku) == 9) &&
                  (all (\x -> length x == 9) (rows sudoku)) &&
                  (areElementsValid sudoku)

areElementsValid :: Sudoku -> Bool
areElementsValid sudoku = all (\x -> (all (\y ->
                          (y == Nothing)||
                          (y == Just 1) ||
                          (y == Just 2) ||
                          (y == Just 3) ||
                          (y == Just 4) ||
                          (y == Just 5) ||
                          (y == Just 6) ||
                          (y == Just 7) ||
                          (y == Just 8) ||
                          (y == Just 9)) (x))) (rows sudoku)

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved sudoku = not (any (\x -> (any (\y -> y == Nothing) (x))) (rows sudoku))

-------------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku sudoku = putStrLn (innerList (rows sudoku))

innerList :: [[Maybe Int]] -> String
innerList list = unlines (map (listToString) (list))

listToString :: [Maybe Int] -> String
listToString list = map convertToChar list

convertToChar :: Maybe Int -> Char
convertToChar input | input == Just 1 = chr 49
                    | input == Just 2 = chr 50
                    | input == Just 3 = chr 51
                    | input == Just 4 = chr 52
                    | input == Just 5 = chr 53
                    | input == Just 6 = chr 54
                    | input == Just 7 = chr 55
                    | input == Just 8 = chr 56
                    | input == Just 9 = chr 57
                    | input == Nothing = chr 46

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku file = do
                    contents <- readFile file
                    if isSudoku (Sudoku (stringListToMatrix (lines contents)))
                    then return (Sudoku (stringListToMatrix (lines contents)))
                    else error "File does not contain a Sudoku"

convertToInt :: Char -> Maybe Int
convertToInt input  | input == chr 49 = Just 1
                    | input == chr 50 = Just 2
                    | input == chr 51 = Just 3
                    | input == chr 52 = Just 4
                    | input == chr 53 = Just 5
                    | input == chr 54 = Just 6
                    | input == chr 55 = Just 7
                    | input == chr 56 = Just 8
                    | input == chr 57 = Just 9
                    | input == chr 46 = Nothing

--fileToString :: FilePath -> String
--fileToString file = return (readFile file)

stringListToMatrix :: [String] -> [[Maybe Int]]
stringListToMatrix charMatrix = map stringToList charMatrix

stringToList :: String -> [Maybe Int]
stringToList string = map convertToInt string

-------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = undefined

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

{-prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = undefined

isOkayBlock :: Block -> Bool
isOkayBlock = undefined

blocks :: Sudoku -> [Block]
blocks = undefined

isOkay :: Sudoku -> Bool
isOkay = undefined-}



-------------------------------------------------------------------------
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 1,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

example2 :: Sudoku
example2 =
    Sudoku (replicate 9 (replicate 9 (Just 1)))
