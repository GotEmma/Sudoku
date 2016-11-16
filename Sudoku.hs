module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.List
import Data.List.Split

-------------------------------------------------------------------------

main :: IO ()
main = do
  printSudoku example
  printSudoku example2
  printSudoku allBlankSudoku

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

-- | allBlankSudoku is a sudoku with just blanks.
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- | isSudoku sud checks if sudoku is really a valid representation of a sudoku
-- | puzzle.
isSudoku :: Sudoku -> Bool
isSudoku sudoku = (length (rows sudoku) == 9) &&
                  (all (\x -> length x == 9) (rows sudoku)) &&
                  (areElementsValid sudoku)

-- Help function for isSudoku. Checks if the elements are 1-9 or blank.
areElementsValid :: Sudoku -> Bool
areElementsValid sudoku = all (\x -> ( all (\y -> (
                          case y of
                            Just n -> n `elem` [1..9]
                            Nothing -> True )) (x) )) (rows sudoku)

-- | isSolved sudoku checks if sud is already solved, i.e. there are no blanks.
isSolved :: Sudoku -> Bool
isSolved sudoku = not (any (\x -> (
                  any (\y -> y == Nothing) (x))) (rows sudoku))

-------------------------------------------------------------------------

-- | printSudoku sud prints a representation of the sudoku sud on the screen.
printSudoku :: Sudoku -> IO ()
printSudoku sud = putStrLn (maybeMatrixToString (rows sud))

maybeMatrixToString :: [[Maybe Int]] -> String
maybeMatrixToString matrix = unlines (map maybeListToString matrix)

maybeListToString :: [Maybe Int] -> String
maybeListToString list = map maybeToChar list

maybeToChar :: Maybe Int -> Char
maybeToChar (Just n) = chr (ord '0' + n)
maybeToChar Nothing = chr (ord '.')

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
cell = frequency [(9, return Nothing), (1, do elements maybeInts)]

maybeInts = [Just 1, Just 2, Just 3, Just 4, Just 5, Just 6, Just 7, Just 8, Just 9]

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku sudoku = isSudoku sudoku

type Block = [Maybe Int]

isOkayBlock :: Block -> Bool
isOkayBlock (x:[]) = True
isOkayBlock (x:xs) = if any (\y -> y == x) xs
                     then False
                     else isOkayBlock xs

blocks :: Sudoku -> [Block]
blocks sudoku = concat [rows sudoku, transpose (rows sudoku),
                        sudokuToBlock3x3 (rows sudoku)]

sudokuToBlock3x3 :: [[Maybe Int]] -> [Block]
sudokuToBlock3x3 sudoku = [listToSudoku (take 3 sudoku),
                          listToSudoku (take 3 (drop 3 sudoku)),
                          listToSudoku (drop 6 sudoku)]

listToSudoku :: [[Maybe Int]] -> Block
listToSudoku (x:y:z:a) = concat [(take 3 x), (take 3 y), (take 3 z)]

isOkay :: Sudoku -> Bool
isOkay = undefined

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
