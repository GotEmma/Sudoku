module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.List
import Data.List.Split

-------------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
  deriving ( Show, Eq )

type Pos = (Int,Int)

type Block = [Maybe Int]

maybeInts = [Just 1, Just 2, Just 3, Just 4, Just 5,
             Just 6, Just 7, Just 8, Just 9]

main :: IO ()
main = do
        printSudoku example

-- | allBlankSudoku is a sudoku with just blanks.
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- | isSudoku sud checks if sudoku is really a valid representation of a sudoku
-- | puzzle.
isSudoku :: Sudoku -> Bool
isSudoku sudoku = (length (rows sudoku) == 9) &&
                  (all (\x -> length x == 9) (rows sudoku)) &&
                  (areElementsValid sudoku)

-- | help function for isSudoku. Checks if the elements are 1-9 or blank.
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

-- | converts the matrix of maybe ints to a matrix of chars
maybeMatrixToString :: [[Maybe Int]] -> String
maybeMatrixToString matrix = unlines (map maybeListToString matrix)

-- | converts the list of maybe ints to a list of chars
maybeListToString :: [Maybe Int] -> String
maybeListToString list = map maybeToChar list

-- | converts maybe int to char
maybeToChar :: Maybe Int -> Char
maybeToChar (Just n) = chr (ord '0' + n)
maybeToChar Nothing = chr (ord '.')

-- | reads from the file, and either delivers it, or stops
-- | if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku file = do
                    contents <- readFile file
                    if isSudoku (Sudoku (stringListToMatrix (lines contents)))
                    then return (Sudoku (stringListToMatrix (lines contents)))
                    else error "File does not contain a Sudoku"

-- | converts char to maybe int
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

-- | converts the matrix of chars to a matrix of maybe ints
stringListToMatrix :: [String] -> [[Maybe Int]]
stringListToMatrix charMatrix = map stringToList charMatrix

-- | converts the list of chars to a list of maybe ints
stringToList :: String -> [Maybe Int]
stringToList string = map convertToInt string

-------------------------------------------------------------------------

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency [(9, return Nothing), (1, do elements maybeInts)]

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

-- | checks that sudoku is a sudoku
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku sudoku = isSudoku sudoku

-- | checks that the block does not contain duplicets of the same number
isOkayBlock :: Block -> Bool
isOkayBlock ([]) = True
isOkayBlock (x:[]) = True
isOkayBlock (x:xs) = if (any (\y -> y == x)) xs && (not (x == Nothing))
                     then False
                     else isOkayBlock xs

-- | divides the sudoku to a list of blocks
blocks :: Sudoku -> [Block]
blocks sudoku = concat [rows sudoku, transpose (rows sudoku),
                        sudokuTo3x3Block (rows sudoku)]

-- | checks that for each Sudoku, there are 3*9 blocks,
-- | and each block has exactly 9 cells.
prop_Blocks :: Sudoku -> Bool
prop_Blocks sudoku = all (\x -> (length x == 9)) (blocks sudoku)

-- | sends all the lists in the list to listToSudoku and outputs a list of blocks
sudokuTo3x3Block :: [[Maybe Int]] -> [Block]
sudokuTo3x3Block sudoku = [maybeMatrixToBlock (take 3 sudoku),
                          maybeMatrixToBlock (take 3 (drop 3 sudoku)),
                          maybeMatrixToBlock (drop 6 sudoku)]

-- | takes the three first elements in every list and combines them to a block
maybeMatrixToBlock :: [[Maybe Int]] -> Block
maybeMatrixToBlock (x:y:z:a) = concat [(take 3 x), (take 3 y), (take 3 z)]

-- | checks if every block in the input sudoku is okay
isOkay :: Sudoku -> Bool
isOkay sudoku = all (\x -> (isOkayBlock x)) (blocks sudoku)

-------------------------------------------------------------------------

--blanks :: Sudoku -> [Pos]
--blanks sudoku = map linesBlank sudoku

--linesBlank :: [Maybe Int] -> Bool
--linesBlank line | elemIndex Nothing line

--(!!=) :: [a] -> (Int,a) -> [a]

--update :: Sudoku -> Pos -> Maybe Int -> Sudoku

--candidates :: Sudoku -> Pos -> [Int]



-------------------------------------------------------------------------

example :: Sudoku
example =
    Sudoku
    [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
    , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
    , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
    , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
    , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
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
