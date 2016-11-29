module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.List
import Data.Maybe

-------------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
  deriving ( Show, Eq )

type Pos = (Int,Int)
type Block = [Maybe Int]

maybeInts = [Just 1, Just 2, Just 3, Just 4, Just 5,
             Just 6, Just 7, Just 8, Just 9]

-------------------------------------------------------------------------

-- | generates a sudoku with just blanks.
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

-- | checks if sudoku is really a valid representation of a sudoku puzzle
isSudoku :: Sudoku -> Bool
isSudoku sud = (length (rows sud) == 9) &&
               (all (\x -> length x == 9) (rows sud)) &&
               (all (\x -> ( all (\y -> (
                  case y of
                    Just n -> n `elem` [1..9]
                    Nothing -> True )) (x) )) (rows sud))

-- | checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved sud = not (any (\x -> (any (\y -> y == Nothing) (x))) (rows sud))

-------------------------------------------------------------------------

-- | prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku sud = putStrLn (unlines (map (map maybeToChar) (rows sud)))

-- | converts maybe int to char
maybeToChar :: Maybe Int -> Char
maybeToChar (Just n) = chr (ord '0' + n)
maybeToChar Nothing = '.'

-- | reads from the file, and either delivers it, or stops
-- | if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku file = do
                    contents <- readFile file
                    let sud = Sudoku (map (map charToMaybe) (lines contents))
                    if isSudoku sud
                    then return sud
                    else error "File does not contain a Sudoku"

-- | converts char to maybe int
charToMaybe :: Char -> Maybe Int
charToMaybe '.' = Nothing
charToMaybe c = Just (ord c - ord '0')

-------------------------------------------------------------------------

-- | generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency [(9, return Nothing), (1, do elements maybeInts)]

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

-- | checks that sudoku is a sudoku
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku sud = isSudoku sud

-- | checks that the block does not contain duplicets of the same number
isOkayBlock :: Block -> Bool
isOkayBlock [] = True
isOkayBlock (x:xs) = if (any (\y -> y == x)) xs && (x /= Nothing)
                     then False
                     else isOkayBlock xs

-- | checks that for each Sudoku, there are 3*9 blocks,
-- | and each block has exactly 9 cells
prop_Blocks :: Sudoku -> Bool
prop_Blocks sud = (all (\x -> (length x == 9)) (blocks sud)) &&
                  (length (blocks sud) == 27)

-- | divides the sudoku to a list of blocks
blocks :: Sudoku -> [Block]
blocks sud = rows sud ++ transpose (rows sud) ++ squareBlocks
              where squareBlocks = [sudokuTo3x3Block (rows sud) (x,y)
                      | x <- [0..2], y <- [0..2]]

-- | returns a 3x3 block using the coordinates provided
sudokuTo3x3Block :: [[Maybe Int]] -> (Int, Int) -> [Maybe Int]
sudokuTo3x3Block rows (x,y) = concat (map (take 3) (map (drop (3*x))
                                     (take 3 (drop (3*y) rows))))

-- | checks if every block in the input sudoku is okay
isOkay :: Sudoku -> Bool
isOkay sud = all (\x -> (isOkayBlock x)) (blocks sud)

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
