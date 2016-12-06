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
cell = frequency [(9, return Nothing),
                  (1, do elements [(Just n) | n <- [1..9]])]

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
              where squareBlocks = [get3x3Block (rows sud) (x,y)
                      | x <- [0..2], y <- [0..2]]

get3x3Block :: [[Maybe Int]] -> (Int, Int) -> [Maybe Int]
get3x3Block rows (x,y) = concat (map (take 3) (map (drop (3*y))
                                     (take 3 (drop (3*x) rows))))

-- | checks if every block in the input sudoku is okay
isOkay :: Sudoku -> Bool
isOkay sud = all (\x -> (isOkayBlock x)) (blocks sud)

-------------------------------------------------------------------------

-- | returns a list of the positions in the given Sudoku that are blank by
-- | zipping the x-positions with the y-positions
blanks :: Sudoku -> [Pos]
blanks sud = whereBlank (zip cells pos)
             where
               cells = concat (rows sud)
               pos = [(x, y) | x <- [0..8], y <- [0..8]]

-- | takes a list of pairs (Maybe Int and pos) and returns a list of the pos
-- | of the Maybe Ints that are empty (Nothing)
whereBlank :: [(Maybe Int, Pos)] -> [Pos]
whereBlank (x:[]) = if (fst x) == Nothing then [(snd x)]
                    else []
whereBlank (x:xs) = if (fst x) == Nothing then [(snd x)] ++ whereBlank xs
                    else whereBlank xs

prop_Blanks :: Sudoku -> Bool
prop_Blanks sud = prop_Blanks' sud (blanks sud)

prop_Blanks' :: Sudoku -> [Pos] -> Bool
prop_Blanks' sud [] = True
prop_Blanks' sud ((x, y):xs) = if ((rows sud)!!x)!!y /= Nothing then False
                               else prop_Blanks' sud xs

-- | updates the given list with the new value at the given index
(!!=) :: [a] -> (Int, a) -> [a]
list !!= (i, e) | (i >= (length list)) || (i < 0) = list
                | otherwise = take i list ++ [e] ++ drop (i+1) list

prop_Change :: Eq a => [a] -> (Int, a) -> Bool
prop_Change list (i, e) = i < 0 || i >= length list ||
                          (length (list !!= (i, e)) == length list
                          && (list!!=(i,e))!!i == e)

-- | updates the given Sudoku at the given position with the new value
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update sud (x,y) new | x < 0 || x > 8 || y < 0 || y > 8 = sud
                     | otherwise = Sudoku ((rows sud) !!=
                       (x,(updateInRow (rows sud) (x,y) new)))

-- | updates the given row at the given position with the new value
updateInRow :: [[Maybe Int]] -> Pos -> Maybe Int -> [Maybe Int]
updateInRow sud pos new = (sud!!(fst pos)) !!= ((snd pos), new)

prop_Update :: Sudoku -> Pos -> Maybe Int -> Bool
prop_Update sud (x,y) new | x < 0 || x > 8 || y < 0 || y > 8
                            = update sud (x,y) new == sud
                          | otherwise
                            = ((rows (update sud (x,y) new))!!x)!!y == new

-- | determines which numbers could be legally written into the given position
-- | in the given sudoku
candidates :: Sudoku -> Pos -> [Int]
candidates sud pos = catMaybes (okayNrs (findBlocks (rows sud) pos)
                     [(Just n) | n <- [1..9]])

-- | returns a list of Maybe Int candidates given a list of blocks and list of
-- | Maybe Ints
okayNrs :: [[Maybe Int]] -> [Maybe Int] -> [Maybe Int]
okayNrs matrix (x:[]) = if all (\y -> isOkayBlock (y ++ [x])) matrix
                        then [x] else []
okayNrs matrix (x:xs) = if all (\y -> isOkayBlock (y ++ [x])) matrix
                        then [x] ++ okayNrs matrix xs
                        else [] ++ okayNrs matrix xs

-- | returns a list of the blocks that the position is a part of
findBlocks :: [[Maybe Int]] -> Pos -> [[Maybe Int]]
findBlocks sud (x,y) = [sud!!x] ++
                       [(transpose sud)!!y] ++
                       [get3x3Block sud ((x `quot` 3),(y `quot` 3))]

-- | returns a solved Maybe Sudoku, Nothing if it can't be solved.
-- | checks that the input sudoku is valid
solve :: Sudoku -> Maybe Sudoku
solve sud = if (isSudoku sud) && (isOkay sud)
            then solve' sud
            else Nothing

-- | returns a solved Maybe Sudoku, Nothing if it can't be solved
solve' :: Sudoku -> Maybe Sudoku
solve' sud | isSolved sud = Just sud
           | otherwise =
             listToMaybe (catMaybes [solve' (update sud (head (blanks sud))
             (Just i)) | i <- candidates sud (head (blanks sud))])

-- | reads a file and prints the solution of the sudoku it contains,
-- | or "No solution" if it can't be solved
readAndSolve :: FilePath -> IO ()
readAndSolve file = do
                      sud <- readSudoku file
                      let solved = solve sud
                      maybe (print "No solution") printSudoku solved

-- | checks if the first sudoku is the solved version of the second sudoku
-- | checks that the first sudoku is solved and valid
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf solved unsolved = (isOkay solved) && (isSolved solved) &&
                               (isSolutionOf' (concat (rows solved))
                                              (concat (rows unsolved)))

-- | checks if the first sudoku is the solved version of the second sudoku
isSolutionOf' :: [Maybe Int] -> [Maybe Int] -> Bool
isSolutionOf' (x:[]) (y:[]) = (y == x) || (y == Nothing)
isSolutionOf' (x:xs) (y:ys) | y == Nothing = isSolutionOf' xs ys
                            | otherwise = y == x && (isSolutionOf' xs ys)

prop_SolveSound :: Sudoku -> Property
prop_SolveSound sud = isOkay sud && isSudoku sud ==>
                      isSolved solved &&
                      isSolutionOf solved sud &&
                      isOkay solved &&
                      isSudoku solved
                      where
                        solved = fromJust (solve sud)

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
