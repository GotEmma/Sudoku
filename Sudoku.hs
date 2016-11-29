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

sudokuTo3x3Block :: [[Maybe Int]] -> (Int, Int) -> [Maybe Int]
sudokuTo3x3Block rows (x,y) = concat (map (take 3) (map (drop (3*x))
                                     (take 3 (drop (3*y) rows))))

-- | checks if every block in the input sudoku is okay
isOkay :: Sudoku -> Bool
isOkay sud = all (\x -> (isOkayBlock x)) (blocks sud)

-------------------------------------------------------------------------

-- | returns a list of the positions in the given Sudoku that are blank by
-- | zipping the x-positions with the y-positions
blanks :: Sudoku -> [Pos]
blanks sud = zip (amountInRows (listOfNrOfBlanks (rows sud)))
                 (isBlank(sudokuToPairIndexList (rows sud)))

-- | returns a list of all the blanks y-positions
amountInRows :: [Int] -> [Int]
amountInRows list = concat ([(replicate (list!!i) i) | i <- [0..8]])

-- | returns a list of the number of blanks from all the rows
listOfNrOfBlanks :: [[Maybe Int]] -> [Int]
listOfNrOfBlanks (x:[]) = [nrOfBlanks x]
listOfNrOfBlanks (x:xs) = [nrOfBlanks x] ++ listOfNrOfBlanks xs

-- | returns the number of blanks in a row
nrOfBlanks :: [Maybe Int] -> Int
nrOfBlanks list = length (isBlank(pairIndex list))

-- | creates a total list of pairs containing a Maybe Int and the x position
sudokuToPairIndexList :: [[Maybe Int]] -> [(Maybe Int, Int)]
sudokuToPairIndexList (x:[]) = pairIndex x
sudokuToPairIndexList (x:xs) = pairIndex x ++ sudokuToPairIndexList xs

-- | takes a list of pairs (Maybe Int and pos) and returns a list of the pos
-- | of the Maybe Ints that are empty (Nothing)
isBlank :: [(Maybe Int, Int)] -> [Int]
isBlank (x:[]) = if (fst x) == Nothing then [(snd x)]
                 else []
isBlank (x:xs) = if (fst x) == Nothing then [(snd x)] ++ isBlank xs
                 else isBlank xs

-- | returns list of pairs containing a Maybe Int and the x or y position
pairIndex :: [Maybe Int] -> [(Maybe Int, Int)]
pairIndex list = zip list [0..8]

-- | updates the given list with the new value at the given index
(!!=) :: [a] -> (Int,a) -> [a]
list !!= (i, e) | (i >= (length list)) || (i < 0) = list
                | otherwise = take i list ++ [e] ++ drop (i+1) list

prop_Change :: Eq a => [a] -> (Int, a) -> Bool
prop_Change list (i, e) = i < 0 || i >= length list ||
                          (length (list !!= (i, e)) == length list
                          && (list!!=(i,e))!!i == e)

-- | updates the given Sudoku at the given position with the new value
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update sud pos new | ((fst pos) < 0) || ((fst pos) > 8) ||
                     ((snd pos) < 0) || ((snd pos) > 8) = sud
                   | otherwise = Sudoku ((rows sud) !!= ((fst pos),
                                        (updateInRow (rows sud) (pos) new)))

-- | updates the given row at the given position with the new value
updateInRow :: [[Maybe Int]] -> Pos -> Maybe Int -> [Maybe Int]
updateInRow sud pos new = (sud!!(fst pos)) !!= ((snd pos), new)

prop_Update :: Sudoku -> Pos -> Maybe Int -> Bool
prop_Update sud pos new | ((fst pos) < 0) || ((fst pos) > 8) ||
                          ((snd pos) < 0) || ((snd pos) > 8)
                          = update sud pos new == sud
                        | otherwise = ((rows (update sud pos new))!!
                                      (fst pos))!!(snd pos) == new

-- | determines which numbers could be legally written into the given position
-- | in the given sudoku
candidates :: Sudoku -> Pos -> [Int]
candidates sud pos = catMaybes(listOfOkayNrs(findBlocks(rows sud)pos)maybeInts)

-- | returns a list of Maybe Int candidates given a list of blocks and list of
-- | Maybe Ints
listOfOkayNrs :: [[Maybe Int]] -> [Maybe Int] -> [Maybe Int]
listOfOkayNrs matrix (x:[]) = if all (\y -> isOkayBlock (y ++ [x])) matrix
                              then [x]
                              else []
listOfOkayNrs matrix (x:xs) = if all (\y -> isOkayBlock (y ++ [x])) matrix
                              then [x] ++ listOfOkayNrs matrix xs
                              else [] ++ listOfOkayNrs matrix xs

-- | returns a list of the blocks that the position is a part of
findBlocks :: [[Maybe Int]] -> Pos -> [[Maybe Int]]
findBlocks sud pos = [sud!!(fst pos)] ++
                     [(transpose sud)!!(snd pos)] ++
                     [find3x3Blocks sud pos]

-- | returns the 3x3-block that the position is a part of by checking where the
-- | x-position is and calling find3x3BlocksHelp
find3x3Blocks :: [[Maybe Int]] -> Pos -> [Maybe Int]
find3x3Blocks sud pos | (fst pos) <=2 = find3x3BlocksHelp (take 3 sud) pos
                      | ((fst pos) >=2) && ((fst pos) <= 5) =
                        find3x3BlocksHelp (take 3 (drop 3 sud)) pos
                      | otherwise = find3x3BlocksHelp ((drop 6 sud)) pos

-- | returns the 3x3-block that the position is a part of by checking where the
-- | y-position is
find3x3BlocksHelp :: [[Maybe Int]] -> Pos -> [Maybe Int]
find3x3BlocksHelp (x:y:z:a) pos  | (snd pos) <=2 =
                                    concat [(t 3 x), (t 3 y), (t 3 z)]
                                 | ((snd pos) >=2) && ((snd pos) <= 5) =
                                    concat [(t 3 (d 3 x)),
                                            (t 3 (d 3 y)),
                                            (t 3 (d 3 z))]
                                 | otherwise =
                                   concat [(d 6 x), (d 6 y), (d 6 z)]
                                   where
                                     t = take
                                     d = drop

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
                               (isSameSud (concat (rows solved))
                                          (concat (rows unsolved)))

-- | checks if the first sudoku is the solved version of the second sudoku
isSameSud :: [Maybe Int] -> [Maybe Int] -> Bool
isSameSud (x:[]) (y:[]) = (y == x) || (y == Nothing)
isSameSud (x:xs) (y:ys) | y == Nothing = isSameSud xs ys
                        | otherwise = y == x && (isSameSud xs ys)

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
