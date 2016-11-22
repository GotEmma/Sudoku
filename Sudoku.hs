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
isSudoku sudoku = (length (rows sudoku) == 9) &&
                  (all (\x -> length x == 9) (rows sudoku)) &&
                  (areElementsValid sudoku)

-- | help function for isSudoku. Checks if the elements are 1-9 or blank
areElementsValid :: Sudoku -> Bool
areElementsValid sudoku = all (\x -> ( all (\y -> (
                          case y of
                            Just n -> n `elem` [1..9]
                            Nothing -> True )) (x) )) (rows sudoku)

-- | checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved sudoku = not (any (\x -> (
                  any (\y -> y == Nothing) (x))) (rows sudoku))

-------------------------------------------------------------------------

-- | prints a representation of the sudoku sud on the screen
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
maybeToChar Nothing = '.'

-- | reads from the file, and either delivers it, or stops
-- | if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku file = do
                    contents <- readFile file
                    if isSudoku (Sudoku (stringListToMatrix (lines contents)))
                    then return (Sudoku (stringListToMatrix (lines contents)))
                    else error "File does not contain a Sudoku"

-- | converts char to maybe int
charToMaybe :: Char -> Maybe Int
charToMaybe '.' = Nothing
charToMaybe c = Just (ord c - ord '0')

-- | converts the matrix of chars to a matrix of maybe ints
stringListToMatrix :: [String] -> [[Maybe Int]]
stringListToMatrix charMatrix = map stringToList charMatrix

-- | converts the list of chars to a list of maybe ints
stringToList :: String -> [Maybe Int]
stringToList string = map charToMaybe string

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
prop_Sudoku sudoku = isSudoku sudoku

-- | checks that the block does not contain duplicets of the same number
isOkayBlock :: Block -> Bool
isOkayBlock [] = True
isOkayBlock (x:[]) = True
isOkayBlock (x:xs) = if (any (\y -> y == x)) xs && (not (x == Nothing))
                     then False
                     else isOkayBlock xs

-- | divides the sudoku to a list of blocks
blocks :: Sudoku -> [Block]
blocks sudoku = concat [rows sudoku, transpose (rows sudoku),
                        sudokuTo3x3Block (rows sudoku)]

-- | checks that for each Sudoku, there are 3*9 blocks,
-- | and each block has exactly 9 cells
prop_Blocks :: Sudoku -> Bool
prop_Blocks sudoku = (all (\x -> (length x == 9)) (blocks sudoku)) &&
                     (length (blocks sudoku) == 27)

-- | sends all the lists in the list to maybeMatrixToBlock
-- | and outputs a list of blocks
sudokuTo3x3Block :: [[Maybe Int]] -> [Block]
sudokuTo3x3Block sudoku = concat [maybeMatrixToBlock (take 3 sudoku),
                          maybeMatrixToBlock (take 3 (drop 3 sudoku)),
                          maybeMatrixToBlock (drop 6 sudoku)]

-- | takes the first three elements in every list and combines them to a block
maybeMatrixToBlock :: [[Maybe Int]] -> [Block]
maybeMatrixToBlock (x:y:z:a) = [concat [(take 3 x), (take 3 y), (take 3 z)],
                                concat [(take 3 (drop 3 x)),
                                        (take 3 (drop 3 y)),
                                        (take 3 (drop 3 z))],
                                concat [(drop 6 x), (drop 6 y), (drop 6 z)]]

-- | checks if every block in the input sudoku is okay
isOkay :: Sudoku -> Bool
isOkay sudoku = all (\x -> (isOkayBlock x)) (blocks sudoku)

-------------------------------------------------------------------------

blanks :: Sudoku -> [Pos]
blanks sudoku = zip (amountInRows (listOfNrOfBlanks (rows sudoku)))
                (isBlank(sudokuToPairIndexList (rows sudoku)))

amountInRows :: [Int] -> [Int]
amountInRows list = concat [replicate (list!!0) 0, replicate (list!!1) 1,
                            replicate (list!!2) 2, replicate (list!!3) 3,
                            replicate (list!!4) 4, replicate (list!!5) 5,
                            replicate (list!!6) 6, replicate (list!!7) 7,
                            replicate (list!!8) 8]

listOfNrOfBlanks :: [[Maybe Int]] -> [Int]
listOfNrOfBlanks (x:[]) = [nrOfBlanks x]
listOfNrOfBlanks (x:xs) = [nrOfBlanks x] ++ listOfNrOfBlanks xs

nrOfBlanks :: [Maybe Int] -> Int
nrOfBlanks list = length (isBlank(pairIndex list))

sudokuToPairIndexList :: [[Maybe Int]] -> [(Maybe Int, Int)]
sudokuToPairIndexList (x:[]) = pairIndex x
sudokuToPairIndexList (x:xs) = pairIndex x ++ sudokuToPairIndexList xs

isBlank :: [(Maybe Int, Int)] -> [Int]
isBlank (x:[]) = if (fst x) == Nothing then [(snd x)]
                 else []
isBlank (x:xs) = if (fst x) == Nothing then [(snd x)] ++ isBlank xs
                 else isBlank xs

pairIndex :: [Maybe Int] -> [(Maybe Int, Int)]
pairIndex list = zip list [0..8]

(!!=) :: [a] -> (Int,a) -> [a]
list !!= (index, element) = take index list ++ [element] ++ drop (index+1) list

prop_change :: [a] -> (Int,a) -> Bool
prop_change = undefined

update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update sudoku pos newValue = Sudoku ((rows sudoku) !!= ((fst pos),
                                    (updateInRow (rows sudoku) (pos) newValue)))

updateInRow :: [[Maybe Int]] -> Pos -> Maybe Int -> [Maybe Int]
updateInRow sudoku pos newValue = (sudoku!!(fst pos)) !!= ((snd pos), newValue)

prop_Update :: Sudoku -> Pos -> Maybe Int -> Bool
prop_Update = undefined

candidates :: Sudoku -> Pos -> [Int]
candidates sudoku pos = putLists (findBlocks (rows sudoku) pos)

putLists :: [[Maybe Int]] -> [Int]
putLists (x:[]) = converteInt (compareLists (removeBlanks x) maybeInts)
putLists (x:xs) = converteInt (compareLists (removeBlanks x) maybeInts) ++ putLists xs

converteInt :: [Maybe Int] -> [Int]
converteInt (x:[]) = [fromJust x]
converteInt (x:xs) = [fromJust x] ++ converteInt xs

removeBlanks :: [Maybe Int] -> [Maybe Int]
removeBlanks (x:[]) = if x == Nothing
                      then []
                      else [x]
removeBlanks (x:xs) = if x == Nothing
                      then [] ++ removeBlanks xs
                      else [x] ++ removeBlanks xs

compareLists :: [Maybe Int] -> [Maybe Int] -> [Maybe Int]
compareLists (x:[]) maybeInt = concat [(take (fromJust (maybeInt!!(fromJust x))) maybeInt),
                               (drop (fromJust (maybeInt!!(fromJust x) + 1)) maybeInt)]
compareLists (x:xs) maybeInt = compareLists xs (take (fromJust (maybeInt!!(fromJust x))) maybeInt)
                              -- drop (fromJust (maybeInt!!(fromJust x) + 1)) maybeInt)

findBlocks :: [[Maybe Int]] -> Pos -> [[Maybe Int]]
findBlocks sudoku pos = [sudoku!!(fst pos)] ++ [(transpose sudoku)!!(snd pos)] ++
                        [find3x3Blocks sudoku pos]

find3x3Blocks :: [[Maybe Int]] -> Pos -> [Maybe Int]
find3x3Blocks sudoku pos | (fst pos) <= 2 = find3x3BlocksHelp (take 3 sudoku) pos
                         | ((fst pos) >= 2) && ((fst pos) <= 5) =
                           find3x3BlocksHelp (take 3 (drop 3 sudoku)) pos
                         | otherwise = find3x3BlocksHelp ((drop 6 sudoku)) pos

find3x3BlocksHelp :: [[Maybe Int]] -> Pos -> [Maybe Int]
find3x3BlocksHelp (x:y:z:a) pos  | (snd pos) <= 2 = concat [(take 3 x), (take 3 y), (take 3 z)]
                                 | ((snd pos) >= 2) && ((snd pos) <= 5) =
                                   concat [(take 3 (drop 3 x)), (take 3 (drop 3 y)), (take 3 (drop 3 z))]
                                 | otherwise = concat [(drop 6 x), (drop 6 y), (drop 6 z)]

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
    Sudoku
        [ [j 3,j 6,j 5,n  ,j 7,j 1,j 2,j 5,j 4]
        , [j 5,j 5,j 5,j 5,j 5,j 5,j 1,j 8,n  ]
        , [j 5,j 5,j 9,j 2,j 5,j 4,j 7,n  ,j 5]
        , [n  ,j 5,j 5,j 5,j 1,j 3,j 5,j 2,j 8]
        , [j 4,n  ,j 5,j 5,j 5,j 2,j 5,j 5,j 9]
        , [j 2,j 7,n  ,j 4,j 6,j 5,j 5,j 5,j 5]
        , [j 5,j 5,j 5,j 3,n  ,j 8,j 9,j 5,j 5]
        , [j 5,j 8,j 3,j 5,j 5,n  ,j 5,j 6,j 5]
        , [j 5,j 5,j 7,j 6,j 9,j 5,n  ,j 4,j 3]
        ]
      where
        n = Nothing
        j = Just
