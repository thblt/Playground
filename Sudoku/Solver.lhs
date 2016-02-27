This is a simple and relatively naive sudoku solver in Haskell. It is
a toy program to teach myself Haskell, not anything serious.

Same goes for toy sudoku solvers as for toy RSA implementations: DON'T
USE THEM. Ever. Toy with them as much as you want, but don't use them
in any other code. 

The general behavior is as follows:

 * An unsolved grid is passed to the program. The grid is expressed as
a string of at least 81 characters. 1-9 means these exact values, "0"
means an unknown value (empty cell). All other values are ignored.

 * The program “reduces” the grid, by restricting the range of legal
   values in each cell.

 * The above step is performed recursively, until it returns either a
   fully solved grid or the exact previous grid (the latter case implies
   that all possible linear reductions have been performed)

 * If the grid is not solved, then as much grids as there are legal
   values in the first unsolved cell are generated, and solved or
   discarded by reduction again.

This is not necessarily the fastest approach, but it works for
non-linear grids (that is, grids which require “guessing”) as well as
for linear ones, and can “solve” the empty grid (ie, enumerate all
possible completed grids)

Here be monsters.

Libraries
=========

> module Sudoku (Grid, Cell (..), solve) where
> 
> import Control.Monad (forever)
> import Data.List( (\\), findIndex, intercalate, sortBy )
> import Data.Maybe (fromJust, isJust)

Types
=====

We need a model for a Sudoku grid. Put simply, a grid is a list of
`Cell`s. It may be more efficient to use an array instead of a linked
listed, but I'll look into that later.

> type Grid = [ Cell ]

A cell, in its turn, may be in three different states, which we
represent as a product type. When the solver starts, some cells are
already solved (that is, they have values given to them) and most are
Unsolved. Solved values have a Word variable, which is the value we've
either got or found. Unsolved values have a [Word], to represent the
list of possible values.

> data Cell = Unsolvable
>   | Unsolved [Word]
>   | Solved Word
>   deriving (Eq, Show)

With these two types, we can represent a Sudoku grid. We only need a
few accessor and generator functions. The most basic one is `mkCell`,
which generates a `Cell` out of a list of possible values. Obviously,
the empty list maps to `Unsolvable`, a 1-element list to `Solved`, and any
other list to `Unsolved`.

> mkCell :: [Word] -> Cell
> mkCell [] = Unsolvable
> mkCell p | 1 == length p = Solved $ head p
>         | otherwise = Unsolved p

`isSolved`, `isUnsolved`, isUnsolvable`, `fromSolved`, `fromUnsolved`
are simple accessor functions to get the properties of a given `Cell`.

> isSolved :: Cell -> Bool
> isSolved (Solved _) = True
> isSolved _ = False
>
> isUnsolved :: Cell -> Bool
> isUnsolved (Unsolved _) = True
> isUnsolved _ = False
>
> isUnsolvable :: Cell -> Bool
> isUnsolvable Unsolvable = True
> isUnsolvable _ = False
>
> fromSolved :: Cell -> Word
> fromSolved (Solved x) = x
>
> fromUnsolved :: Cell -> [Word]
> fromUnsolved (Unsolved x) = x

`posValues` transforms any cell to a [Word]. It basically is mkCell in
reverse. We need such a function to extract the full range of possible
and/or legal values in a group of `Cell`s.

> posValues :: Cell -> [Word]
> posValues (Solved x) = [x]
> posValues (Unsolved x) = x
> posValues Unsolvable = []

The solver
==========

Solving a Sudoku is a recursive two-step process. `solve` calls
linearSolve (which in its turn will recursively call reduce on the
Grid) then, if the Grid is unsolved, `generation` is called to
generate a list of Grids with one more position fixed, and solve is
fmapped to this list.

> solve :: Grid -> [Grid]
> solve g = solve' step
>   where
>     solve' :: Maybe Grid -> [Grid]
>     solve' Nothing = []
>     solve' (Just g) | isGridSolved g = [g]
>                     | otherwise = concat . fmap solve $ generation g
>     step = linearSolve g

The first of theses two steps is to reduce the range of possible
values in the `Grid`. Reducing is a simple process, 

 * First, for each position, we eliminate all illegal values, that is,
   values that are already present in the row, column or subgrid. This is
   the first rule of Sudoku: no value may appear twice, etc.

 * Then, we may be able to pick values that are legal only at one
   position. If, for a given position, for are no other positions in the
   row, column or subgrid where 2 and 3 are legal, then they must be at
   this exact position. This is the second rule of Sudoku: all values
   must appear once, etc.

> reduce :: Grid -> Grid
> reduce g = fmap reduce1' [0..80] -- reduce1
>   where
>     reduce1' :: Int -> Cell
>     reduce1' i | isSolved $ g !! i = g !! i
>                | otherwise = mkCell $ reduceValues g i

The core of the reduction function lies in the reduction of the range
of possible values.

To do so, we produce four sets of values :

 * The set of *legal* values, ie the set of all possible values minus
the union of the sets of solved values in the same line, row and
square. (`legal` in the code below)

 * The sets of values that cannot appear anywhere else in the same
line, row or square (these are three sets, called `exclInLine`,
`exclInCol` and `exclInSquare` below)

(Notice that even if I use the word set, the data container used here
is obviously a list)

We return the smallest non-empty set of these four, to reduce as much
as possible the range of possible values.

> reduceValues :: Grid -> Int -> [Word]
> reduceValues g i | choices == [] = []
>                  | otherwise     = head $ sortBy (\a b -> compare (length a) (length b)) choices
>   where
>     choices = filter ([] /=) [legal, exclInLine, exclInCol, exclInSquare]
>       where
>         illegal =  (solvedValues $ lineValues g i ++ colValues g i ++ squareValues g i)
>         legal = [1..9] \\ illegal
>         exclInLine = legal \\ (concat $ fmap posValues $ lineValues g i)
>         exclInCol = legal \\ (concat $ fmap posValues $ colValues g i)
>         exclInSquare = legal \\ (concat $ fmap posValues $ squareValues g i)
>

These two functions (`reduce` and `reduceValues`) require a few
helpers, the most obvious one being a function which takes a grid, a
pair of coordinates, and returns all cells between these two
coordinates:

> subset :: Grid -> (Int, Int) -> (Int, Int) -> [Cell]
> subset g (x, y) (x', y') = [g !! xytoi (x'', y'') | x'' <- [x..x'] , y'' <- [y..y']]

The three functions above use `subset` to return the `Cell`s in the line, column and square of a given Cell, minus this `Cell` itself.

> lineValues :: Grid -> Int -> [Cell]
> lineValues g i = subset g (0,y) (8,y) \\ [g !! i]
>   where (_, y) = itoxy i

> colValues :: Grid -> Int -> [Cell]
> colValues g i = subset g (x,0) (x,8) \\ [g !! i]
>   where (x, _) = itoxy i

> squareValues :: Grid -> Int -> [Cell]
> squareValues g i = subset g st sb \\ [ g !! i ]
>   where
>     st@(sx, sy) = (3 * quot x 3, 3 * quot y 3)
>     sb = (sx + 2, sy + 2)
>     (x, y) = itoxy i

We'll also need a few filters

> verify :: Grid -> Bool
> verify g = foldr (&&) True $ fmap verify' [0..80]
>   where
>     verify' i | isSolved $  g !! i = (fromSolved $ g !! i) `elem` reduceValues g i
>               | otherwise = True

Reduction will be performed recursively until a solution is found, but
not all grids can be solved this way. If we are stuck (that is, reduce
returns its parameter unmodified), `generation` is called once
generate as much grids as there are possible values in the first
Unsolved position.

Having a generation function means that we can theoretically compute
/all/ complete grids, passing a fully unsolved grid to the program
(ie, 81 zeros). Generating is quite expensive compared to reducing,
and thus we should call it only when reducing isn't enough.

> generation :: Grid -> [ Grid ]
> generation g | isJust index = [setPos g (fromJust index) (Solved x) | x <- posValues $ g !! (fromJust index)]
>   where
>     index = findIndex isUnsolved g 

> solvedValues :: Grid -> [Word]
> solvedValues = fmap fromSolved . filter isSolved

> unsolvedValues :: Grid -> [Word]
> unsolvedValues = concat . fmap fromUnsolved . filter isUnsolved
>
> xytoi :: (Int, Int) -> Int
> xytoi (x, y) = y * 9 + x
>
> itoxy :: Int -> (Int, Int)
> itoxy i = (mod i 9, quot i 9) 

> setPos :: Grid -> Int -> Cell -> Grid
> setPos g i v = take i g ++ [v] ++ drop (i+1) g



> readGrid :: String -> Maybe Grid
> readGrid g = if 81 == length grid then Just grid else Nothing
>   where
>     grid = fmap readGrid' $ filter (flip elem $ ['0'..'9']) g
>     readGrid' a | a `elem` ['1' .. '9'] = Solved $ read [a]
>     readGrid' _ = Unsolved [ 1,2,3,4,5,6,7,8,9 ]
>


> gridPP :: (Cell -> String) -> Grid -> String
> gridPP f = concat . gridPP'
>   where
>     gridPP' [] = []
>     gridPP' g = (fmap f $ take 9 g) ++ ["\n"] ++  (gridPP' $ drop 9 g)

> showGrid :: Grid -> String
> showGrid = gridPP showFunc
>   where
>     showFunc (Solved x) = show x ++  " "
>     showFunc (Unsolved _) = "  "
>     showFunc Unsolvable = "X "
>
> showGridData :: Grid -> String
> showGridData = gridPP showFunc
>   where
>     showFunc (Solved x) = "[" ++ show x ++ "]       "
>     showFunc (Unsolved x) = showFunc' x ++ " "
>     showFunc Unsolvable = "          " 
>     showFunc' :: [Word] -> String
>     showFunc' v = concat $ fmap showFunc'' [1..9]
>       where
>         showFunc'' i = if i `elem` v then show i else "-"
>

> linearSolve :: Grid -> Maybe Grid
> linearSolve g | isGridUnsolvable g || (not . verify $ g) = Nothing
>               | g /= g' =  linearSolve g'
>               | otherwise = Just g
>   where
>     g' = reduce g
>


> main :: IO ()
> main = putStrLn . intercalate "\n" $  fmap showGrid $ solve testGridBad1

> isGridUnsolvable :: Grid -> Bool
> isGridUnsolvable = elem Unsolvable
>
> isGridSolved :: Grid -> Bool
> isGridSolved = foldr ((&&) . isSolved) True 

> emptyGrid = fromJust $ readGrid $ replicate 81 '0'
> testGridBad1 = fromJust $ readGrid "561986435641680146354631814941864158314867415487563410546981741564154188546848441"
> testGridBad2 = fromJust $ readGrid "023456789199999999099999999999999999999999999999999999999999999999999999999999999"
> testGridGood = fromJust $ readGrid "000756238078931546500002791689517423125384679347629815891475362432168957756293180"
> testGridGood2 = fromJust $ readGrid "900000200070030540000002701009017000005000600000620800801400000032060050006000004"
> testGridGood3 = fromJust $ readGrid "007900008020030050900004100800006700050070080001300006008500009010040070500001400"
> testGridGood4 = fromJust $ readGrid "005602400010000090400000007500901008000000000200504009900000005030000060004306200"
> testGridGood5 = fromJust $ readGrid "900000200070030540000002701009017000005000600000620800801400000032060050006000004"
