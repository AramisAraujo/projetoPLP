import Data.List
import System.IO
import System.Random

--Game Tile Constants
coveredTile = '■'
emptyTile = '□'
flaggedTile = '🏴'
bombTile = '💣'

--Game Constants
boardSize = 9 -- 9x9
bombDensity = 0.1 -- 10% of the game board

--Game Initial functions
getAmountBombs :: Int -> Float -> Int
getAmountBombs boardSize bombDensity = round ( (fromIntegral boardSize) * bombDensity * 10) :: Int

getInitialGameboard :: Int -> [[Int]]
getInitialGameboard boardSize = replicate boardSize (replicate boardSize 0)

getInitialDisplay :: Int -> [[Char]]
getInitialDisplay boardSize = replicate boardSize (replicate boardSize coveredTile)

--Printing related functions
printBoard :: [[Int]] -> IO()
printBoard gameBoard = do
	sequence_ (map print gameBoard) --Sequence will execute the IO() commands that are in the mapping

printDisplay :: [[Char]] -> IO()
printDisplay displayBoard = do
	sequence_ (map putStrLn (map formatLine displayBoard))--This external map creates ::[IO()]. Sequence executes those IO commands

formatLine :: String -> String
formatLine line =  formatLineAux line (length line)

	where 
		formatLineAux :: String -> Int -> String
		formatLineAux line 1 = line
		formatLineAux (x:xs) len = (x : "  ") ++ formatLineAux xs (length xs)


--List operation related functions

editBoardAt :: [[a]] -> (Int,Int) -> a -> [[a]]
editBoardAt board coords element = editRowAt board coords 0 element

editRowAt :: [[a]] -> (Int,Int) -> Int -> a -> [[a]]
editRowAt [] coords nRow element = []
editRowAt (b:bs) (xCoord,yCoord) nRow element 
	|nRow == xCoord = (editLineAt b yCoord 0 element) : bs --change element in this line and put them back together
	|otherwise = b : (editRowAt bs (xCoord,yCoord) (nRow + 1) element) --Keep iterating

editLineAt :: [a] -> Int -> Int -> a -> [a]
editLineAt [] yCoord nColumn element = []
editLineAt (l:ls) yCoord nColumn element 
	|nColumn == yCoord = element : ls -- Change the element in that position
	|otherwise = l : (editLineAt ls yCoord (nColumn + 1) element)

getElement ::[[a]] -> (Int, Int) -> a
getElement matrix (x, y) = matrix !! x !! y

-- Method that return the adjacent positions
adjacentCoordinates :: (Int,Int) -> [(Int,Int)]
adjacentCoordinates (xCoord,yCoord) = validCoordinates [(xCoord-1,yCoord-1), (xCoord-1,yCoord), (xCoord-1,yCoord+1), (xCoord,yCoord-1), (xCoord,yCoord+1), (xCoord+1,yCoord-1), (xCoord+1,yCoord), (xCoord+1,yCoord+1)]
 
validCoordinates :: [(Int,Int)] -> [(Int,Int)]
validCoordinates [] = []
validCoordinates ((xCoord,yCoord):xs)
    | xCoord<boardSize && yCoord<boardSize && xCoord>=0 && yCoord>=0 = (xCoord,yCoord):validCoordinates xs
    | otherwise = validCoordinates xs
-- Method that return the bombs positions

tuples :: [(Int,Int)]
tuples = generateTuples 9 []
 
generateTuples :: Int -> [(Int,Int)] -> [(Int,Int)]
generateTuples 0 (x:xs) = []
generateTuples quantity [] = (generateTuples (quantity - 1) [(random (0, boardSize-1),random (0, boardSize-1))])
generateTuples quantity (x:xs) = if (coordinates `elem` (x:xs)) then  generateTuples quantity (x:xs)
        else coordinates:(generateTuples (quantity - 1) ((x:xs)++[coordinates]))
            where coordinates = (random (0, boardSize-1),random (0, boardSize-1))
			
main = do
	
	let amountBombs = getAmountBombs boardSize bombDensity
	let iGameboard = getInitialGameboard boardSize
	let iDisplay = getInitialDisplay boardSize

	printBoard iGameboard

	printDisplay iDisplay

	putStrLn " "

	printDisplay (editBoardAt iDisplay (3,5) emptyTile)

	--putChar (getElement (editBoardAt iDisplay (3,5) bombTile) (3,5)) 

	putStrLn "Not Yet Implemented"
