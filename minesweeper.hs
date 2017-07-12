import Data.List
import Data.Char
import System.IO
import System.IO.Unsafe
import System.Random

--Game Tile Constants
coveredTile = '■'
emptyTile = '□'
flaggedTile = '🏴'
bombTile = '💣'

--Game Constants
boardSize = 9 -- 9x9 board
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
printDisplay display = do
	let upperNums = formatNumGuide [0..((length display) - 1)]
	--sequence_ (map putStrLn --This external map creates ::[IO()]. Sequence executes those IO commands
	let formattedBoard = [("   " ++ upperNums)] ++ ["   " ++ (concat(replicate (length display) "  ▼"))] ++ formatBoard display

	sequence_ (map putStrLn formattedBoard)

	where
		formatNumGuide [] = " "
		formatNumGuide (n:ns)
			|n <= 9 = "  " ++ (intToDigit n) : formatNumGuide ns
			|otherwise = (intToDigit n) : formatNumGuide ns

formatBoard :: [String] -> [String]
formatBoard board =  formatBoardAux board (length board) 0

	where 
		formatBoardAux :: [String] -> Int -> Int -> [String]
		formatBoardAux lines 0 nLine = lines
		formatBoardAux (x:xs) len nLine
			|nLine <= 9 = ["  " ++ (intToDigit (nLine)) : "▶ "  ++ (formatLine x)] ++ formatBoardAux xs (length xs) (nLine + 1)
			|otherwise = [(intToDigit (nLine)) : "▶ " ++ (formatLine x)] ++ formatBoardAux xs (length xs) (nLine + 1)

formatLine :: String -> String
formatLine [] = []
formatLine (c:cs) = c : "  " ++ formatLine cs


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

-- Method that return the adjacent positions of a coordinate
getAdjCoords :: Int -> (Int,Int)  -> [(Int,Int)]
getAdjCoords boardSize (x,y) = validateCoordinates [upperLeft x y, up x y, upperRight x y, left x y, right x y,
 lowerLeft x y, down x y, lowerRight x y] boardSize
	
	where 
		upperLeft x y = (x-1,y-1)

		up x y = (x-1,y)
		
		upperRight x y = (x-1,y+1)
		
		left x y = (x,y-1) 
		
		right x y = (x,y+1)
		
		lowerLeft x y = (x+1,y-1)
		
		down x y = (x+1,y)
		
		lowerRight x y = (x+1,y+1)

auxGetAdjCoordsList :: Int -> [(Int, Int)] -> [(Int, Int)] -- Get right adjacents coordinates of all coordinates on the first list
auxGetAdjCoordsList boardSize (c:cs)
	| length cs > 0 = getAdjCoords boardSize c ++ getAdjCoordsList boardSize cs
	| otherwise = getAdjCoords boardSize c

getAdjCoordsList :: Int -> [(Int, Int)] -> [(Int, Int)] -- Remove repeteaded elements of the auxGetAdjCoordsList list
getAdjCoordsList boardSize coords = nub (auxGetAdjCoordsList boardSize coords)

getHintCoords :: [(Int, Int)] -> [[Int]] -> [(Int, Int)] -- Get only 'hints' (element > 1) of the getAdjCoordsList
getHintCoords [] gameBoard = []
getHintCoords (c:cs) gameBoard
	| (getElement gameBoard c) > 0 = c: getHintCoords cs gameBoard
	| otherwise = getHintCoords cs gameBoard


-- Evaluates valid coordinates from a list
validateCoordinates :: [(Int,Int)] -> Int -> [(Int,Int)]
validateCoordinates [] boardSize = []
validateCoordinates ((xCoord,yCoord):xs) boardSize
    | (xCoord < boardSize && yCoord < boardSize) && (xCoord >= 0 && yCoord >= 0) = (xCoord,yCoord):validateCoordinates xs boardSize
    | otherwise = validateCoordinates xs boardSize

-- Returns a list of random coodinates given a threshold
getRandomCoords :: Int -> Int -> [(Int,Int)]
getRandomCoords amount boardSize = genRandomCoords amount boardSize []

--Generates a list of different coordinates (randomly choosen)
genRandomCoords :: Int -> Int -> [(Int,Int)] -> [(Int,Int)]
genRandomCoords 0 bSize coords = coords
genRandomCoords amount bSize [] = genRandomCoords (amount - 1) bSize [getRndTuple (bSize - 1)]
genRandomCoords amount bSize coords = do
	let sample = getRndTuple (bSize - 1)
	if (sample `elem` coords) == True then genRandomCoords amount bSize coords
		else genRandomCoords (amount - 1) bSize (sample : coords)

--Generates a tuple of random Int
getRndTuple :: Int -> (Int,Int)
getRndTuple threshold = (getRandomIntR(0,threshold), getRandomIntR(0,threshold))

--Generates a random Int from a range
getRandomIntR :: (Int,Int) -> Int
getRandomIntR (x,y) = unsafePerformIO (randomRIO (x,y))

--Gets all coordinates for adjacent empty spaces
getEmptyAdj :: [[Int]] -> (Int,Int) -> [(Int,Int)]
getEmptyAdj board point = getAdjIfEmpty board [point] [(x,y) | (x,y) <- (getAdjCoords (length board) point), isEmptyTile board (x,y)]

getAdjIfEmpty :: [[Int]] -> [(Int,Int)] -> [(Int,Int)] -> [(Int, Int)]
getAdjIfEmpty board results [] = results
getAdjIfEmpty board results toCheck = do
	--This list comprehension here gets all points adjacent to the coordinates in toCheck, if they are emptyTiles
	let newResults = results ++ [(x,y) | (x,y) <- (toCheck), isEmptyTile board (x,y), not ((x,y) `elem` results)]

	--This list comprehension here gets all adjacent emptyTiles from toCheck, if they are not yet in the newResults
	--'concat' does [[a]] -> [a] and 'nub' removes all duplicates from the list
	let newToCheck = nub ([(x,y) | (x,y) <- (concat (map (getAdjCoords (length board)) toCheck)), isEmptyTile board (x,y), not((x,y) `elem` newResults)])

	--RecursiveStep
	getAdjIfEmpty board newResults newToCheck

--Checks if coordinate leads to an EmptyTile
isEmptyTile :: [[Int]] -> (Int,Int) -> Bool
isEmptyTile board point = (getElement board point) == 0

-- Game title
getTitle :: IO ()
getTitle = putStrLn "\n *** Minesweeper *** \n"

-- Menu Options
getMenu :: String
getMenu  = "Options\n  1. Open Tile\n  2. Flag/Unflag Tile\n  3. Exit Game\n\n Choose your Option: "

-- Asking for coordinates
getUserOption :: String
getUserOption = "Please type Row and Column coordinates to open a tile."

openTiles :: [(Int, Int)] -> [[Int]] -> [[Char]]  -> [[Char]]
openTiles [] board display = display
openTiles (c:cs) board display
	| getElement display c /= coveredTile = openTiles cs board display
	| getElement board c == 0 = openTiles cs board (editBoardAt display c emptyTile)
	| getElement board c > 0 = openTiles cs board (editBoardAt display c (intToDigit (getElement board c)))
	| getElement board c == -1 = openTiles cs board (editBoardAt display c bombTile)

openAllTiles :: [[Int]] -> [[Char]]  -> [[Char]]
openAllTiles board display = openTiles [(x, y) | x <- [0.. (length board)-1], y <- [0..(length board)-1]] board display
	
insertHints :: [[Int]] -> [(Int,Int)] ->[[Int]]
--The list comprehension here evaluates to all coordinates around the bombs (excluding bombs themselves)
insertHints board bombCoords = insertRec board [(x,y) | (x,y) <- (concat(map (getAdjCoords (length board)) bombCoords)),
 not ((getElement board (x,y)) == -1)]

	where 
		insertRec board [] = board
		insertRec board (c:cs) = insertRec (editBoardAt board c ((getElement board c) + 1)) cs



main = do
	
	let amountBombs = getAmountBombs boardSize bombDensity
	let iGameboard = getInitialGameboard boardSize
	let iDisplay = getInitialDisplay boardSize


	printBoard iGameboard

	printDisplay iDisplay

	getTitle

	putStrLn getMenu

	putStrLn " "

	printDisplay (editBoardAt iDisplay (3,5) emptyTile)

	 
	let testB = [[3,2,1,0,0,0,0,0,0],
				 [0,0,1,0,0,0,0,0,0],
				 [0,0,1,0,0,0,0,0,0],
 				 [1,1,1,0,0,0,0,0,0],
				 [0,0,0,0,0,0,0,0,0],
				 [0,0,0,0,0,0,0,0,0],
				 [0,0,0,0,0,0,0,0,0],
				 [0,0,0,0,0,0,0,0,0],
				 [0,0,0,0,0,0,0,0,0]]

	let coords = [(0, 0), (1, 1), (0, 1)]

	print (getElement testB (0, 0))

	print (getHintCoords coords testB)

	-- print (getEmptyAdj testB (0,0))



	putStrLn "Not Yet Implemented"
