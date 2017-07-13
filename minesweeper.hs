import Data.List
import qualified Data.Set as Set
import Data.Char
import System.IO
import System.IO.Unsafe
import System.Random

--Game Tile Constants
coveredTile = '■'
emptyTile = '□'
flaggedTile = '🏴'
bombTile = '💣'

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


getHintCoords :: [(Int, Int)] -> [[Int]] -> [(Int, Int)] -- Get only 'hints' (element > 1) of the getAdjCoordsList
getHintCoords [] gBoard = []
getHintCoords (c:cs) gBoard = [(x,y) | (x,y) <- (getAdjCoords (length gBoard) c), getElement gBoard (x,y) > 0] ++ getHintCoords cs gBoard


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
	--'concat' does [[a]] -> [a] and 'delDups' removes all duplicates from the list
	let newToCheck = delDups ([(x,y) | (x,y) <- (concat (map (getAdjCoords (length board)) toCheck)), isEmptyTile board (x,y), not((x,y) `elem` newResults)])

	--RecursiveStep
	getAdjIfEmpty board newResults newToCheck

--Checks if coordinate leads to an EmptyTile
isEmptyTile :: [[Int]] -> (Int,Int) -> Bool
isEmptyTile board point = (getElement board point) == 0

coordsAreValid :: [(Int,Int)] -> Int-> Bool
coordsAreValid coords boardSize = (length (validateCoordinates coords boardSize)) > 0

-- Game title
getTitle :: String
getTitle = "\n *** Minesweeper *** \n"

-- Menu Options
getMenu :: String
getMenu  = "Options\n  1. Open Tile\n  2. Flag/Unflag Tile\n  3. Exit Game\n\n Choose your Option: "

-- Asking for coordinates
askForCoordsOpen :: String
askForCoordsOpen = "Please type Row and Column coordinates to open a tile: "

askForCoordsFlag :: String
askForCoordsFlag = "Please type Row and Column coordinates to Flag/Unflag a tile: "

gameover :: Int -> [[Int]] -> [[Char]] -> IO()
gameover points gBoard display = do
	let revealedDisplay = openAllTiles gBoard display
	printDisplay revealedDisplay
	putStrLn ("\nGame Over! You achieved " ++ (show points) ++ " point(s)!")

getCoordFromList :: [Int] -> (Int,Int)
getCoordFromList list = ((list !! 0), (list !! 1))

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

insertBombs :: [[Int]] -> [(Int,Int)] -> [[Int]]
insertBombs gBoard [] = gBoard
insertBombs gBoard (c:cs) = insertBombs (editBoardAt gBoard c (-1)) cs

delDups :: Ord a => [a] -> [a]
delDups = Set.toList . Set.fromList


start :: [[Int]] -> [[Char]] -> Int -> Int -> Int-> IO()
start gBoard display flags points tilesLeft = do

	if tilesLeft == 0 then gameover points gBoard display

	else do
		putStrLn getTitle
		
		putStrLn (" Points: " ++ show points)
		putStrLn (" Remaining Flags: " ++ show flags ++ flaggedTile : "\n")	
	

		printDisplay display	

		putStrLn ("\n" ++ getMenu)	

		option' <- getLine	

		let option = read option' :: Int	

		if option == 1 then do --Open a tile
				putStrLn askForCoordsOpen
				input <- getLine	

				let coords = getCoordFromList (map read $ words input :: [Int])	

				if coordsAreValid [coords] (length gBoard) && (getElement display coords) /= flaggedTile then --If the coords are valid and are not flagged
					if getElement gBoard coords == -1 then gameover points gBoard display -- If the coords lead to a bomb, it's game over
					else do --In this case the coords lead to an empty space or a hint tile	

						let coordsToOpen = coords : getEmptyAdj gBoard coords
						let hints = delDups (getHintCoords coordsToOpen gBoard)
						let newScore = length (coordsToOpen)
						let remainingTiles = length (coordsToOpen ++ hints)	

						start gBoard (openTiles (coordsToOpen ++ hints) gBoard display) flags (points + newScore) remainingTiles	

				else do --Invalid/Flagged coordinates
					putStrLn "Invalid coordinates! The tile is flagged or could not be found."
					start gBoard display flags points tilesLeft	
	

		else if option == 2 then do --Flag/Unflag a tile	

			putStrLn askForCoordsFlag
			input <- getLine	

			let coords = getCoordFromList (map read $ words input :: [Int])	
	

			if flags > 0 && coordsAreValid [coords] (length gBoard) && getElement display coords == coveredTile then --Flags are available, coordinates are valid and lead to a covered tile
				start gBoard (editBoardAt display coords flaggedTile) (flags - 1) points (tilesLeft - 1)	

			else if coordsAreValid [coords] (length gBoard) && getElement display coords == flaggedTile then --Unflag a tile
				start gBoard (editBoardAt display coords coveredTile) (flags + 1) points (tilesLeft + 1)	

			else start gBoard display flags points tilesLeft --Nothing happens, proceed with the game	
	

		else if option == 3 then gameover points gBoard display	

		else do --Not a valid option
			putStrLn "That was not a valid option!"
			start gBoard display flags points tilesLeft


main = do
	
	--Game Constants
	let boardSize = 9 -- 9x9 board
	let bombDensity = 0.1 -- 10% of the game board

	let amountBombs = getAmountBombs boardSize bombDensity
	let amountFlags = amountBombs --A flag for each bomb
	let iPoints = 0

	let bombCoords = getRandomCoords amountBombs (boardSize)

	let iGameboard = insertHints (insertBombs (getInitialGameboard boardSize) bombCoords) bombCoords --Insert bombs then their hints in the board
	let iDisplay = getInitialDisplay boardSize

	let iRemainingTiles = (boardSize * boardSize) - amountBombs
	

	start iGameboard iDisplay amountFlags iPoints iRemainingTiles
