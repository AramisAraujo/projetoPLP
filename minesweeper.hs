import Data.List
import System.IO
--import System.Random

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

--Printing 
printBoard :: [[Int]] -> IO()
printBoard gameBoard = do
	sequence_ (map print gameBoard) --Sequence will execute the IO() commands that are in the mapping

printDisplay :: [[Char]] -> IO()
printDisplay displayBoard = do
	sequence_ (map putStrLn (map prepLine displayBoard))--This external map creates ::[IO()]. Sequence executes those IO commands

prepLine :: String -> String
prepLine line =  prepLineAux line (length line)

prepLineAux :: String -> Int -> String
prepLineAux line 1 = line
prepLineAux (x:xs) len = (x : "  ") ++ prepLineAux xs (length xs)


main = do
	
	let amountBombs = getAmountBombs boardSize bombDensity
	let iGameboard = getInitialGameboard boardSize
	let iDisplay = getInitialDisplay boardSize

	printBoard iGameboard

	printDisplay iDisplay

	putStrLn "Not Yet Implemented"
