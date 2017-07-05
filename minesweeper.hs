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

main = do
	
	let amountBombs = getAmountBombs boardSize bombDensity
	let iGameboard = getInitialGameboard boardSize
	let iDisplay = getInitialDisplay boardSize

	putStrLn "Not Yet Implemented"
