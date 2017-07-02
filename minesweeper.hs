import Data.List
import System.IO
import System.Random

--Game Tile Constants
coveredTile = "■"
emptyTile = "□"
flaggedTile = "🏴"
bombTile = "💣"


--Game Constants
boardSize = 8 -- 9x9
bombDensity = 0.1 -- 10% of the game board
bombs = getAmountBombs boardSize bombDensity


getAmountBombs :: Int -> Float -> Int
getAmountBombs boardSize bombDensity = round (boardSize * bombDensity) :: Int

main = do
	putStrLn "Not Yet Implemented"