module World where

import System.IO
import Data.Map (Map)
import qualified Data.Map as Map

worldSizeX = 20
worldSizeY = 8

playerPixel = '@'

type Pixel = Char -- encodes info what to display

data Tile = Step Pixel
          | Obstacle Pixel
          | Portal Pixel FilePath
  deriving Show

data World = World (Map (Int, Int) Tile)
  deriving Show

data Player = Player (Int, Int)

playerX (Player (x,y)) = x
playerY (Player (x,y)) = y


testValidity (Player location) (World world) = Map.lookup location world

tilePixel :: Tile -> Pixel
tilePixel (Step p)     = p
tilePixel (Obstacle p) = p
tilePixel (Portal p _) = p

formatDisplay :: World -> Player -> [String]
formatDisplay world player = mapToStrings (worldToMap world player)
  where worldToMap :: World -> Player -> Map (Int, Int) Pixel
        worldToMap (World world) (Player location) = Map.insert location playerPixel (Map.map tilePixel world)
        mapToStrings :: Map (Int, Int) Pixel -> [String]
        mapToStrings m = [[ pixelAt (x,y) | x <- [1..worldSizeX]] | y <- [1..worldSizeY]]
           where pixelAt (x,y) = case Map.lookup (x,y) m of
                                   Nothing -> ' '
                                   Just c  -> c

makeWorld :: [[Char]] -> World
makeWorld xss = World . Map.fromList $ [((x,y), t) | (y,xs) <- zip [1..] xss, (x,c) <- zip [1..] xs, t <- charToTile c]
  where charToTile '#' = [Obstacle '#']
        charToTile '.' = [Step ' ']
        charToTile 'O' = [Portal 'O' "map2"]
        charToTile _   = []

readWorld :: FilePath -> IO World
readWorld file = do xs <- readFile file
                    return (makeWorld (lines xs))

displayWorld :: World -> Player -> IO () 
displayWorld world player = mapM_ putStrLn (formatDisplay world player)

newPos (Player (x,y)) 'w' = (Player (x,(y-1)))
newPos (Player (x,y)) 'a' = (Player ((x-1),y))
newPos (Player (x,y)) 's' = (Player (x,(y+1)))
newPos (Player (x,y)) 'd' = (Player ((x+1),y))
newPos current         _  = current -- unchanged