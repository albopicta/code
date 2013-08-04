import System.IO
import World

gameLoop :: World -> Player -> IO ()
gameLoop world playerLocation =
  do displayWorld world playerLocation
     c <- getChar
     case testValidity (newPos playerLocation c) world of 
       Just (Step _)     -> gameLoop world (newPos playerLocation c)
       Just (Portal a b) -> do new <- readWorld b
                               gameLoop new playerLocation
       --gameLoop (readWorld b) playerLocation
       _                 -> putStrLn "Invalid position" >> gameLoop world playerLocation

main = do world <- readWorld "map1"
          hSetBuffering stdin NoBuffering
          hSetEcho stdin False
          gameLoop world (Player (4,2))
