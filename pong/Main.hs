import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Fixed (mod')

data Paddle = Paddle { paddlePos :: Float, paddleVel :: Float }
  deriving (Eq, Ord, Show)

makePaddle x dx = Paddle (clampScreen x) dx

data World = World { paddle1 :: Paddle, paddle2 :: Paddle }
  deriving (Eq, Ord, Show)

dimX, dimY, marginXPaddle, dimPaddle :: Float
dimX = 150
dimY = 150
marginXPaddle = 10
marginYPaddle = 10
dimPaddle = 20
velAdd = 150
paddleOneOffset = 140
paddleTwoOffset = -140

clamp a b x = max a (min b x)
clampScreen = clamp (-dimY + dimPaddle + marginYPaddle) (dimY - dimPaddle - marginYPaddle)

background = dark (makeColor 0.3 0.3 0.3 0.3)

path xCoord x = line [(xCoord, x-dimPaddle), (xCoord, x+dimPaddle)]

animation :: World -> Picture
animation w = Pictures [ color white (path (dimX - marginXPaddle) (paddlePos (paddle1 w))),
                         color blue (path (-dimX + marginXPaddle) (paddlePos (paddle2 w)))]

updatePaddle1 :: (Paddle -> Paddle) -> World -> World
updatePaddle1 f w = w { paddle1 = f (paddle1 w) }

updatePaddle2 :: (Paddle -> Paddle) -> World -> World
updatePaddle2 f w = w { paddle2 = f (paddle2 w) }

updateVel :: Float -> Paddle -> Paddle
updateVel dx p = p {paddleVel = paddleVel p + dx}

updatePos :: Float -> Paddle -> Paddle
updatePos x p = p {paddlePos = clampScreen (paddlePos p + x)}

transition :: Event -> World -> World
transition (EventKey (SpecialKey KeyUp)   Down _ _) = updatePaddle1 (updateVel velAdd)
transition (EventKey (SpecialKey KeyUp)   Up   _ _) = updatePaddle1 (updateVel (-velAdd))
transition (EventKey (SpecialKey KeyDown) Down _ _) = updatePaddle1 (updateVel (-velAdd))
transition (EventKey (SpecialKey KeyDown) Up   _ _) = updatePaddle1 (updateVel velAdd)

transition (EventKey (Char 'w') Down _ _) = updatePaddle2 (updateVel velAdd)
transition (EventKey (Char 'w') Up   _ _) = updatePaddle2 (updateVel (-velAdd))
transition (EventKey (Char 's') Down _ _) = updatePaddle2 (updateVel (-velAdd))
transition (EventKey (Char 's') Up   _ _) = updatePaddle2 (updateVel velAdd)

transition _ = id

main = play (InWindow "Hello" (floor (2*dimX), floor (2*dimY)) (200,200))
            background
            60
            (World (Paddle paddleOneOffset 0) (Paddle paddleTwoOffset 0))
            animation
            transition
            timeStep

timeStepPaddle :: Float -> Paddle -> Paddle
timeStepPaddle dt p = updatePos (paddleVel p * dt) p

timeStep :: Float -> World -> World
timeStep dt = updatePaddle1 (timeStepPaddle dt) . updatePaddle2 (timeStepPaddle dt)


{-
path xCoord x = line [(xCoord, x-dimPaddle), (xCoord, x+dimPaddle)]

animation (x,dx, b, db) = Pictures [color white (path (dimX - marginXPaddle) x), 
                                    color blue (path (-dimX + marginXPaddle) b)]

main = play (InWindow "Hello" (floor (2*dimX), floor (2*dimY)) (200,200))
            background
            60
            (0,0,0,0)
            animation
            transition
            timeStep

transition :: Event -> (Float, Float, Float, Float) -> (Float, Float, Float, Float)
transition (EventKey (SpecialKey KeyUp)   Down _ _) (x,dx, b, db) = (x,dx+150, b, db)
transition (EventKey (SpecialKey KeyUp)   Up   _ _) (x,dx, b, db) = (x,dx-150, b, db)
transition (EventKey (SpecialKey KeyDown) Down _ _) (x,dx, b, db) = (x,dx-150, b, db)
transition (EventKey (SpecialKey KeyDown) Up   _ _) (x,dx, b, db) = (x,dx+150, b, db)

transition (EventKey (Char 'w') Down _ _) (x,dx, b, db) = (x,dx, b, db+150)
transition (EventKey (Char 'w') Up   _ _) (x,dx, b, db) = (x,dx, b, db-150)
transition (EventKey (Char 's') Down _ _) (x,dx, b, db) = (x,dx, b, db-150)
transition (EventKey (Char 's') Up   _ _) (x,dx, b, db) = (x,dx, b, db+150)


transition _ x = x


timeStep :: Float -> (Float, Float, Float ,Float) -> (Float, Float, Float ,Float)
timeStep dt (x, dx, b, db) = (cl (x+dx*dt), dx, cl (b+dt*db), db)
-}







{-

main = display (InWindow "Hello" (300,300) (200,200))
               background
               color (light (light blue)) 
               (line path))


main = display (InWindow "Hello" (300,300) (200,200))
               background
               (color (light blue) (circleSolid 30))


main = playIO (InWindow "Hello" (0,0) (200,200))
              color
              2
-}              

{-
rainbow t = makeColor (max (cos (0.5*t)) 0) (max (cos (0.5*(t - 2*pi/3))) 0) (max (cos (0.5*(t - 4*pi/3))) 0) 1

path = [(1, 1), (150, 150)]
animation x = color white (line [(0,0), (100 * cos x,100 * sin x)])
animation x = color (rainbow x) (thickArc 350 370 (5*x) 2)

regularPoly n = polygon [(cos (2*pi*k/n), sin (2*pi*k/n)) | k <- [0..n-1]]

squares t = pictures [ rotate k (color (rainbow (t + 10*k / 360)) (scale k k (regularPoly 5))) | k <- [360,359..0] ]
-}