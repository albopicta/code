import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Fixed (mod')

data Ball = Ball { ballPosX :: Float, ballPosY :: Float, ballVelX :: Float, ballVelY :: Float }
  deriving (Eq, Ord, Show)

data Paddle = Paddle { paddlePos :: Float, paddleVel :: Float }
  deriving (Eq, Ord, Show)

makePaddle x dx = Paddle (clampScreenY x) dx

data World = World { paddle1 :: Paddle, paddle2 :: Paddle, ball :: Ball  }
  deriving (Eq, Ord, Show)

dimX, dimY, paddingXPaddle, dimPaddle :: Float
dimX = 150
dimY = 150
paddingXPaddle = 10
marginX = 0
marginY = 10
dimPaddle = 20
velAdd = 150
paddleOneOffset = 140
paddleTwoOffset = -140
ballRadius = 5

clamp a b x = max a (min b x)
clampScreenX = clamp (-dimY + marginX) (dimY - marginX)
clampScreenY = clamp (-dimY + dimPaddle + marginY) (dimY - dimPaddle - marginY)

background = dark (makeColor 0.3 0.3 0.3 0.3)

path xCoord x = line [(xCoord, x-dimPaddle), (xCoord, x+dimPaddle)]

animation :: World -> Picture
animation w = Pictures [ color white (path (dimX - paddingXPaddle) (paddlePos (paddle1 w))),
                         color blue (path (-dimX + paddingXPaddle) (paddlePos (paddle2 w))),
                         color white (Translate (ballPosX (ball w)) (ballPosY (ball w)) (circleSolid ballRadius))]

updatePaddle1 :: (Paddle -> Paddle) -> World -> World
updatePaddle1 f w = w { paddle1 = f (paddle1 w) }

updatePaddle2 :: (Paddle -> Paddle) -> World -> World
updatePaddle2 f w = w { paddle2 = f (paddle2 w) }

updateBall :: (Ball -> Ball) -> World -> World
updateBall f w = w { ball = f (ball w) }

updateBallPosX dt b = b {ballPosX = ballPosX b + dt * ballVelX b}
updateBallPosY dt b = b {ballPosY = ballPosX b + dt * ballVelY b}

timeStepBall :: Float -> Ball -> Ball
--timeStepBall dt b = updateBall (updateBallPosY dt) (updateBall (updateBallPosX dt) b)
timeStepBall dt b = updateBallPosY dt (updateBallPosX dt b)

updatePaddleVel :: Float -> Paddle -> Paddle
updatePaddleVel dx p = p {paddleVel = paddleVel p + dx}

updatePaddlePos :: Float -> Paddle -> Paddle
updatePaddlePos x p = p {paddlePos = clampScreenY (paddlePos p + x)}


transition :: Event -> World -> World
transition (EventKey (SpecialKey KeyUp)   Down _ _) = updatePaddle1 (updatePaddleVel velAdd)
transition (EventKey (SpecialKey KeyUp)   Up   _ _) = updatePaddle1 (updatePaddleVel (-velAdd))
transition (EventKey (SpecialKey KeyDown) Down _ _) = updatePaddle1 (updatePaddleVel (-velAdd))
transition (EventKey (SpecialKey KeyDown) Up   _ _) = updatePaddle1 (updatePaddleVel velAdd)

transition (EventKey (Char 'w') Down _ _) = updatePaddle2 (updatePaddleVel velAdd)
transition (EventKey (Char 'w') Up   _ _) = updatePaddle2 (updatePaddleVel (-velAdd))
transition (EventKey (Char 's') Down _ _) = updatePaddle2 (updatePaddleVel (-velAdd))
transition (EventKey (Char 's') Up   _ _) = updatePaddle2 (updatePaddleVel velAdd)

transition _ = id

beginning = World (Paddle paddleOneOffset 0) 
                  (Paddle paddleTwoOffset 0) 
                  (Ball 0 0 50 5) -- update to random values later

main = play (InWindow "Hello" (floor (2*dimX), floor (2*dimY)) (200,200))
            background
            60
            beginning
            animation
            transition
            timeStep

timeStepPaddle :: Float -> Paddle -> Paddle
timeStepPaddle dt p = updatePaddlePos (paddleVel p * dt) p


timeStep :: Float -> World -> World
timeStep dt = updateBall (timeStepBall dt) . updatePaddle1 (timeStepPaddle dt) . updatePaddle2 (timeStepPaddle dt)


{-
path xCoord x = line [(xCoord, x-dimPaddle), (xCoord, x+dimPaddle)]

animation (x,dx, b, db) = Pictures [color white (path (dimX - paddingXPaddle) x), 
                                    color blue (path (-dimX + paddingXPaddle) b)]

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
