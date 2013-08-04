import Graphics.Gloss

rainbow t = makeColor (max (cos (0.5*t)) 0) (max (cos (0.5*(t - 2*pi/3))) 0) (max (cos (0.5*(t - 4*pi/3))) 0) 1

squares t = pictures [ 
                rotate (10*t) (
                  rotate (50*k) (
                    color (rainbow (t + 10*k * 1 / 360)) (
                      scale 4 4 (
                        rectangleSolid k k
                      )
                    )
                  )
                ) | k <- [720, 719..0] ]

background = white

main = animate (InWindow "Hello" (300, 300) (200,200))
               background
               squares
