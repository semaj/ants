{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace
import System.Random


-- Constants

antV = 5.0
antRadius = 20.0
foodW = 50.0
foodH = 50.0
mapD = 700

-- Data

data AntState = Forage | Sleep

data Ant = Ant {
  antX :: Float,
  antY :: Float,
  state :: AntState
}

data Food = Food {
  foodX :: Float,
  foodY :: Float
}

data World = World {
  ants :: [Ant],
  food :: Food,
  numGen :: StdGen
}

-- Functions

moveAnt :: Food -> Ant -> Ant
moveAnt (Food fx fy) (Ant x y s) = Ant newX newY s
  where (udx, udy) = getUnitDs (fx,fy) (x,y)
        (newX, newY) = ((x + (udx * antV)), (y + (udy * antV)))

nudgeAnt :: Ant -> Ant -> Ant
nudgeAnt (Ant x' y' _) ant@(Ant x y s) = if d < 70.0 then newAnt else ant
  where (udx, udy) = getUnitDs (x,y) (x',y')
        d = dist (x', y') (x,y)
        (newX, newY) = ((x + (udx * 3.0)), (y + (udy * 3.0)))
        newAnt = (Ant newX newY s)

spaceAnts :: [Ant] -> [Ant]
spaceAnts [] = []
spaceAnts (ant:ants) = (map (nudgeAnt ant) ants) ++ [ant]

moveFood :: [Ant] -> Food -> Float -> Float -> Food
moveFood ants f@(Food fx fy) newX newY
  | any (\(Ant ax ay _) -> (dist (ax,ay) (fx,fy)) < 3.0) ants = Food newX newY
  | otherwise = f

drawAnt :: Ant -> Picture
drawAnt (Ant x y s) = Translate x y (circleSolid antRadius)

drawFood :: Food -> Picture
drawFood (Food x y) = Translate x y (rectangleSolid foodW foodH)

draw :: World -> Picture
draw (World ants food _) = Pictures $ (map drawAnt ants) ++ [(drawFood food)]

event :: Event -> World -> World
event e w = w

step :: Float -> World -> World
step f (World ants food rng) = World {
    ants = map (moveAnt food) $ spaceAnts ants,
    food = moveFood ants food (clean num) (clean num'),
    numGen = rng''
  }
  where (num, rng') = next rng
        (num', rng'') = next rng'

main :: IO ()
main =  do
          g <- newStdGen
          let (x, ng) = next g
          let y = (mod x 700) - 350
          let (a, nng) = next ng
          let b = (mod a 700) - 350
          let initialWorld = World {
            ants = [(Ant 0 0 Sleep),
                   (Ant 40 (-30) Sleep),
                   (Ant (-300) 20 Sleep),
                   (Ant (-100) 190 Forage),
                   (Ant (50) 220 Forage),
                   (Ant (-300) 120 Forage),
                   (Ant (5) 320 Sleep),
                   (Ant (-50) (-300) Forage)],
            food = (Food (realToFrac y) (realToFrac b)),
            numGen = nng
          }
          play (InWindow "Haskell Ants" (mapD, mapD) (10, 10))
                white
                50
                initialWorld
                draw
                event
                step



-- Helpers

dist :: (Float, Float) -> (Float, Float) -> Float
dist (a,b) (x,y) = sqrt $ realToFrac $ ((a - x) ^ 2) + ((b - y) ^ 2)

clean :: Int -> Float
clean i = (realToFrac $ mod i mapD) - 350.0

getUnitDs :: (Float, Float) -> (Float, Float) -> (Float, Float)
getUnitDs (a,b) (x,y) = (cos fixed, sin fixed)
    where r = (atan $ (b - y) / (a - x))
          fixed = if a < x then pi + r else r

