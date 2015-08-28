{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Debug.Trace
import System.Random
import Data.Foldable

-- Constants

antV = 2.0
antRadius = 20.0
foodW = 50.0
foodH = 50.0
mapD = 700

-- Data

data AntState = Forage | Sleep deriving Eq

data Ant = Ant {
  avec :: Vector,
  state :: AntState
}

data Food = Food {
  fvec :: Vector
}

data World = World {
  ants :: [Ant],
  food :: Food,
  numGen :: StdGen
}

-- Functions

antDist :: Ant -> Ant -> Float
antDist (Ant avec _) (Ant bvec _) = magV $ minu bvec avec

moveAnt :: Food -> Ant -> Ant
moveAnt _ a@(Ant avec Sleep) = a
moveAnt (Food fvec) (Ant avec s) = Ant newvec s
  where newvec =  plu avec $ mulSV antV $ normalizeV $ minu fvec avec

spaceAnt :: Ant -> Ant -> Ant
spaceAnt (Ant avec _) b@(Ant bvec s)
  | magV diff < 50.0 = Ant repulse s
  | otherwise = b
  where diff = minu bvec avec
        modmag = (magV diff) ^ 2
        repulse = plu bvec $ mulSV 1.0 $ normalizeV diff

count :: [a] -> (a -> Bool) -> Int
count [] _ = 0
count (a:as) p = if p a then 1 + (count as p) else count as p

cyclePredAnts :: [Ant] -> [Ant] -> Float -> (Ant -> Bool) -> (Int -> Bool) -> (Ant -> Ant) -> [Ant]
cyclePredAnts [] _ _ _ _ _ = []
cyclePredAnts (a:as) (b:bs) fl c p t = newa:(cyclePredAnts as (bs ++ [a]) fl c p t)
      where newa = if (p (count bs (\y -> (fl > antDist a y) && c y))) then t a else a

sleepAnts :: [Ant] -> [Ant]
sleepAnts ants = cyclePredAnts ants ants 60.0 (\x -> (state x) == Forage) (> 2) (\ a -> a { state = Sleep })

cycleAnts :: [Ant] -> [Ant] -> (Ant -> Ant -> Ant) -> [Ant]
cycleAnts [] _ _ = []
cycleAnts (a:as) (b:bs) f = (foldr f a bs):(cycleAnts as (bs ++ [a]) f)

spaceAnts :: [Ant] -> [Ant]
spaceAnts [] = []
spaceAnts ants = cycleAnts ants ants spaceAnt

moveFood :: [Ant] -> Food -> Float -> Float -> Food
moveFood ants f@(Food fvec) newX newY
  | any (\(Ant avec _) -> (magV (minu fvec avec)) < 3.0) ants = Food (newX,newY)
  | otherwise = f

drawAnt :: Ant -> Picture
drawAnt (Ant (x,y) Sleep) = Translate x y (color red $ circleSolid antRadius)
drawAnt (Ant (x,y) Forage) = Translate x y (color blue $ circleSolid antRadius)

drawFood :: Food -> Picture
drawFood (Food (x,y)) = Translate x y (rectangleSolid foodW foodH)

draw :: World -> Picture
draw (World ants food _) = Pictures $ (map drawAnt ants) ++ [(drawFood food)]

event :: Event -> World -> World
event e w = w

step :: Float -> World -> World
step f (World ants food rng) = World {
    ants = sleepAnts $ spaceAnts $ map (moveAnt food) ants,
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
            ants = [(Ant (0,0) Forage),
                   (Ant (40,(-30)) Forage),
                   (Ant ((-300),20) Forage),
                   (Ant ((-100),190) Forage),
                   (Ant ((50),220) Forage),
                   (Ant ((-300),120) Forage),
                   (Ant ((5),320) Forage),
                   (Ant ((-50),(-300)) Forage)],
            food = (Food ((realToFrac y),(realToFrac b))),
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

minu :: Vector -> Vector -> Vector
minu (x,y) (x',y') = (x - x', y - y')

plu :: Vector -> Vector -> Vector
plu (x,y) (x',y') = (x + x', y + y')


