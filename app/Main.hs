{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Time
import Data.Time.Clock.POSIX
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)
import System.Random

nanosSinceEpoch :: UTCTime -> Int
nanosSinceEpoch =
    floor . (1e9 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 480

logoWidth :: Float
logoWidth = 640.0 / 8

logoHeight :: Float
logoHeight = 480.0 / 8

data LogoState = LogoState {x :: Float, y :: Float, dx :: Float, dy :: Float} deriving (Show)

initialState :: Int -> LogoState
initialState seed = LogoState{x = x, y = y, dx = 640.0 / 2, dy = 480.0 / 2}
  where
    screenWidthQ = fromIntegral screenWidth / 4
    screenHeightQ = fromIntegral screenHeight / 4
    seed' = mkStdGen seed
    (x, seed'') = randomR (-screenWidthQ, screenWidthQ) seed'
    (y, _) = randomR (-screenHeightQ, screenHeightQ) seed''

drawLogo :: LogoState -> Picture
drawLogo state = translate (x state) (y state) $ color white $ rectangleSolid logoWidth logoHeight

stepLogo :: ViewPort -> Float -> LogoState -> LogoState
stepLogo _ delta LogoState{..} =
    let x' = x + dx * delta
        y' = y + dy * delta
        screenWidthHalf = fromIntegral screenWidth / 2
        screenHeightHalf = fromIntegral screenHeight / 2
        dx' = if x' + logoWidth / 2 > screenWidthHalf || x' - logoWidth / 2 < -screenWidthHalf then -dx else dx
        dy' = if y' + logoHeight / 2 > screenHeightHalf || y' - logoHeight / 2 < -screenHeightHalf then -dy else dy
     in LogoState{x = x', y = y', dx = dx', dy = dy'}

main :: IO ()
main = do
    seed <- getCurrentTime
    simulate
        (InWindow "Logo" (screenWidth, screenHeight) (100, 100))
        black
        60
        (initialState $ nanosSinceEpoch seed)
        drawLogo
        stepLogo
