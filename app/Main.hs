module Main where

import Data.List (sort)

-- This code is gross but I'd rather catch back up than make it good.

main :: IO ()
main = do
    input <- getContents'
    print $ partA input
    print $ partB input

data Race = Race {time :: Integer, recordDistance :: Integer}
    deriving (Show)

parseRace :: String -> [Race]
parseRace = runMyParser do
    "Time:"
    times <- many int
    "Distance:"
    distances <- many int
    pure $ zipWith Race times distances

parseRaceB :: String -> Race
parseRaceB = runMyParser do
    "Time:"
    time <- read . foldMap show <$> many integer
    "Distance:"
    distance <- read . foldMap show <$> many integer
    pure $ Race time distance

partA :: String -> Int
partA = product . fmap (length . getWinningDistances) . parseRace

getWinningDistances :: Race -> [Integer]
getWinningDistances Race{..} = filter (> recordDistance) $ getDistances time

getDistances :: Integer -> [Integer]
getDistances totalTime = fmap (race totalTime) [0 .. totalTime]

race :: Integer -> Integer -> Integer
race totalTime held = (totalTime - held) * held

partB :: String -> Int
partB = length . getWinningDistances . parseRaceB
