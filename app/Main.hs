module Main where

import Control.Arrow
import Data.Char
import Data.Coerce
import Data.Functor
import Data.List (isPrefixOf)
import Data.List.NonEmpty (NonEmpty, fromList, tails, toList)
import Data.Semigroup
import Data.Semigroup.Foldable
import System.IO
import Text.Read (readMaybe)
import Witherable
import Prelude hiding (filter)

main :: IO ()
main = do
    input <- getContents'
    print $ partA input
    print $ partB input

partA :: String -> Int
partA = doPart partAParser

partB :: String -> Int
partB = doPart partBParser

doPart :: (String -> NonEmpty Int) -> String -> Int
doPart parser = sum . fmap (handleLine . parser) . filter (not . null) . lines

partAParser :: String -> NonEmpty Int
partAParser = fromList . mapMaybe readCharMaybe

readCharMaybe :: (Read a) => Char -> Maybe a
readCharMaybe = readMaybe . pure

handleLine :: NonEmpty Int -> Int
handleLine = combineDigits . coerce . foldMap1 (First &&& Last)

combineDigits :: (Int, Int) -> Int
combineDigits (oldNum, newDigit) = oldNum * 10 + newDigit

partBParser :: String -> NonEmpty Int
partBParser = fromList . mapMaybe parsePartOfB . toList . tails

parsePartOfB :: String -> Maybe Int
parsePartOfB [] = Nothing
parsePartOfB part
    | "one" `isPrefixOf` part = Just 1
    | "two" `isPrefixOf` part = Just 2
    | "three" `isPrefixOf` part = Just 3
    | "four" `isPrefixOf` part = Just 4
    | "five" `isPrefixOf` part = Just 5
    | "six" `isPrefixOf` part = Just 6
    | "seven" `isPrefixOf` part = Just 7
    | "eight" `isPrefixOf` part = Just 8
    | "nine" `isPrefixOf` part = Just 9
    | otherwise = readCharMaybe $ head part
