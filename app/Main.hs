module Main where

import Data.List (elemIndex, nub, sort)

-- Not great code, but it works

main :: IO ()
main = do
    input <- getContents'
    print $ partA input
    print $ partB input

data Card = Weakest | Two | Three | Four | Five | Six | Seven | Eight | Nine | T | J | Q | K | A
    deriving (Ord, Eq, Show, Enum, Bounded)

-- data Hand a = Hand {first :: a, second :: a, third :: a, fourth :: a, fifth :: a, bid :: Int}
data Hand a = Hand {cards :: [a], bid :: Int}
    deriving (Show, Functor, Foldable, Eq)

parseHands :: String -> [Hand Card]
parseHands = runMyParser $ many parseHand

parseHand :: MyParser (Hand Card)
parseHand = do
  cards <- many $ runUnspaced parseCard
  " "
  bid <- int
  pure Hand{..}

parseCard :: Unspaced MyParser Card
parseCard =
    choice
        [ "2" $> Two
        , "3" $> Three
        , "4" $> Four
        , "5" $> Five
        , "6" $> Six
        , "7" $> Seven
        , "8" $> Eight
        , "9" $> Nine
        , "T" $> T
        , "J" $> J
        , "Q" $> Q
        , "K" $> K
        , "A" $> A
        ]

fiveOfAKind :: (Ord a, Eq a) => [a] -> Bool
fiveOfAKind = (1 ==) . numUnique

fourOfAKind :: (Ord a, Eq a) => [a] -> Bool
fourOfAKind cards
    | [a, _] <- nub cards = any (cardDups cards a) ([4, 1] :: [Int])
    | otherwise = False

fullHouse :: (Ord a, Eq a) => [a] -> Bool
fullHouse cards
    | [a, _] <- nub cards = any (cardDups cards a) ([3, 2] :: [Int])
    | otherwise = False

threeOfAKind :: (Ord a, Eq a) => [a] -> Bool
threeOfAKind cards
    | [_, _, _] <- nub cards = any (\n -> cardDups cards n 3) $ nub cards
    | otherwise = False

twoPair :: (Ord a, Eq a) => [a] -> Bool
twoPair = (== 3) . numUnique

pair :: (Ord a, Eq a) => [a] -> Bool
pair = (== 4) . numUnique

powerOrder :: (Ord a, Eq a) => [a] -> Int
powerOrder cards = maybe 0 (6 -) $ elemIndex True $ fmap ($ cards) [fiveOfAKind, fourOfAKind, fullHouse, threeOfAKind, twoPair, pair]

countCard :: (Eq a) => [a] -> a -> Int
countCard cards a = length $ filter (== a) cards

cardDups :: (Eq a) => [a] -> a -> Int -> Bool
cardDups cards a n = n == countCard cards a

numUnique :: (Eq a) => [a] -> Int
numUnique = length . nub

instance (Eq a, Ord a) => Ord (Hand a) where
    a <= b = (powerOrder a.cards, a.cards) <= (powerOrder b.cards, b.cards)

partA :: String -> Int
partA = sum . fmap (uncurry (*)) . zip [1 ..] . fmap bid . sort . parseHands

partB :: String -> Int
partB = sum . fmap (uncurry (*)) . zip [1 ..] . fmap bid . sortWith jokerSort . parseHands

jokerSort :: Hand Card -> (Int, [Card])
jokerSort hand = maximum $ fmap (`scoreJoker` hand) allButJoker

scoreJoker :: Card -> Hand Card -> (Int, [Card])
scoreJoker card hand = (powerOrder $ cards $ replaceJokerWith card hand, cards $ replaceJokerWith Weakest hand)

replaceJokerWith :: Card -> Hand Card -> Hand Card
replaceJokerWith card = fmap (replaceIfJoker card)

replaceIfJoker :: Card -> Card -> Card
replaceIfJoker replacement J = replacement
replaceIfJoker _ card = card

allButJoker :: [Card]
allButJoker = filter (/= J) $ enumFrom Two
