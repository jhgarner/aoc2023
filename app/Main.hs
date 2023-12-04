module Main where

main :: IO ()
main = do
    input <- getContents'
    print $ partA input
    print $ partB input

data Card a = Card {winningNumbers :: a, myNumbers :: a}
    deriving (Show, Functor, Foldable, Generic)
    deriving (Semigroup, Monoid) via Generically (Card a)

instance Foldable1 Card where
    foldMap1 f Card{..} = f winningNumbers <> f myNumbers

parseCards :: String -> [Card (Set Int)]
parseCards = runMyParser $ many do
    _ <- "Card" *> integer <* ":"
    winningNumbers <- parseNumbers
    "|"
    myNumbers <- parseNumbers
    pure Card{..}

parseNumbers :: MyParser (Set Int)
parseNumbers = fmap fold $ many $ fmap setOf int

partA :: String -> Int
partA = sum . fmap (calculateScore . combineNumbers) . parseCards

combineNumbers :: Card (Set Int) -> Set Int
combineNumbers = foldAs1 @(Intersection Int)

calculateScore :: Set Int -> Int
calculateScore = floor @Double . (2.0 ^^) . (- 1) . length

partB :: String -> Int
partB = getSum . processAllCards . fmap (length . combineNumbers) . parseCards

-- This runs in O(n) where n is the length of the input. That's kind of neat
-- since the naive implementation I started with would have been O(n*k) where k
-- is related to the number of intersections.
processAllCards :: [Int] -> Sum Int
processAllCards = total . foldl' processCardStack (CardStack 0 (FutureCards mempty 1 0))

processCardStack :: CardStack -> Int -> CardStack
processCardStack stack intersections = pop . addItemTop intersections $ addTop stack

data CardStack = CardStack {total :: Sum Int, stack :: FutureCards}
    deriving (Show, Generic)
    deriving (Semigroup, Monoid) via Generically CardStack

pop :: CardStack -> CardStack
pop cardStack@CardStack{stack} = cardStack{stack = step stack}

addTop :: CardStack -> CardStack
addTop cardStack@CardStack{total, stack = FutureCards{numActive}} = cardStack{total = numActive <> total}

addItemTop :: Int -> CardStack -> CardStack
addItemTop n cardStack@CardStack{stack = FutureCards{numActive}} = cardStack <> mempty{stack = stackOf n numActive}

-- This structure tracks everything we need in O(1) time.
-- The idea is that at each time step, one card might trigger copies for the
-- next n cards (aka next n time steps). The number of copies that exist for a
-- card is equal to the number of durations that span the card's time step. This
-- data structure implements a way to add, query, and step durations in O(1)
-- time.
data FutureCards = FutureCards {durations :: Map Int (Sum Int), numActive :: Sum Int, currentTime :: Int}
    deriving (Show, Generic)

-- And this bit of complexity is the cost we pay for O(1) time
instance Semigroup FutureCards where
    a <> b
        | a.currentTime < b.currentTime =
            FutureCards
                { durations = mapKeysMonotonic (+ (b.currentTime - a.currentTime)) a.durations <> b.durations
                , numActive = a.numActive <> b.numActive
                , currentTime = b.currentTime
                }
        | otherwise =
            FutureCards
                { durations = mapKeysMonotonic (+ (a.currentTime - b.currentTime)) b.durations <> a.durations
                , numActive = a.numActive <> b.numActive
                , currentTime = a.currentTime
                }

instance Monoid FutureCards where
  mempty = FutureCards mempty mempty 0

stackOf :: Int -> Sum Int -> FutureCards
stackOf k v = FutureCards (mapOf k v) v 0

step :: FutureCards -> FutureCards
step FutureCards{..} =
    FutureCards
        { durations = delete currentTime durations
        , numActive = maybe numActive (numActive -) $ durations !? currentTime
        , currentTime = currentTime + 1
        }
