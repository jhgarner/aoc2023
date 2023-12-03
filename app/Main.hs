module Main where

-- A sparse representation of the Grid. Why are component and part type
-- parameters? I'm using `part` as a type parameter because I know I'll want to
-- map, extend, and fold over the part numbers in the engine map.
data EngineMap component part = EngineMap
    { partNumbers :: Map Loc part
    , components :: Map Loc component
    }
    deriving (Show, Generic)
    deriving (Semigroup, Monoid) via (Generically (EngineMap component part))

fromComponent :: (Semigroup part) => Loc -> component -> EngineMap component part
fromComponent loc component =
    EngineMap{components = mapOf loc component, partNumbers = mempty}

fromPartNumber :: (Semigroup component) => Loc -> part -> EngineMap component part
fromPartNumber loc part =
    EngineMap{partNumbers = mapOf loc part, components = mempty}

-- If we're iterating over all of the parts, this represents one step of the
-- iteration
data EngineIter component part = EngineIter
    { lookingAt :: Loc
    , iterPartNumbers :: Map Loc part
    , iterComponents :: Map Loc component
    }
    deriving (Functor, Foldable, Generic, Show)

instance Filterable (EngineIter component) where
    catMaybes engine@EngineIter{..} =
        engine{iterPartNumbers = catMaybes iterPartNumbers}

-- Comonad = map over a structure with an index
instance Comonad (EngineIter component) where
    extract EngineIter{..} = iterPartNumbers ! lookingAt
    duplicate engine@EngineIter{..} =
        engine{iterPartNumbers = mapWithKey (\k _ -> engine{lookingAt = k}) iterPartNumbers}

mapToIter :: EngineMap component part -> EngineIter component part
mapToIter EngineMap{..} =
    EngineIter{lookingAt = mempty, iterPartNumbers = partNumbers, iterComponents = components}

at :: EngineIter component part -> Loc -> Maybe component
at EngineIter{..} loc = iterComponents !? (loc <> lookingAt)

main :: IO ()
main = do
    input <- getContents'
    print $ partA input
    print $ partB input

parseGrid :: MyParser (EngineMap Char Int)
parseGrid =
    fmap (foldFrom @(EngineMap (OnlyOne Char) (OnlyOne Int))) $
        many $
            choice
                [ symbolic '.' $> mempty
                , fromPartNumber <$> getLoc <*> nat
                , fromComponent <$> getLoc <*> anyChar
                ]

partA :: String -> Int
partA = partTotal . onlyValidParts . mapToIter . runMyParser parseGrid

numDigits :: Int -> Int
numDigits = floor @Double . (+ 1) . logBase 10 . fromIntegral

neighbors :: EngineIter component Int -> [Loc]
neighbors engine =
    let size = numDigits $ extract engine
     in Loc -1 0 : Loc size 0 : [Loc x y | x <- [-1 .. size], y <- [-1, 1]]

hasSymbolNeighbor :: EngineIter component Int -> Bool
hasSymbolNeighbor engine = any (isJust . at engine) $ neighbors engine

onlyValidParts :: EngineIter component Int -> EngineIter component (Maybe Int)
onlyValidParts = extend (fmap extract . justWhen hasSymbolNeighbor)

partTotal :: EngineIter component (Maybe Int) -> Int
partTotal = getSum . foldMap' (maybe 0 Sum)

partB :: String -> Int
partB = sum . fmap product . onlyValidGears . mapToIter . runMyParser parseGrid

gearNeighbors :: EngineIter Char Int -> Map Loc [Int]
gearNeighbors engine =
    let gears = fmap (engine.lookingAt <>) . filter ((Just '*' ==) . at engine) $ neighbors engine
     in foldMap (`mapOf` [extract engine]) gears

onlyValidGears :: EngineIter Char Int -> Map Loc [Int]
onlyValidGears = filter ((2 ==) . length) . fold . extend gearNeighbors
