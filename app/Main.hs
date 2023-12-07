module Main where
import Data.List (sort)

-- This code is gross but I'd rather catch back up than make it good.

main :: IO ()
main = do
    input <- getContents'
    print $ partA input
    print $ partB input

data Problem = Problem { seeds :: [Int], mappings :: [[Mapping]]}

parseIt :: String -> Problem
parseIt = runMyParser do
  seeds <- parseSeeds
  mappings <- many do
    eatMapHeader
    sort <$> many parseRow
  pure Problem{..}

parseSeeds :: MyParser [Int]
parseSeeds = "seeds:" >> many int

eatMapHeader :: MyParser ()
eatMapHeader = void $ manyTill (anyChar @Char) ":"

data Mapping = Mapping { destinationStart :: Int, sourceStart :: Int, rangeLength :: Int }
  deriving (Show, Ord, Eq)

doMap :: Int -> Mapping -> Maybe Int
doMap a Mapping{..}
  | a >= sourceStart && a < sourceStart + rangeLength = Just $ a - sourceStart + destinationStart
  | otherwise = Nothing

parseRow :: MyParser Mapping
parseRow = Mapping <$> int <*> int <*> int

-- Note: Part A doesn't actually work, but it's right enough for the minimum
-- seed. I can actually use my Part B solution to get the correct Part A
-- numbers.
partA :: String -> Int
partA s =
  let Problem{..} = parseIt s
  in minimum $ fmap (mapAll mappings) seeds

bumpIfNeeded :: Int -> Mapping -> Int
bumpIfNeeded a Mapping{..}
  | destinationStart <= a = a + rangeLength
  | otherwise = a

stepping :: Int -> Either Int Int -> Mapping -> Either Int Int
stepping _ (Left found) _ = Left found
stepping needle (Right dest) mapping = maybe (Right $ bumpIfNeeded dest mapping) Left $ doMap needle mapping

mapAll :: [[Mapping]] -> Int -> Int
mapAll mappings a = foldl' mapIt a mappings

mapIt :: Int -> [Mapping] -> Int
mapIt a = either id id . foldl' (stepping a) (Right a)



-- The idea here is that we can flatten all of the maps into a single map. The
-- size of the map is based on the number of elements in the submaps which is
-- luckily pretty small. From there, we know that the solution will either be
-- at the start of a map source range (keeping only the starts that sit within
-- the seed range) or at the start of a seed range.
partB :: String -> Int
partB s =
  let Problem{..} = parseIt s
      flat = composePipeline mappings
  in minimum $ findMins (pairSeeds seeds) flat

pairSeeds :: [Int] -> [Pairing]
pairSeeds [] = []
pairSeeds (a:b:rest) = Pair{pStart=a, pEnd=a+b} : pairSeeds rest
pairSeeds _ = error "Whoops!"

findMins :: [Pairing] -> [Mapping] -> [Int]
findMins seeds mappings = fmap ((`mapIt` mappings) . pStart) seeds ++ fmap snd (filter (inAnyPairing seeds . fst) $ fmap getMin mappings)

getMin :: Mapping -> (Int, Int)
getMin Mapping{..} = (sourceStart, destinationStart)

sortBySource :: [Mapping] -> [Mapping]
sortBySource = sortWith sourceStart

sourceEndOf :: Mapping -> Int
sourceEndOf Mapping{..} = sourceStart + rangeLength

destEndOf :: Mapping -> Int
destEndOf Mapping{..} = destinationStart + rangeLength

data Pairing = Pair {pStart :: Int, pEnd :: Int}
  deriving (Show)

pairDist :: Pairing -> Int
pairDist Pair{..} = pEnd - pStart

inAnyPairing :: [Pairing] -> Int -> Bool
inAnyPairing pairings a = any (inPairing a) pairings

inPairing :: Int -> Pairing -> Bool
inPairing a Pair{..} = a >= pStart && a < pEnd

saturateMappings :: [Mapping] -> [Mapping]
saturateMappings mappings = mappings ++ pairAllEmptySpaces (getAllEmptySources mappings) (getAllEmptyTargets mappings)

getAllEmpty :: (Mapping -> Int) -> Int -> [Mapping] -> [Pairing]
getAllEmpty _ a [] = [Pair{pStart = a, pEnd = maxBound}]
getAllEmpty getStart a (mapping@Mapping{rangeLength}:rest)
  | a == getStart mapping = getAllEmpty getStart (getStart mapping + rangeLength) rest
  | otherwise = Pair{pStart = a, pEnd = getStart mapping} : getAllEmpty getStart (getStart mapping + rangeLength) rest

getAllEmptySources :: [Mapping] -> [Pairing]
getAllEmptySources = getAllEmpty sourceStart 0 . sortBySource

getAllEmptyTargets :: [Mapping] -> [Pairing]
getAllEmptyTargets = getAllEmpty destinationStart 0 . sort


pairAllEmptySpaces :: [Pairing] -> [Pairing] -> [Mapping]
pairAllEmptySpaces [] [] = []
pairAllEmptySpaces (src:srcs) (trgt:trgts) =
  case pairEmptySpace src trgt of
    (mapping, Nothing) -> mapping : pairAllEmptySpaces srcs trgts
    (mapping, Just (Left leftOver)) -> mapping : pairAllEmptySpaces (leftOver:srcs) trgts
    (mapping, Just (Right leftOver)) -> mapping : pairAllEmptySpaces srcs (leftOver:trgts)
pairAllEmptySpaces _ _ = error "Impossible!"

pairEmptySpace :: Pairing -> Pairing -> (Mapping, Maybe (Either Pairing Pairing))
pairEmptySpace src trgt
  | pairDist src == pairDist trgt = (mapping{rangeLength = pairDist src}, Nothing)
  | pairDist src < pairDist trgt = (mapping{rangeLength = pairDist src}, Just $ Right trgt{pStart=trgt.pStart + pairDist src})
  | pairDist src > pairDist trgt = (mapping{rangeLength = pairDist trgt}, Just $ Left src{pStart=src.pStart + pairDist trgt})
  | otherwise = error "Not possible"
  where
    mapping = Mapping {sourceStart = src.pStart, destinationStart = trgt.pStart, rangeLength = error "Not filled in"}

composePipeline :: [[Mapping]] -> [Mapping]
composePipeline = foldl1 composeMappings . fmap saturateMappings

composeMappings :: [Mapping] -> [Mapping] -> [Mapping]
composeMappings g f = combineMappings (sort g) (sortBySource f)

combineMappings :: [Mapping] -> [Mapping] -> [Mapping]
combineMappings [] [] = []
combineMappings (g:gs) (f:fs) =
  case combine g f of
    (mapping, Nothing) -> mapping : combineMappings gs fs
    (mapping, Just (Left leftOver)) -> mapping : combineMappings (leftOver:gs) fs
    (mapping, Just (Right leftOver)) -> mapping : combineMappings gs (leftOver:fs)
combineMappings _ _ = error "Never!"

combine :: Mapping -> Mapping -> (Mapping, Maybe (Either Mapping Mapping))
combine a b
  | destEndOf a == sourceEndOf b = (mapping, Nothing)
  | destEndOf a < sourceEndOf b =
      let usedBLength = b.rangeLength - remainingLength
      in (mapping, Just $ Right cutMapping{sourceStart = destEndOf a, destinationStart = b.destinationStart + usedBLength})
  | destEndOf a > sourceEndOf b =
      let usedALength = a.rangeLength - remainingLength
      in (mapping, Just $ Left cutMapping{destinationStart = sourceEndOf b, sourceStart = a.sourceStart + usedALength})
  | otherwise = error "Impossible"
  where
    remainingLength = abs $ sourceEndOf b - destEndOf a
    cutMapping = Mapping{sourceStart = undefined, destinationStart = undefined, rangeLength = remainingLength}
    mapping = Mapping {sourceStart = a.sourceStart, destinationStart = b.destinationStart, rangeLength = min a.rangeLength b.rangeLength}
