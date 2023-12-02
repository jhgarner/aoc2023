module Main where

data Color = Red | Blue | Green
    deriving (Show)

data Pull a = Pull {redCubes :: a, blueCubes :: a, greenCubes :: a}
    deriving (Show, Generic, Functor, Foldable, Traversable)
    deriving (Semigroup, Monoid) via (Generically (Pull a))

makePull :: (Monoid a) => Color -> a -> Pull a
makePull Red numCubes = mempty{redCubes = numCubes}
makePull Blue numCubes = mempty{blueCubes = numCubes}
makePull Green numCubes = mempty{greenCubes = numCubes}

main :: IO ()
main = do
    input <- nonemptyLines
    print $ partA input
    print $ partB input

partA :: [String] -> Int
partA = sum . keys . filter isPossible . day2Parser

partB :: [String] -> Int
partB = sum . fmap product . day2Parser

day2Parser :: [String] -> Map Int (Pull Int)
day2Parser = foldMap' (parse parseLine)

parseLine :: Parser (Map Int (Pull Int))
parseLine = do
    gameId <- "Game" *> int <* ":"
    groups <- parseGroups
    let maxes = foldAs @(Pull (Max Int)) groups
    pure [(gameId, maxes)]

parseGroups :: Parser [Pull Int]
parseGroups = parseGroup `sepBy` ";"

parseGroup :: Parser (Pull Int)
parseGroup = foldTo <$> parseCubes `sepBy` ","

parseCubes :: Parser (Pull (Sum Int))
parseCubes = do
    numCubes <- int
    cubeColor <-
        choice
            [ "red" $> Red
            , "blue" $> Blue
            , "green" $> Green
            ]
    pure $ makePull cubeColor $ Sum numCubes

isPossible :: (Ord a, Num a) => Pull a -> Bool
isPossible Pull{..} = redCubes <= 12 && greenCubes <= 13 && blueCubes <= 14
