module Main where

-- Let's walk through how I solved advent of code 2023 part 3 using Haskell.
-- I ended up using a sparse representation and a pattern called `Comonad` to
-- deal with the grid. Just like how `Functor` has `map` to iterate over the
-- values of some data structure, `Comonad` has `extend` to iterate over all the
-- indices along with `extract` to get the value at the focused/current index.

-- The wording of the problem is kind of abstract, so I'll use `component` often
-- times to refer to the non-period symbols in the grid, and partNumbers (or
-- just parts) to refer to the numbers in the grid.
--
-- Why does EngineMap have type parameters? Won't the grid always store Ints and
-- Chars? Yes, the grid will always store those things, but data types in
-- Haskell aren't just for storing data; they're also for deciding control flow!
-- This will be more relevant with EngineIter later on, but I know that the
-- control flow will involve processing all the cells in the grid, so I'll make
-- them type paramters so that the compiler can see what it is we'll be
-- iterating over.
data EngineMap component part = EngineMap
    { partNumbers :: Map Loc part
    , components :: Map Loc component
    }
    deriving (Show, Generic)
    -- These two derivings aren't built into the compiler, but they're trivial
    -- enough that a computer should still do them.
    deriving (Semigroup, Monoid) via (Generically (EngineMap component part))

-- There are two ways to build a map. The imperative style is to create an
-- empty map and call an insert method on it a bunch of times. In Haskell,
-- this is kind of weird because the Map is supposed to be immutable so you have
-- to thread the newly inserted into map around and it's kind of a pain. The
-- alternative I'll use is to build up a list of single element maps and merge
-- them afterwards. These two methods allow for building the EngineMap based on
-- whether we're seeing a component or a part number.

fromComponent :: (Semigroup part) => Loc -> component -> EngineMap component part
fromComponent loc component =
    EngineMap{components = mapOf loc component, partNumbers = mempty}

fromPartNumber :: (Semigroup component) => Loc -> part -> EngineMap component part
fromPartNumber loc part =
    EngineMap{partNumbers = mapOf loc part, components = mempty}

-- This is our magic Comonad. Loosely, any datatype that represents some
-- structure with a focused element is a Comonad. This is just like EngineMap
-- except we store the focused element as `lookingAt`.
--
-- Remember what I said about control flow and type parameters? Because we make
-- `part` the last type parameter, the compiler will generate ways for us to
-- iterate over all of the parts. That's represented by the Functor and Foldable
-- type classes that we automatically derive.
data EngineIter component part = EngineIter
    { lookingAt :: Loc
    , iterPartNumbers :: Map Loc part
    , iterComponents :: Map Loc component
    }
    deriving (Functor, Foldable, Generic, Show)

-- This should also be derived by a computer, but no one's written that yet.
instance Filterable (EngineIter component) where
    -- The {..} part is a GHC extension which automatically creates variables
    -- matching the names of the record fields.
    catMaybes engine@EngineIter{..} =
        engine{iterPartNumbers = catMaybes iterPartNumbers}

instance Comonad (EngineIter component) where
    -- Extract the currently focused element. I've decided that everything
    -- should happen from the part number's point of view so part numbers are
    -- the only elements that can be focused.
    extract EngineIter{..} = iterPartNumbers ! lookingAt -- ! is the indexing operator
    -- Seeing how duplicate (`(w a -> b) -> w a -> w b`) relates to iterating
    -- over indices is very strange. Basically, we're going to take each value
    -- in our structure and replace it with a near duplicate of the structure
    -- where the only difference is that the near duplicate focuses on the value
    -- we're replacing. You can see that in the implementation where we replace
    -- the `lookingAt` field with the key of our map. So if you have a grid of
    -- numbers, duplicate will turn the first cell in the grid into a grid
    -- focused on the first cell, and it will turn the second cell in the grid
    -- into a grid focused on the second cell, etc. We'll end up with a grid of
    -- grids. Luckily the extend function (which we don't have to write) will
    -- handle how to turn that grid of grids into something useful. We just have
    -- to say how to build it.
    duplicate engine@EngineIter{..} =
        engine
            { iterPartNumbers =
                mapWithKey
                    (\k _ -> engine{lookingAt = k})
                    iterPartNumbers
            }

-- Boilerplate to convert between EngineMap and EngineIter. Some kind of generic
-- Iterator type might be neat and probably already exists.
mapToIter :: EngineMap component part -> EngineIter component part
mapToIter EngineMap{..} =
    EngineIter
        { lookingAt = mempty -- It doesn't really matter where we start
        , iterPartNumbers = partNumbers
        , iterComponents =
            components
        }

-- Index into an EngineIter with a relative location safely handling grid edges
at :: EngineIter component part -> Loc -> Maybe component
-- !? is the safe indexing operator and <> is the Semigroup combining operator.
-- The Semigroup instance for Loc just adds the components together. Semigroups
-- are closely related to Monoids which I'll talk about later.
at EngineIter{..} loc = iterComponents !? (loc <> lookingAt) 

-- We're now through all the somewhat generic data structure stuff, so let's
-- look at solving our problem.
main :: IO ()
main = do
    input <- getContents'
    print $ partA input
    print $ partB input

-- MyParser is a Parser combinator where you can access the current line+column
-- while parsing. It also gives really nice error messages if the parser fails.
parseGrid :: MyParser (EngineMap Char Int)
parseGrid =
    -- Let's talk about Monoids because they'll come up a lot. A Monoid is
    -- something with an initial value and some way to accumulate it. For
    -- example, there's a `Sum` Monoid which starts at 0 and accumulates values
    -- by adding them together. There's also a `Product` Monoid, a `Max` Monoid,
    -- a `Min` Monoid, a `First` Monoid, a `Last` Monoid, the `[a]` Monoid, and
    -- many more. `OnlyOne` is like a Monoid (Really a Semigroup which means it
    -- doesn't have an initial value) which throws an error if you try to
    -- combine it with anything else.
    --
    -- So, how does this parser work? First, `choice` picks a parser based on
    -- the current character. If it's a '.', we return an empty EngineMap. If
    -- it's a number, we return an EngineMap with a single element in the
    -- partNumbers map. If it's anything else, we return an EngineMap with a
    -- single element in the Component map (remember those functions from
    -- earlier). `many` means run the provided parser until it fails, creating a
    -- list of all the values the parser returned. We provide it our choice parser,
    -- so it creates a list of all the EngineMaps. The foldFrom says that we'd
    -- like to use the Monoid instance of the EngineMap to combine all of our
    -- EngineMaps together. The automatically generted Monoid instance of the
    -- EngineMap will accumulate them by unioning one partNumber Map with the
    -- other and unioning one component Map with the other. If there are any
    -- intersections between two EngineMaps, it uses the Semigroup instance of
    -- the values stored inside the maps. We're telling it to use the OnlyOne
    -- Semigroup instance to combine any intersections, which means we're saying
    -- to throw an error if the parser somehow generated multiple values at the
    -- same spot.
    --
    -- This is a lot of words to say that we're going to parse the entire grid
    -- by combining parsers that can handle individual cells.
    fmap (foldFrom @(EngineMap (OnlyOne Char) (OnlyOne Int))) $
        many $
            choice
                [ symbolic '.' $> mempty
                , fromPartNumber <$> getLoc <*> nat
                , fromComponent <$> getLoc <*> anyChar
                ]

-- So here's what we'll end with: We're going to parse our grid, convert it into
-- an EngineIter, remove all the invalid part numbers (aka the parts not next to
-- a component), then we'll add them up.
partA :: String -> Int
partA = sum . onlyValidParts . mapToIter . runMyParser parseGrid

-- Determine how many cells an integer would have taken up. FromIntegral is
-- boilerplate that casts one number type into another.
numDigits :: Int -> Int
numDigits = floor @Double . (+ 1) . logBase 10 . fromIntegral

-- Find all the locations relative to the leftmost digit of a number that could
-- be neighbors.
neighborsOf :: EngineIter component Int -> [Loc]
neighborsOf engine =
    -- We're using extract to get the size of the focused part.
    let size = numDigits $ extract engine 
     in Loc -1 0 : Loc size 0 : [Loc x y | x <- [-1 .. size], y <- [-1, 1]]

-- Checks if the focused part has a symbol as a neighbor.
hasSymbolNeighbor :: EngineIter component Int -> Bool
hasSymbolNeighbor engine = any (isJust . at engine) $ neighborsOf engine

-- This method iterates over all the part numbers and keeps only the ones which
-- are next to a symbol.
onlyValidParts :: EngineIter component Int -> EngineIter component Int
-- Breaking the following line down a bit:
-- * catMaybes filters out all the empty values from a collection of optional
-- values.
-- * extend (provided by Comonad since we implemented duplicate) iterates over
-- all the indices.
-- * justWhen wraps the input in `Just` when the predicate is true. Otherwise it
-- returns `Nothing`.
-- * fmap extract will grab the focused value from the grid when justWhen
-- returned a Just value.
onlyValidParts = catMaybes . extend (fmap extract . justWhen hasSymbolNeighbor)

-- And that's everything for part A! We were able to handle a grid of values in
-- a reasonable (or as reasonable as Haskell code can be) way by splitting
-- responsibilities. The Comonad instance was responsible for saying how to
-- iterate over the grid, and the methods we just looked at were responsible for
-- saying what to do during an iteration.


-- Part B can be handled in much the same way. First, we'll find all the valid
-- gears, then we'll do the math to combine them.
partB :: String -> Int
partB = sum . fmap product . onlyValidGears . mapToIter . runMyParser parseGrid

-- We want to create a big Map of gears to parts that they touch. Just like
-- before, we're going to start by building a bunch of small maps, then combine
-- them all together at the end. Each gear is represented as a location (the key
-- of our map) and the values store lists of all the part numbers touching the
-- gear.
gearNeighbors :: EngineIter Char Int -> Map Loc [Int]
gearNeighbors engine =
    -- Yeah so this line exists. I should split it up to be more reasonable. It's
    -- got the annoying Haskell quality of requiring you to read parts of it
    -- left to right and parts of it right to left. If you squint and ignore the
    -- type tetris boilerplate, it's vaguely kind of readable:
    -- `engine.lookingAt + (filter ('*' == at engine) neighborsOf engine)`
    -- In other words, it finds the neighbors of the the focused engine index,
    -- removes all the neighboars that aren't '*', then turns the relative
    -- positions of those neighbors into absolute ones.
    let gears = fmap (engine.lookingAt <>) . filter ((Just '*' ==) . at engine) $ neighborsOf engine
    -- Look, another fold! This time we're using foldMap which applies some
    -- transformation to the values in the list before using the Monoid instance
    -- to combine them. We used the OnlyOne Semigroup before. Now we'll use the
    -- List Semigroup which just concatenates them if there are intersections
    -- (which there should be since one gear might touch multiple numbers). As
    -- an aside, `sum` is equivalent to `foldAs @Sum` and `product` is
    -- equivalent to `foldAs @Product`. Folds are really powerful! Just like how
    -- we used Comonad to split how we want to iterate from what we want to do
    -- during each iteration, a Foldable lets us split how we want to accumulate
    -- from what we want to accumulate. This is part of what makes functional
    -- programming cool: we can split control flow details from our business
    -- logic!
     in foldMap (`mapOf` [extract engine]) gears

onlyValidGears :: EngineIter Char Int -> Map Loc [Int]
-- Since our foldMap above accumulates all of the part numbers into a list, we can
-- filter for lists with a length of 2 to find all the valid gears. The fold in
-- the middle combines all of the single element maps spread around the grid
-- into one big Map. The extend does the same thing it did before.
onlyValidGears = filter ((2 ==) . length) . fold . extend gearNeighbors

-- And there we have it! We've solved part 3 using grids and functional
-- programming. Most people associate functional programming with recursion, but
-- we wrote no recursive functions in this file. All of the recursion was
-- provided by generic libraries or generated by the compiler.
--
-- What was it like to debug this code? Actually it was pretty nice! At any
-- point in these pipelines, you can insert `traceShowId` to print out the value
-- at that point in the pipeline. There's also `traceShowIdWith x` which prints
-- some value x alongside whatever was in the pipeline.
